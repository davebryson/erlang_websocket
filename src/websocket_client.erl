%% 
%% Basic implementation of the WebSocket API:
%% http://dev.w3.org/html5/websockets/
%% However, it's not completely compliant with the WebSocket spec.
%% Specifically it doesn't handle the case where 'length' is included
%% in the TCP packet, SSL is not supported, and you don't pass a 'ws://type url to it.
%%
%% It also defines a behaviour to implement for client implementations.
%% @author Dave Bryson [http://weblog.miceda.org]
%%
-module(websocket_client).

-behaviour(gen_server).

%% API
-export([start/3,start/4,write/1,close/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Ready States
-define(CONNECTING,0).
-define(OPEN,1).
-define(CLOSED,2).

%% Behaviour definition
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{onmessage,1},{onopen,0},{onclose,0},{close,0},{send,1}];
behaviour_info(_) ->
    undefined.

-record(state, {socket,readystate=undefined,headers=[],callback}).

start(Host,Port,Mod) ->
  start(Host,Port,"/",Mod).
  
start(Host,Port,Path,Mod) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{Host,Port,Path,Mod}], []).

init(Args) ->
    process_flag(trap_exit,true),
    [{Host,Port,Path,Mod}] = Args,
    {ok, Sock} = gen_tcp:connect(Host,Port,[binary,{packet, 0},{active,true}]),
    
    Req = initial_request(Host,Path),
    ok = gen_tcp:send(Sock,Req),
    inet:setopts(Sock, [{packet, http}]),
    
    {ok,#state{socket=Sock,callback=Mod}}.

%% Write to the server
write(Data) ->
    gen_server:cast(?MODULE,{send,Data}).

%% Close the socket
close() ->
    gen_server:cast(?MODULE,close).

handle_cast({send,Data}, State) ->
    gen_tcp:send(State#state.socket,[0] ++ Data ++ [255]),
    {noreply, State};

handle_cast(close,State) ->
    Mod = State#state.callback,
    Mod:onclose(),
    gen_tcp:close(State#state.socket),
    State1 = State#state{readystate=?CLOSED},
    {stop,normal,State1}.

%% Start handshake
handle_info({http,Socket,{http_response,{1,1},101,"Web Socket Protocol Handshake"}}, State) ->
    State1 = State#state{readystate=?CONNECTING,socket=Socket},
    {noreply, State1};

%% Extract the headers
handle_info({http,Socket,{http_header, _, Name, _, Value}},State) ->
    case State#state.readystate of
	?CONNECTING ->
	    H = [{Name,Value} | State#state.headers],
	    State1 = State#state{headers=H,socket=Socket},
	    {noreply,State1};
	undefined ->
	    %% Bad state should have received response first
	    {stop,error,State}
    end;

%% Once we have all the headers check for the 'Upgrade' flag 
handle_info({http,Socket,http_eoh},State) ->
    %% Validate headers, set state, change packet type back to raw
     case State#state.readystate of
	?CONNECTING ->
	     Headers = State#state.headers,
	     case proplists:get_value('Upgrade',Headers) of
		 "WebSocket" ->
		     inet:setopts(Socket, [{packet, raw}]),
		     State1 = State#state{readystate=?OPEN,socket=Socket},
		     Mod = State#state.callback,
		     Mod:onopen(),
		     {noreply,State1};
		 _Any  ->
		     {stop,error,State}
	     end;
	undefined ->
	    %% Bad state should have received response first
	    {stop,error,State}
    end;

%% Handshake complete, handle packets
handle_info({tcp, _Socket, Data},State) ->
    case State#state.readystate of
	?OPEN ->
	    D = unframe(binary_to_list(Data)),
	    Mod = State#state.callback,
	    Mod:onmessage(D),
	    {noreply,State};
	_Any ->
	    {stop,error,State}
    end;

handle_info({tcp_closed, _Socket},State) ->
    Mod = State#state.callback,
    Mod:onclose(),
    {stop,normal,State};

handle_info({tcp_error, _Socket, _Reason},State) ->
    {stop,tcp_error,State};

handle_info({'EXIT', _Pid, _Reason},State) ->
    {noreply,State}.

handle_call(_Request,_From,State) ->
    {reply,ok,State}.

terminate(Reason, _State) ->
    error_logger:info_msg("Terminated ~p~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_request(Host,Path) ->
    "GET "++ Path ++" HTTP/1.1\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n" ++ 
	"Host: " ++ Host ++ "\r\n" ++
	"Origin: http://" ++ Host ++ "/\r\n\r\n".


unframe([0|T]) -> unframe1(T).
unframe1([255]) -> [];
unframe1([H|T]) -> [H|unframe1(T)].

    
