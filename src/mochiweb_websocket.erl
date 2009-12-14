%% 
%% This is a wrapper around the mochiweb_socket_server.  It's based
%% on 'mochiweb_http' but handles the WebSocket protocol.
%% In this implementation there's no timeout set on reading from the socket so the
%% client connection does not timeout.
%% @author Dave Bryson [http://weblog.miceda.org]
%%
-module(mochiweb_websocket).

-export([start/0, start/1, stop/0, stop/1]).
-export([loop/2, default_hello/1]).

-define(DEFAULTS, [{name, ?MODULE},
                   {port, 8002}]).

-define(WEBSOCKET_PREFIX,"HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n").

set_default({Prop, Value}, PropList) ->
    case proplists:is_defined(Prop, PropList) of
        true ->
            PropList;
        false ->
            [{Prop, Value} | PropList]
    end.

set_defaults(Defaults, PropList) ->
    lists:foldl(fun set_default/2, PropList, Defaults).

parse_options(Options) ->
    {loop, MyLoop} = proplists:lookup(loop, Options),
    Loop = fun (S) ->
                   ?MODULE:loop(S, MyLoop)
           end,
    Options1 = [{loop, Loop} | proplists:delete(loop, Options)],
    set_defaults(?DEFAULTS, Options1).

stop() ->
    mochiweb_socket_server:stop(?MODULE).

stop(Name) ->
    mochiweb_socket_server:stop(Name).

start() ->
    start([{ip, "127.0.0.1"},
           {loop, {?MODULE, default_hello}}]).

start(Options) ->
    mochiweb_socket_server:start(parse_options(Options)).

%% Default loop if you start the server with 'start()'
default_hello(WebSocket) ->
    Data = WebSocket:get_data(),
    error_logger:info_msg("Rec from the client: ~p~n",[Data]),
    WebSocket:send("hello from the new WebSocket api").

loop(Socket, MyLoop) ->
    %% Set to http packet here to do handshake
    inet:setopts(Socket, [{packet, http}]),
    handshake(Socket,MyLoop).

handshake(Socket,MyLoop) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, {http_request, _Method, Path, _Version}} ->
	    {abs_path,PathA} = Path,
	    check_header(Socket,PathA,[],MyLoop);
	{error, {http_error, "\r\n"}} ->
            handshake(Socket, MyLoop);
        {error, {http_error, "\n"}} ->
            handshake(Socket, MyLoop);
        _Other ->
            gen_tcp:close(Socket),
            exit(normal)
    end.

check_header(Socket,Path,Headers,MyLoop) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, http_eoh} ->
	    verify_handshake(Socket,Path,Headers),
	    %% Set packet back to raw for the rest of the connection
            inet:setopts(Socket, [{packet, raw}]),
            request(Socket,MyLoop);
        {ok, {http_header, _, Name, _, Value}} ->
            check_header(Socket, Path, [{Name, Value} | Headers],MyLoop);
        _Other ->
            gen_tcp:close(Socket),
            exit(normal)
    end.

verify_handshake(Socket,Path,Headers) ->
    error_logger:info_msg("Incoming Headers: ~p~n",[Headers]),
    
    case proplists:get_value('Upgrade',Headers) of
	"WebSocket" ->
	    send_handshake(Socket,Path,Headers);
	_Other ->
	    error_logger:error_msg("Incorrect WebSocket headers. Closing the connection~n"),
	    gen_tcp:close(Socket),
            exit(normal)
    end.

send_handshake(Socket,Path,Headers) ->
    Origin = proplists:get_value("Origin",Headers),
    Location = proplists:get_value('Host',Headers),
    Resp = ?WEBSOCKET_PREFIX ++
	"WebSocket-Origin: " ++ Origin ++ "\r\n" ++
	"WebSocket-Location: ws://" ++ Location ++ Path ++ "\r\n\r\n",
    gen_tcp:send(Socket, Resp).

request(Socket, MyLoop) ->
    WebSocketRequest = websocket_request:new(Socket),
    MyLoop(WebSocketRequest),
    request(Socket,MyLoop).
