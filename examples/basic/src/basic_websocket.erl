%%
%% Simple Echo example
%% @author Dave Bryson [http://weblog.miceda.org]
%%
-module(basic_websocket).

-export([start/1, stop/0, loop/1]).

start(Options) ->
    Loop = fun (WebSocket) ->
                   ?MODULE:loop(WebSocket)
           end,
    mochiweb_websocket:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_websocket:stop(?MODULE).


loop(WebSocket) ->
    %% Get the data sent from the client
    Data = WebSocket:get_data(),
    
    %% Our example...
    case Data of
	%% On initial connect we get this message
	"client-connected" ->
	    WebSocket:send("You are connected!");
	%% Other messages go here
	Other ->
	    Msg = "You Said: " ++ Other,
	    WebSocket:send(Msg)
    end.
    
