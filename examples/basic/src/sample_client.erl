-module(sample_client).

-behaviour(websocket).

-export([start/0]).

%% websocket specific callbacks
-export([onmessage/1,onopen/0,onclose/0,close/0,send/1]).


send(Data) ->
    websocket_client:write(Data).

start() ->
    websocket_client:start("localhost",8002,?MODULE).

onmessage(Data) ->
    io:format("Got some data:: ~p~n",[Data]).

onclose() ->
    io:format("Connection closed~n").

onopen() ->
    io:format("Connection open~n"),
    send("client-connected").

close() ->
    websocket_client:close().
