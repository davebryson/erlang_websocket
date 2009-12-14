%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(basic).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the basic server.
start() ->
    basic_deps:ensure(),
    ensure_started(crypto),
    application:start(basic).

%% @spec stop() -> ok
%% @doc Stop the basic server.
stop() ->
    Res = application:stop(basic),
    application:stop(crypto),
    Res.
