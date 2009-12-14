%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the basic application.

-module(basic_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for basic.
start(_Type, _StartArgs) ->
    basic_deps:ensure(),
    basic_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for basic.
stop(_State) ->
    ok.
