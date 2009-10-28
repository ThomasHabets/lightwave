%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the lightwave application.

-module(lightwave_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for lightwave.
start(_Type, _StartArgs) ->
    lightwave_deps:ensure(),
    lightwave_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for lightwave.
stop(_State) ->
    ok.
