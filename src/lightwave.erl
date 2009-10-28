%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(lightwave).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the lightwave server.
start() ->
    lightwave_deps:ensure(),
    ensure_started(crypto),
    application:start(lightwave).

%% @spec stop() -> ok
%% @doc Stop the lightwave server.
stop() ->
    Res = application:stop(lightwave),
    application:stop(crypto),
    Res.
