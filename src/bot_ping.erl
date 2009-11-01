-module(bot_ping).
-export([
         start/1,
         start/0,
         loop/1
        ]).

loop(Waves) ->
    receive
        {addWave, Wave} ->
            wave:subscribe(Wave),
            ?MODULE:loop([Wave | Waves]);

        {delWave, Wave} ->
            wave:unsubscribe(Wave),
            ?MODULE:loop(Waves -- [Waves]);

        %% Don't care about typing
        {Wave, type, _} ->
            ?MODULE:loop(Waves);

        %% Handle ping
        {Wave, message, {_Tick, _Ts, _Who, <<"ping">>}} ->
            wave:post(Wave, <<"pong">>),
            ?MODULE:loop(Waves);

        %% Ignore all other messages
        {Wave, message, _} ->
            ?MODULE:loop(Waves);
            
        Any ->
            io:format("Bot: error message: ~p~n", [Any]),
            ?MODULE:loop(Waves)
    end.

start() ->
    spawn(fun() -> loop([]) end).
start(Node) ->
    spawn(Node, bot_ping, loop, [[]]).
