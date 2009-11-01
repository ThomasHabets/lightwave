%% lightwave/src/bot_wave.erl
%%
%% @author author <thomas@habets.pp.se>
%% @copyright 2009 Thomas Habets
%%
%% @doc Wave info bot
%%
-module(bot_wave).
-export([
         %% External API
         start/1,
         start/0,

         %% Internal API
         loop/1
        ]).

start() ->
    spawn(fun() -> loop([]) end).
start(Node) ->
    spawn(Node, bot_ping, loop, [[]]).

loop(Waves) ->
    receive
        {addWave, Wave} ->
            wave:subscribe(Wave),
            ?MODULE:loop([Wave | Waves]);

        {delWave, Wave} ->
            wave:unsubscribe(Wave),
            ?MODULE:loop(Waves -- [Waves]);

        %% Don't care about typing
        {_Wave, type, _} ->
            ?MODULE:loop(Waves);

        %% Handle ping
        {Wave, message, {_Tick, _Ts, _Who, <<"/names">>}} ->
            wave:post(Wave, 'bot_wave',
                      list_to_binary(string:concat("names: ",
                                                   waveutil:stringJoin(
                                                     wave:names(Wave),
                                                     ", ")))),
            ?MODULE:loop(Waves);

        %% Ignore all other messages
        {_Wave, message, _} ->
            ?MODULE:loop(Waves);
            
        Any ->
            io:format("Bot: error message: ~p~n", [Any]),
            ?MODULE:loop(Waves)
    end.
