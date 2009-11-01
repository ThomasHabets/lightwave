%% lightwave/src/wave.erl
%%
%% @author author <thomas@habets.pp.se>
%% @copyright 2009 Thomas Habets
%%
%% @doc Wave process code
%%
-module(wave).

-export([
         %% External API
         findWave/1,
         subscribe/1,
         unsubscribe/1,
         post/2,

         %% Internal API only
         loop/4,
         loopStart/0
        ]).

-include("lightwave.hrl").

%%%%%%%%%%%%%%%%%%%%%%%
%% External API
%%%%%%%%%%%%%%%%%%%%%%%

%%
%%
%%
unsubscribe(WavePid) ->
    WavePid ! {self(), unsubscribe},
    receive
        {WavePid, unsubscribed} ->
            ok
    after ?ACK_TIMEOUT ->
            io:format("Pid~p: FIXME: Failed to unsubscribe from ~p~n",
                      [self(),WavePid])
    end.

%%
%%
%%
subscribe(WavePid) ->
    WavePid ! {self(), subscribe},
    receive
        {WavePid, subscribed} ->
            ok
    after ?ACK_TIMEOUT ->
            io:format("Pid~p: FIXME: Failed to subscribe from ~p~n",
                      [self(),WavePid])
    end.

%%
%%
%%
post(Wave, Msg) ->
    Wave ! {self(), post, 'bot_ping', Msg},
    receive
        {Wave, posted} ->
            ok
    after 1000 ->
            io:format("Bot: timout posting :-(~n")
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Init wave
%%
loopStart() ->
    io:format("wave(~p): booting~n", [self()]),
    bot_ping:start() ! {addWave, self()},
    ?MODULE:loop(3,
                 [],
                 [
                  {1, '2009-01-01 00:00:00', lightwave,
                   list_to_binary("Initial message")},
                  {2, '2009-01-01 00:00:01', lightwave,
                   list_to_binary("Initial message2")}
                 ],
                dict:new()).

%% loop(Tick, Users, Data, Keys)
%%
%% Tick: next tick in channel. Tick *may* be wave-specific.
%% Users: Pids of users subscribed
%% Data: ordered list of all data in channel {Tick, Timestamp, Who, Message}
%% Keys: dict of keypress data
%%
loop(Tick, Users, Data, Keys) ->
    %%io:format("wave(~p) loop: clients=~p~n", [self(), length(Users)]),

    receive
        %%
        %% System
        %%
        {system, {_From,_Ref}, {debug, {trace,Trace}}} ->
            io:format("wave(~p): Trace started? ~p~n", [self(), Trace]),
            ?MODULE:loop(Tick, Users, Data, Keys);

        %%
        %% Debug
        %%
        {From, getTyped} ->
            io:format("wave: getTyped~n"),
            From ! Keys,
            ?MODULE:loop(Tick, Users, Data, Keys);


        %%
        %% Interface
        %%

        %% Subscribe without history
        {From, subscribe} ->
            From ! {self(), subscribed},
            ?MODULE:loop(Tick, [From | Users], Data, Keys);

        %% Subscribe with history
        {From, subscribe, TickStart} ->
            From ! {self(), subscribed},
            flushWave(From, TickStart, Data),
            ?MODULE:loop(Tick, [From | Users], Data, Keys);

        %% Unsubscribe
        {From, unsubscribe} ->
            From ! {self(), unsubscribed},
            ?MODULE:loop(Tick, Users -- [From], Data, Keys);

        %% Type
        {From, type, Who, Typed} ->
            From ! {self(), typed},
            case dict:find(Who, Keys) of
                %% Ignore typing if it's just the same data anyway
                {ok, {_, Typed}} ->
                    ?MODULE:loop(Tick, Users, Data, Keys);
                _Any ->
                    lists:foreach(
                      fun(User) ->
                              User ! {self(),
                                      type,
                                      {Tick,
                                       waveutil:nowString(),
                                       Who,
                                       Typed}}
                      end,Users),
                    NewKeys = case Typed of
                                  <<>> ->
                                      dict:erase(Who, Keys);
                                  _ ->
                                      dict:store(Who, {Tick, Typed}, Keys)
                              end,
                    ?MODULE:loop(Tick+1, Users, Data, NewKeys)
            end;
            

        %% Empty Post, ignore
        {From, post, _, <<>>} ->
            From ! {self(), posted},
            ?MODULE:loop(Tick+1, Users, Data, Keys);
            
        %% Post
        {From, post, Who, Message} ->
            From ! {self(), posted},
            New = {Tick, waveutil:nowString(), Who, Message},
            lists:foreach(fun(User) ->
                                  %% broadcast the message
                                  User ! {self(), message, New}
                          end, Users),
            ?MODULE:loop(Tick+1,
                         Users,
                         lists:reverse([New | lists:reverse(Data)]),
                         Keys);
        _ ->
            io:format("wave(~p): unknown message received~n", [self()]),
            ?MODULE:loop(Tick, Users, Data, Keys)
    end.

%%
%% wave finder. Currently only one wave, so just return that Pid
%%
findWave(_WaveName) ->
    Pid = global:whereis_name(thewave),
    if
        is_pid(Pid) -> Pid;
        true ->
            % create it
            NewPid = spawn(fun() -> ?MODULE:loopStart() end),
            global:register_name(thewave, NewPid),
            NewPid
    end.

%%
%% Send the whole history of the wave to the client
%%
%% Client: Process to send the data
%% TickStart: only send blips newer than this
%% Data: full data history of wave
%%
%% FIXME: this does not include typed data
%%
flushWave(Client, TickStart, Data) ->
    lists:foreach(fun(D) ->
                          {Tick, _, _, _} = D,
                          case Tick+1 > TickStart of
                              true ->
                                  Client ! {self(),message,D};
                              _ ->
                                  ok
                          end
                  end, Data).
