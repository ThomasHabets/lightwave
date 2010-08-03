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
         post/3,
         names/1,
         save/2,
         load/2,

         %% Internal API only
         loop/2,
         loopStart/1,

	 getData/1,
	 putData/1
        ]).

-include("lightwave.hrl").

%%%%%%%%%%%%%%%%%%%%%%%
%% External API
%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @spec unsubscribe(WavePid) -> ok
%% @doc Unsubscribe from wave
%%
unsubscribe(WavePid) ->
    WavePid ! {self(), unsubscribe},
    receive
        {WavePid, unsubscribed} ->
            ok
    after ?ACK_TIMEOUT ->
            io:format("Pid~p: FIXME: Failed to unsubscribe from ~p~n",
                      [self(),WavePid]),
            error
    end.
%%
%% @spec save(WavePid, Filename) -> ok
%% @doc Save wave data
%%
save(WavePid, Filename) ->
    WavePid ! {self(), save, Filename},
    receive
        {WavePid, saved} ->
            ok
    after ?ACK_TIMEOUT ->
            io:format("Pid~p: FIXME: Failed to save wave ~p~n",
                      [self(),WavePid]),
            error
    end.
%%
%% @spec load(WavePid, Filename) -> ok
%% @doc Load wave data
%%
load(WavePid, Filename) ->
    WavePid ! {self(), load, Filename},
    receive
        {WavePid, loaded} ->
            ok
    after ?ACK_TIMEOUT ->
            io:format("Pid~p: FIXME: Failed to load to wave ~p~n",
                      [self(),WavePid]),
            error
    end.

%%
%% spec names(WavePid) -> List of names in wave
%% @doc Get a list of all names in the wave
%%
names(WavePid) ->
    WavePid ! {self(), names},
    receive
        {WavePid, names, Data} ->
            Data
    after ?ACK_TIMEOUT ->
            io:format("Pid~p: FIXME: Failed to get names from ~p~n",
                      [self(),WavePid]),
            ["Error: unable to get list of members"]
    end.

%%
%% @spec subscribe(WavePid) -> ok
%% @doc Subscribe to wave messages
%%
subscribe(WavePid) ->
    WavePid ! {self(), subscribe},
    receive
        {WavePid, subscribed} ->
            ok
    after ?ACK_TIMEOUT ->
            io:format("Pid~p: FIXME: Failed to subscribe from ~p~n",
                      [self(),WavePid]),
            error
    end.

%%
%% @spec post(Wave,Who,Msg) -> ok
%% @doc Post message to wave
%%
post(Wave, Who, Msg) ->
    Wave ! {self(), post, Who, Msg},
    receive
        {Wave, posted} ->
            ok
    after 1000 ->
            io:format("Bot: timout posting :-(~n"),
            error
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(waveData, {wavename, tick, users, data, keys}).
%% rd(waveData, {tick, users, data, keys}).

putData(Data) ->
    mnesia:transaction(fun() ->
			       mnesia:write(Data)
		       end).


getData(WaveName) ->
    Res = mnesia:transaction(fun() ->
				     mnesia:read(waveData, WaveName, write)
			     end),
    case Res of
	{aborted, {no_exists, waveData}} ->
	    getData(WaveName);
	{atomic, []} ->
	    {ok};
	{atomic, Res2} ->
	    [Top|_] = Res2,
	    {ok, Top}
    end.

%% @spec loopStart(WaveName) -> ok
%% @doc Init wave
%%
loopStart(WaveName) ->
    %%io:format("wave(~p): booting~n", [self()]),
    bot_ping:start() ! {addWave, self()},
    bot_wave:start() ! {addWave, self()},
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(waveData,
			[{disc_copies, [node()]}, {attributes,
						   record_info(fields,
							       waveData)}]),
    Initial = #waveData{wavename=WaveName,
			tick=3,
			users=[],
			data=[
			      {1, '2009-01-01 00:00:00', lightwave,
			       list_to_binary("Initial message")},
			      {2, '2009-01-01 00:00:01', lightwave,
			       list_to_binary("Initial message2")}
			     ],
			keys=dict:new()},
    DBData=getData(WaveName),
    Data = case DBData of
	       {ok, D} ->
		   D;
	       Any ->
		   io:format("WHAT ~p~n", [Any]),
		   Initial
	   end,
    ?MODULE:loop(WaveName, Data).


%% @spec loop(WaveName, WaveData) -> ok
%% @doc main loop of wave
%%
%% Tick: next tick in channel. Tick *may* be wave-specific.
%% Users: Pids of users subscribed
%% Data: ordered list of all data in channel {Tick, Timestamp, Who, Message}
%% Keys: dict of keypress data
%%
loop(WaveName, WaveData) ->
    %%io:format("wave(~p) loop: data=~p~n", [WaveName,
%%					      WaveData]),
    %%io:format("mnesia for ~p: ~p~n", [WaveName, WaveData2]),
    Data=WaveData#waveData.data,
    Tick=WaveData#waveData.tick,
    Users=WaveData#waveData.users,
    Keys=WaveData#waveData.keys,

    putData(WaveData#waveData{users=[]}),
%%io:format("Write op: ~p~n", [putData(WaveName, WaveData)]),
    %%io:format("readback: ~p~n", [getData(WaveName)]),

    receive
        %%
        %% System
        %%
        {system, {_From,_Ref}, {debug, {trace,Trace}}} ->
            io:format("wave(~p): Trace started? ~p~n", [self(), Trace]),
            ?MODULE:loop(WaveName, WaveData);

        %%
        %% Debug
        %%
        {From, getTyped} ->
            io:format("wave: getTyped~n"),
            From ! Keys,
            ?MODULE:loop(WaveName, WaveData);


        %%
        %% Interface
        %%

        %% List names
        {From, names} ->
            From ! {self(), names, ["FIXME", "FIXME2"]},
            ?MODULE:loop(WaveName, WaveData);

        %% Save data to file
        {From, save, Filename} ->
            saveData(Data, Filename),
            From ! {self(), saved},
            ?MODULE:loop(WaveName, WaveData);

        %% Load data from file
        %% FIXME: force a client reload of page somehow
        {From, load, Filename} ->
            Data2 = WaveData#waveData{data=loadData(Filename, Tick),
				      tick=Tick+1},
            From ! {self(), loaded},
            ?MODULE:loop(WaveName, Data2);

        %% Subscribe without history
        {From, subscribe} ->
            From ! {self(), subscribed},
	    ?MODULE:loop(WaveName,
			 WaveData#waveData{users=[From | Users]});

        %% Subscribe with history
        {From, subscribe, TickStart} ->
            From ! {self(), subscribed},
            flushWave(From, TickStart, Data),
            ?MODULE:loop(WaveName,
			 WaveData#waveData{users=[From | Users]});

        %% Unsubscribe
        {From, unsubscribe} ->
            From ! {self(), unsubscribed},
            ?MODULE:loop(WaveName,
			 WaveData#waveData{users=Users -- [From]});

        %% Type
        {From, type, Who, Typed} ->
            From ! {self(), typed},
            case dict:find(Who, Keys) of
                %% Ignore typing if it's just the same data anyway
                {ok, {_, Typed}} ->
                    ?MODULE:loop(WaveName, WaveData);
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
                    ?MODULE:loop(WaveName,
				 WaveData#waveData{tick=Tick+1,
						   keys=NewKeys})
            end;
            

        %% Empty Post, ignore
        {From, post, _, <<>>} ->
            From ! {self(), posted},
            ?MODULE:loop(WaveName,
			 WaveData#waveData{tick=Tick+1});

        %% Post
        {From, post, Who, Message} ->
            From ! {self(), posted},
            New = {Tick, waveutil:nowString(), Who, Message},
            lists:foreach(fun(User) ->
                                  %% broadcast the message
                                  User ! {self(), message, New}
                          end, Users),
            ?MODULE:loop(WaveName,
			 WaveData#waveData{tick=Tick+1,
					   data=lists:reverse([New | lists:reverse(Data)])});
        _ ->
            io:format("wave(~p): unknown message received~n", [self()]),
            ?MODULE:loop(WaveName, WaveData)
    end.



%%
%% @spec findWave(WaveName) -> WavePid
%% @doc wave finder. Currently only one wave, so just return that Pid
%%
findWave(WaveName) ->
    {ok, Pid} = wavefinder:get(WaveName),
    if
        is_pid(Pid) -> Pid;
        true ->
            % create it
            NewPid = spawn(fun() -> ?MODULE:loopStart(WaveName) end),
            wavefinder:register(WaveName, NewPid),
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


data2diskdata(Data) ->
    lists:map(fun(E) ->
                      {_Tick, Ts, Who, Msg} = E,
                      R={Ts, Who, Msg},
                      R
              end, Data).

diskdata2data(Data, Tick) ->
    diskdata2data(Data, Tick, []).
diskdata2data([], _Tick, Ret) ->
    lists:reverse(Ret);
diskdata2data(Data, Tick, Ret) ->
    [E|T] = Data,
    {Ts, Who, Msg} = E,
    diskdata2data(T, Tick+1, [{Tick, Ts, Who, Msg}|Ret]).

%% @spec saveData(Data, Filename) -> ok
saveData(Data, Filename) ->
    {ok,F} = file:open(Filename,[write]),
    io:fwrite(F, "~p.~n", [data2diskdata(Data)]),
    file:close(F),
    ok.


%% @spec loadData(Filename, Tick) -> ok
loadData(Filename, Tick) ->
    {ok, F} = file:open(Filename,[read]),
    {ok, Diskdata} = io:read(F, ''),
    file:close(F),
    diskdata2data(Diskdata, Tick).
