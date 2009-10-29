%% lightwave/src/lightwave_web.erl
%%
%% @author author <thomas@habets.pp.se>
%% @copyright 2009 Thomas Habets
%%
%% @doc Web server for lightwave.
%%
-module(lightwave_web).
-author('Thomas Habets <thomas@habets.pp.se>').

-export([start/1,
         stop/0,
         loop/2,
         wave/0,
         wave/4]).

%% timeout for internal messages that should just be acked
-define(ACK_TIMEOUT, 100).

%% max time to wait a poll before returning a timeout
-define(GET_TIMEOUT, 120000).
%%-define(GET_TIMEOUT, 5000).

%% External API

%%
%% start 
%%
start(Options) ->
    {DocRoot, Options1} = getOption(docroot, Options),
    Handler = fun (Req) ->
                      ?MODULE:loop(Req, DocRoot)
              end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Handler} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

%%
%% Send the whole history of the wave to the client
%%
%% Client: Process to send the data
%% Tickstart: only send blips newer than this
%% Data: full data history of wave
%%
waveFlush(Client, TickStart, Data) ->
    %%io:format("Flushing from ~p~n", [TimeStart]),
    lists:foreach(fun(D) ->
                          %%io:format("Iter: ~p~n", [D]),
                          {Tick, _, _, _} = D,
                          %%io:format("Iter: ~p ~p ~p~n", [N, Who, Line]),
                          case Tick+1 > TickStart of
                              true ->
                                  Msg = {message, D},
                                  %%io:format("send: ~p~n", [Msg]),
                                  Client ! Msg;
                              _ ->
                                  ok
                          end
                  end, Data).


%%
%% Init wave
%%
wave() ->
    io:format("wave(~p): booting~n", [self()]),
    ?MODULE:wave(3,
                 [],
                 [
                  {1, '2009-01-01 00:00:00', lightwave,
                   list_to_binary("Initial message")},
                  {2, '2009-01-01 00:00:01', lightwave,
                   list_to_binary("Initial message2")}
                 ],
                dict:new()).

%%
%% Tick: next tick in channel. Tick *may* be wave-specific.
%% Users: Pids of users subscribed
%% Data: ordered list of all data in channel {Tick, Timestamp, Who, Message}
%%
wave(Tick, Users, Data, Keys) ->
    %%io:format("wave(~p) loop: clients=~p~n", [self(), length(Users)]),

    receive
        %%
        %% System
        %%
        {system, {_From,_Ref}, {debug, {trace,Trace}}} ->
            io:format("wave(~p): Trace started? ~p~n", [self(), Trace]);

        %%
        %% Debug
        %%
        {From, getTyped} ->
            %%io:format("wave: getTyped~n"),
            From ! Keys,
            ?MODULE:wave(Tick, Users, Data, Keys);


        %%
        %% Interface
        %%

        %% Subscribe
        {From, subscribe, TickStart} ->
            From ! subscribed,
            waveFlush(From, TickStart, Data),
            ?MODULE:wave(Tick, [From | Users], Data, Keys);

        %% Unsubscribe
        {From, unsubscribe} ->
            From ! unsubscribed,
            ?MODULE:wave(Tick, Users -- [From], Data, Keys);

        %% Type
        {From, type, Who, Typed} ->
            From ! typed,
            lists:foreach(fun(User) ->
                                  User ! {type,
                                          {Tick, nowString(), Who, Typed}}
                          end, Users),
            ?MODULE:wave(Tick+1, Users, Data,
                         dict:store(Who, {Tick,Typed}, Keys));

        %% Empty Post
        {From, post, _, <<>>} ->
            From ! posted,
            ?MODULE:wave(Tick+1, Users, Data, Keys);
            
        %% Post
        {From, post, Who, Message} ->
            From ! posted,
            New = {Tick, nowString(), Who, Message},
            lists:foreach(fun(User) ->
                                  %% broadcast the message
                                  User ! {message, New}
                          end, Users),
            ?MODULE:wave(Tick+1,
                         Users,
                         lists:reverse([New | lists:reverse(Data)]),
                         Keys);
        _ ->
            io:format("wave(~p): unknown message received~n", [self()]),
            ?MODULE:wave(Tick, Users, Data, Keys)
    end.

%%
%% wave finder. Currently only one wave, so just return that Pid
%%
findWave(_WaveName) ->
    Pid = whereis(thewave),
    if
        is_pid(Pid) -> Pid;
        true ->
            % create it
            NewPid = spawn(fun() -> ?MODULE:wave() end),
            register(thewave, NewPid),
            NewPid
    end.

%%
%% Return true if the Data list is just a timeout error, else false
%%
onlyError(L) ->
    case length(L) of
        1 ->
            [{S,T,_,_,_,_}|_] = L,
            case {S,T} of
                {error, timeout} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getMessages(FromTick)
%% Get all messages from FromTick and on, ut until the timeout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Set timeout timer and default return value
%%
%% FromTick: First tick that the client is interested in
%%
getMessages(FromTick) ->
    {ok, TRef} = timer:send_after(?GET_TIMEOUT, done),
    R = getMessages(FromTick, [{error, timeout, 1, nowString(),
                                lightwave, <<"timeout">>}]),
    {ok, cancel} = timer:cancel(TRef),
    receive
        done ->
            ok
    after 10 ->  %% epsilon. Either it's in the queue or it never will be (?)
            ok
    end,
    lists:reverse(R).

%%
%% FromTick: First tick the user is interested in
%%
getMessages(FromTick, Data) ->
    %%io:format("Waiting for message ~p ~p~n", [FromTime, self()]),
    receive
        done ->
            Data;
        {Type, {Tick, Timestamp, Who, Message}} ->
            case Tick < FromTick of
                true ->
                    %%io:format("Ignored ~p~n", [MsgTime]),
                    getMessages(FromTick, Data);
                _ ->
                    %%io:format("Added ~p to ~p~n", [Message, Data]),
                    New = {ok, Type, Tick, Timestamp, Who, Message},
                    timer:send_after(10, idone),
                    case onlyError(Data) of
                        true ->
                            getMessages(Tick+1, [New]);
                        false ->
                            getMessages(Tick+1, [New | Data])
                    end
            end;
        idone ->
            %% check for false alarm (lingering idone from previous request)
            case onlyError(Data) of
                true ->
                    getMessages(FromTick, Data);
                false ->
                    Data
            end;
        Any ->
            io:format("invalid message sent to getMessage(): ~p~n", [Any]),
            Data
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% contructReply()
%%
%% Take the output of getMessages() and create a structure suitable for 
%% json encoding.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Entry
%%
constructReply(Messages) ->
    constructReply(Messages, []).

%%
%% All done, reverse it.
%%
constructReply([], Ret) ->
    lists:reverse(Ret);

%%
%% Create jsonable entry
%%
constructReply(Messages, Ret) ->
    [H|T] = Messages,
    {Status, Type, Tick, Timestamp, Who, Message} = H,
    Cur = {struct, [
                    {status, Status},
                    {type, Type},
                    {who, Who},
                    {message, Message},
                    {timestamp, Timestamp},
                    {tick, Tick}
                   ]},
    case Status of
        %% on error, just give up. We don't want to return both error and
        %% data in same request
        error ->
            constructReply([], [Cur]);
        _ ->
            constructReply(T, [Cur | Ret])
    end.

%%
%%
%%
unsubscribe(WavePid) ->
    WavePid ! {self(), unsubscribe},
    receive
        unsubscribed ->
            ok
    after ?ACK_TIMEOUT ->
            io:format("Pid~p: FIXME: Failed to unsubscribe from ~p~n",
                      [self(),WavePid])
    end.


%%
%% FIXME: parse out wave name
%%
handleGET(Req, DocRoot) ->
    "/foo/" ++ Path = Req:get(path),
    Wave = "foo",
    case Path of
        "get/" ++ FromTimeS ->
            FromTime = list_to_integer(FromTimeS),
            WavePid = findWave(Wave),
            WavePid ! {self(), subscribe, FromTime},
            receive
                subscribed ->
                    Msgs = getMessages(FromTime),
                    Rep = constructReply(Msgs),
                    unsubscribe(WavePid),
                    Req:ok({"text/javascript", mochijson2:encode(Rep)})
            after ?ACK_TIMEOUT ->
                    io:format("handleGET(~p): FIXME: sub timeout to ~p~n",
                              [self(), WavePid]),
                    unsubscribe(WavePid),
                    Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                     {"status", error},
                                     {"message",
                                      <<"Failed to subscribe to wave">>}
                                    ]})
                           })
            end;
        _ ->
            Req:serve_file(Path, DocRoot)
    end.

%%
%%
%%
handlePOST(Req) ->
    "/foo/" ++ Path = Req:get(path),
    Wave = "foo",
    case Path of
        %% Client is typing
        "type" ->
            %%io:format("POST type~n"),
            Data = Req:parse_post(),
            PostWho = list_to_binary(proplists:get_value("who", Data)),
            PostKeys = list_to_binary(proplists:get_value("keys", Data)),
            
            WavePid = findWave(Wave),
            %% post
            WavePid ! {self(),
                    type,
                    PostWho,
                    PostKeys
                    },
            
            receive
                typed ->
                    {Status, Message} = {ok, <<"typed">>}
            after ?ACK_TIMEOUT ->
                    {Status, Message} = {error, <<"timeout">>}
            end,
            Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                     {status, Status},
                                     {message, Message}
                                    ]
                        })
                   });

        %% Client submitted a line
        "chat" ->
            %%io:format("POST chat~n"),
            Data = Req:parse_post(),
            PostMessage = list_to_binary(proplists:get_value("message", Data)),
            PostWho = list_to_binary(proplists:get_value("who", Data)),
            Wave = "foo",
            WavePid = findWave(Wave),
            %% post
            WavePid ! {self(),
                    post,
                    PostWho,
                    PostMessage
                    },
            receive
                posted ->
                    %% posted
                    {Status, Message} = {ok, <<"posted">>}
            after ?ACK_TIMEOUT ->
                    %% something went wrong
                    {Status, Message} = {error, <<"timeout">>}
            end,
            
            %% send back the JSON message
            Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                     {status, Status},
                                     {message, Message}
                                    ]
                        })
                   });
        _ ->
            Req:not_found()
    end.

%%
%% entry point for new requests
%%
loop(Req, DocRoot) ->
    case Req:get(method) of

        %%
        %% Get new data
        %%
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            handleGET(Req, DocRoot);
        'POST' ->
            handlePOST(Req);
        _ ->
            Req:respond({501, [], []})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper functions
%%

%%
%%
%%
getOption(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


%%
%% FIXME: force fixed-width
%%
nowString() ->
    {{Year,Month,Day},{Hour,Min,Seconds}} = erlang:universaltime(),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                 [Year,Month,Day,Hour,Min,Seconds])).
