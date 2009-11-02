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
         loop/2
        ]).

-include("lightwave.hrl").

%% External API

%%
%% start 
%%
start(Options) ->
    io:format("Start options: ~p~n", [Options]),
    wavefinder:start(),
    {DocRoot, Options1} = getOption(docroot, Options),
    Handler = fun (Req) ->
                      ?MODULE:loop(Req, DocRoot)
              end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Handler} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

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
%% Get all messages from FromTick and on, or until the timeout
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Set timeout timer and default return value
%%
%% FromTick: First tick that the client is interested in
%%
getMessages(FromTick) ->
    {ok, TRef} = timer:send_after(?GET_TIMEOUT, done),
    R = getMessages(FromTick, [{error, timeout, 1, waveutil:nowString(),
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
        {_Wave, Type, {Tick, Timestamp, Who, Message}} ->
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
%% FIXME: parse out wave name
%%
handleGET(Req, DocRoot) ->
    io:format("GET: ~p~n", [Req:get(path)]),
    [Wave|Tail] = string:tokens(Req:get(path), "/"),
    case Tail of
        ["get", FromTimeS] ->
            FromTime = list_to_integer(FromTimeS),
            WavePid = wave:findWave(Wave),
            WavePid ! {self(), subscribe, FromTime},
            receive
                {WavePid, subscribed} ->
                    Msgs = getMessages(FromTime),
                    Rep = constructReply(Msgs),
                    wave:unsubscribe(WavePid),
                    Req:ok({"text/javascript", mochijson2:encode(Rep)})
            after ?ACK_TIMEOUT ->
                    io:format("handleGET(~p): FIXME: sub timeout to ~p~n",
                              [self(), WavePid]),
                    wave:unsubscribe(WavePid),
                    Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                     {"status", error},
                                     {"message",
                                      <<"Failed to subscribe to wave">>}
                                    ]})
                           })
            end;
        ["static", _] ->
            Req:serve_file(Req:get(path), DocRoot);
        [] ->
            Req:serve_file("", DocRoot)
    end.

%%
%%
%%
handlePOST(Req) ->
    [Wave|Tail] = string:tokens(Req:get(path), "/"),
    case Tail of
        %% Client is typing
        ["type"] ->
            %%io:format("POST type~n"),
            Data = Req:parse_post(),
            PostWho = list_to_binary(proplists:get_value("who", Data)),
            PostKeys = list_to_binary(proplists:get_value("keys", Data)),
            
            WavePid = wave:findWave(Wave),
            %% post
            WavePid ! {self(),
                    type,
                    PostWho,
                    PostKeys
                    },
            
            receive
                {WavePid,typed} ->
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
        ["chat"] ->
            %%io:format("POST chat~n"),
            Data = Req:parse_post(),
            PostMessage = list_to_binary(proplists:get_value("message", Data)),
            PostWho = list_to_binary(proplists:get_value("who", Data)),
            WavePid = wave:findWave(Wave),
            %% post
            WavePid ! {self(),
                    post,
                    PostWho,
                    PostMessage
                    },
            receive
                {WavePid,posted} ->
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
