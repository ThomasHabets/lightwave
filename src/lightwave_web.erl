%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for lightwave.

-module(lightwave_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2,
         room/0, room/4]).

% timeout for internal messages that should just be acked
-define(ACK_TIMEOUT, 100).

-define(GET_TIMEOUT, 30000).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

roomFlush(From, TimeStart, Data) ->
    io:format("Flushing from ~p~n", [TimeStart]),
    lists:foreach(fun(D) ->
                          io:format("Iter: ~p~n", [D]),
                          {N, Who, Line} = D,
                          io:format("Iter: ~p ~p ~p~n", [N, Who, Line]),
                          case N+1 > TimeStart of
                              true ->
                                  Msg = {message, D},
                                  io:format("send: ~p~n", [Msg]),
                                  From ! Msg;
                              Any ->
                                  io:format("don't send: ~p~n", [Any]),
                                  ok
                          end
                  end, Data).

atoi(A) ->
    {R,_} = string:to_integer(A),
    R.
itoa(I) ->
    lists:flatten(io_lib:format("~p", [I])).

room() ->
    io:format("room: booting~n"),
    ?MODULE:room(3,
                 [],
                 [
                  {1, lightwave, list_to_binary("Initial message")},
                  {2, lightwave, list_to_binary("Initial message2")}
                 ],
                []).
%%
%% Time: next timestamp in channel
%% Users: Pids of users subscribed
%% Data: ordered list of all data in channel {Time, Who, Message}
%%
room(Time, Users, Data, Keys) ->
    io:format("room: looping~n"),
    receive
        {From, subscribe, TimeStart} ->
            io:format("room: subscribe ~p ~p~n", [From, TimeStart]),
            From ! subscribed,
            roomFlush(From, TimeStart, Data),
            ?MODULE:room(Time, [From | Users], Data, Keys);
        {From, unsubscribe} ->
            io:format("room: unsubscribe~n"),
            From ! unsubscribed,
            ?MODULE:room(Time, Users -- [From], Data, Keys);
        {From, type, Who, Typed} ->
            ?MODULE:room(Time, Users, Data, [Typed | Keys]);
        {From, post, Who, Message} ->
            io:format("room: post~n"),
            From ! posted,
            lists:foreach(fun(User) ->
                    % broadcast the message
                    User ! {message, {Time, Who, Message}}
                end, Users),
            ?MODULE:room(Time+1, Users,
                         lists:reverse([{Time,Who,Message}
                                        | lists:reverse(Data)]), Keys);
        _ ->
            io:format("room: unknown message received~n"),
            ?MODULE:room(Time, Users, Data, Keys)
    end.

get_the_room(_RoomName) ->
    % does the room exists?
    Pid = whereis(theroom),
    if
        is_pid(Pid) ->
            % yup
            Pid;
        true ->
            % create it
            NewPid = spawn(fun() ->
                ?MODULE:room()
            end),
            register(theroom, NewPid),
            NewPid
    end.

getMessages(FromTime) ->
    getMessages(FromTime, {error, error, lightwave, <<"timeout">>, 0}).
getMessages(FromTime, TimeoutError) ->
    io:format("Waiting for message ~p ~p~n", [FromTime, self()]),
    receive
        {Type, {MsgTime, Who, Message}} ->
            case MsgTime < FromTime of
                true ->
                    io:format("Ignored ~p~n", [MsgTime]),
                    getMessages(FromTime);
                _ ->
                    [{ok, Type, Who, Message, MsgTime}
                     | getMessages(FromTime, [])]
            end;
        Any ->
            io:format("invalid message sent to getMessage(): ~p~n", [Any])
    after 100 ->
            TimeoutError
    end.

constructReply(Req, Messages) ->
    constructReply(Req, Messages, []).
constructReply(Req, [], Ret) ->
    Ret2 = lists:reverse(Ret),
    io:format("Encoding: ~p~n", [Ret2]),
    Req:ok({"text/javascript", mochijson2:encode(Ret2)});

constructReply(Req, Messages, Ret) ->
    [H|T] = Messages,
    {Status, Type, Who, Message, MsgTime} = H,
    Cur = {struct, [
                    {status, Status},
                    {type, Type},
                    {who, Who},
                    {message, Message},
                    {time, MsgTime}
                   ]},
    case Status of
        error ->
            constructReply(Req, [], Cur);
        _ ->
            constructReply(Req, T, [Cur | Ret])
    end.

handleGET(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Path of
        "chat/foo/" ++ FromTimeS ->
            FromTime = atoi(FromTimeS),
            Room = "foo",
            RoomPid = get_the_room(Room),
            RoomPid ! {self(), subscribe, FromTime},
            receive
                subscribed ->
                    Msgs = getMessages(FromTime),
                    io:format("webloop: messages: ~p ~p~n", [Req, Msgs]),
                    constructReply(Req, Msgs)
            after ?ACK_TIMEOUT ->
                    io:format("webloop: can't sub?~n"),
                    RoomPid ! {self(), unsubscribe},
                    receive
                        unsubscribed ->
                            ok
                    after ?ACK_TIMEOUT ->
                            %% FIXME: log unsubscribe timeout
                            ok
                    end,
                    Req:ok({"text/javascript", mochijson2:encode(
                                                 {
                            struct, [
                                     {"status", error},
                                     {"message", "blaha"}
                                    ]})
                           })
            end;
        _ ->
            Req:serve_file(Path, DocRoot)
    end.

handlePOST(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Path of
        "type/" ++ Room ->
            io:format("POST type~n"),
            Data = Req:parse_post(),
            PostWho = list_to_binary(proplists:get_value("who", Data)),
            PostKeys = list_to_binary(proplists:get_value("keys", Data)),
            Room = "foo",
            
            RoomPid = get_the_room(Room),
            %% post
            RoomPid ! {self(),
                    type,
                    PostWho,
                    PostKeys
                    },
            
            receive
                typed ->
                    {Status, Message} = {ok, <<"typed">>}
            after ?ACK_TIMEOUT ->
                    {Status, Message} = {error, <<"timeout">>}
            end;
            
        "chat" ->
            io:format("POST chat~n"),
            Data = Req:parse_post(),
            PostMessage = list_to_binary(proplists:get_value("message", Data)),
            PostWho = list_to_binary(proplists:get_value("who", Data)),
            Room = "foo",
            RoomPid = get_the_room(Room),
            %% post
            RoomPid ! {self(),
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

loop(Req, DocRoot) ->
    case Req:get(method) of

        %%
        %% Get new data
        %%
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            handleGET(Req, DocRoot);
        'POST' ->
            handlePOST(Req, DocRoot);
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
