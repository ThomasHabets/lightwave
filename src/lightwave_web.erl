%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for lightwave.

-module(lightwave_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2,
         room/0, room/3]).

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
                          {N, Line} = D,
                          io:format("Iter: ~p ~p~n", [N, Line]),
                          case N+1 > TimeStart of
                              true ->
                                  io:format("send: ~p~n", [Line]),
                                  From ! D;
                              Any ->
                                  io:format("don't send: ~p~n", [Any]),
                                  ok
                          end
                  end, Data).

atoi(A) ->
    {R,_} = string:to_integer(A),
    R.

room() ->
    io:format("room: booting~n"),
    ?MODULE:room(3, [], [
                         {1, list_to_binary("Initial message")},
                         {2, list_to_binary("Initial message, line 2")}
                        ]).
%%
%% Time: next timestamp in channel
%% Users: Pids of users subscribed
%% Data: ordered list of all data in channel
%%
room(Time, Users, Data) ->
    io:format("room: looping~n"),
    receive
        {From, subscribe, TimeStart} ->
            io:format("room: subscribe ~p ~p~n", [From, TimeStart]),
            From ! subscribed,
            roomFlush(From, atoi(TimeStart), Data),
            ?MODULE:room(Time, [From | Users], Data);
        {From, unsubscribe} ->
            io:format("room: unsubscribe~n"),
            From ! unsubscribed,
            ?MODULE:room(Time, Users -- [From], Data);
        {From, post, Message} ->
            io:format("room: post~n"),
            From ! posted,
            lists:foreach(fun(User) ->
                    % broadcast the message
                    User ! {Time,Message}
                end, Users),
            ?MODULE:room(Time+1, Users,
                         lists:reverse([{Time,Message}
                                        | lists:reverse(Data)]));
        _ ->
            io:format("room: unknown message received~n"),
            ?MODULE:room(Time, Users, Data)
    end.

get_the_room() ->
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

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of

        %
        % Get new data
        %
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "chat/foo/" ++ FromTime ->
                    Room = "foo",
                    RoomPid = get_the_room(),
                    RoomPid ! {self(), subscribe, FromTime},
                    receive
                        subscribed ->
                            % subscription is ok
                            % now wait for a message
                            receive
                                {MsgTime, Message} ->
                                    {Type, Message} = {ok, Message}
                            after ?GET_TIMEOUT ->
                                    {Type, Message} = {error, <<"timeout">>}
                            end
                    after ?ACK_TIMEOUT ->
                            io:format("webloop: can't sub?~n"),
                            {Type, Message} = {error, <<"timeout">>}
                    end,

                    case Type of
                        error ->
                            % we need to unsubscribe from the room
                            % because it failed somewhere
                            RoomPid ! {self(), unsubscribe},
                            receive
                                unsubscribed ->
                                    ok
                            after 1000 ->
                                % FIXME: log subscribe timeout, no data
                                ok
                            end;
                        ok ->
                            % everything went fine
                            ok
                    end,
                    
                    % send back the JSON message
                    Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                {Type, Message}
                            ]
                        })
                    });
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                "chat" ->
                    io:format("POST chat~n"),
                    Data = Req:parse_post(),
                    Room = get_the_room(),
                    % post
                    Room ! {self(), post, list_to_binary(proplists:get_value("message", Data))},
                    receive
                        posted ->
                            % posted
                            Body = {ok, <<"posted">>}
                    after 1000 ->
                        % something went wrong
                        Body = {error, <<"timeout">>}
                    end,
                    
                    % send back the JSON message
                    Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                Body
                            ]
                        })
                    });
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
