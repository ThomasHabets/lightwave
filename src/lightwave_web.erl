%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for lightwave.

-module(lightwave_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2, room/2]).

-define(TIMEOUT, 20000).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

room() ->
    room([], []).
room(Users, Data) ->
    receive
        {From, subscribe} ->
            io:format("room DEBUG: subscribed ~s~n", [From]),
            From ! subscribed,
            ?MODULE:room([From | Users], Data);
        {From, unsubscribe} ->
            From ! unsubscribed,
            ?MODULE:room(Users -- [From], Data);
        {From, post, Message} ->
            From ! posted,
            lists:foreach(fun(User) ->
                    % broadcast the message
                    User ! Message
                end, Users),
            ?MODULE:room(Users, Data);
        Any ->
            io:format("room: unknown message: ~s~n", [Any]),
            ?MODULE:room(Users, Data)
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
                room()
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
            FromId = 1,
            case Path of
                "chat/" ++ ChatRest ->
                    Room = "foo",
                    RoomPid = get_the_room(),
                    RoomPid ! {self(), subscribe},
                    receive
                        subscribed ->
                            % subscription is ok
                            % now wait for a message
                            receive
                                Message ->
                                    {Type, Message} = {ok, Message}
                            after ?TIMEOUT ->
                                % we waited too long
                                {Type, Message} = {error, <<"timeout">>}
                            end
                    after 1000 ->
                        % subscription failed on time
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
