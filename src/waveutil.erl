-module(waveutil).

-export([
         nowString/0,
         stringJoin/1,
         stringJoin/2
        ]).

%%
%% FIXME: force fixed-width
%%
nowString() ->
    {{Year,Month,Day},{Hour,Min,Seconds}} = erlang:universaltime(),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                 [Year,Month,Day,Hour,Min,Seconds])).

stringJoin(Arr) ->
    stringJoin(Arr, [], []).
stringJoin(Arr, Sep) ->
    stringJoin(Arr, Sep, []).
stringJoin([], _Sep, Ret) ->
    Ret;
stringJoin(Arr, Sep, Ret) ->
    [H|T] = Arr,
    case Ret of
        [] ->
            stringJoin(T, Sep, H);
        _ ->
            stringJoin(T, Sep, string:concat(Ret, string:concat(Sep, H)))
    end.
