-module(waveutil).

-export([
         nowString/0
        ]).

%%
%% FIXME: force fixed-width
%%
nowString() ->
    {{Year,Month,Day},{Hour,Min,Seconds}} = erlang:universaltime(),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                 [Year,Month,Day,Hour,Min,Seconds])).
