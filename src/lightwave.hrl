%% lightwave/src/lightwave.hrl

%% timeout for internal messages that should just be acked
-define(ACK_TIMEOUT, 100).

%% max time to wait a poll before returning a timeout
-define(GET_TIMEOUT, 120000).
%%-define(GET_TIMEOUT, 5000).
