-module(wavefinder).
-author("Thomas Habets <thomas@habets.pp.se>").
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start/0, register/2, forget/1, get/1]).

% These are all wrappers for calls to the server
start() ->
     gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
register(Name, Pid) ->
     gen_server:call(?MODULE, {register, Name, Pid}). 
forget(Name) ->
     gen_server:call(?MODULE, {forget, Name}).
get(Name) ->
     gen_server:call(?MODULE, {get, Name}).

% This is called when a connection is made to the server
init([]) ->
    Data = dict:new(),
    {ok, Data}.

% handle_call is invoked in response to gen_server:call
handle_call({register, Name, Pid}, _From, Data) ->
    {reply, ok, dict:store(Name, Pid, Data)};

handle_call({forget, Name}, _From, Data) ->
    {reply, ok, dict:erase(Name, Data)};

handle_call({get, Name}, _From, Data) ->
    E = dict:find(Name, Data),
    case E of
        {ok, E2} ->
            case erlang:is_process_alive(E2) of
                true ->
                    {reply, {ok, E2}, Data};
                false ->
                    {reply, {ok, []}, dict:erase(Name, Data)}
            end;
        _ ->
            {reply, {ok, []}, Data}
    end;

handle_call(_Message, _From, Data) ->
    {reply, error, Data}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Data) -> {noreply, Data}.
handle_info(_Message, Data) -> {noreply, Data}.
terminate(_Reason, _Data) -> ok.
code_change(_OldVersion, Data, _Extra) -> {ok, Data}.
