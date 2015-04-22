-module(simple_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_server/1,
         stop_server/1,
         servers/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Restart, Type),
{   I,                      % Id
    {I, start_link, []},    % Start function
    Restart,                % Restart strategy
    5000,                   % Shutdown strategy/time
    Type,                   % Type of process
    [I]                     % Modules
}).

%%=============================================================================
%% API functions
%%=============================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% TODO Check specs
-spec start_server(atom()) -> ok.
start_server(ServerName) ->
    {ok, _Pid} = supervisor:start_child(?MODULE, [ServerName]),
    ok.

-spec stop_server(atom()) -> ok.
stop_server(ServerName) ->
    supervisor:terminate_child(?MODULE, erlang:whereis(ServerName)).

-spec servers() -> [atom()].
servers() ->
    lists:map(fun ({undefined, Pid, worker, [simple_cache_server]}) ->
                      {registered_name, Name} = erlang:process_info(Pid, registered_name),
                      Name
              end, supervisor:which_children(?MODULE)).

%%=============================================================================
%% Supervisor callbacks
%%=============================================================================
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [?CHILD(simple_cache_server, temporary, worker)]}}.
