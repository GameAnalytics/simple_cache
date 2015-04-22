-module(simple_cache_app).
-behaviour(application).

-include("simple_cache.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%=============================================================================
%% Application callbacks
%%=============================================================================
-spec start(any(), term()) -> {'error', term()} | {'ok', pid()}.
start(_StartType, _StartArgs) ->
    {ok, Pid} = simple_cache_sup:start_link(),
    simple_cache:new(?DEFAULT_CACHE),
    {ok, Pid}.

-spec stop(atom()) -> 'ok'.
stop(_State) ->
    simple_cache:delete(?DEFAULT_CACHE).
