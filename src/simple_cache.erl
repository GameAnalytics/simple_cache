-module(simple_cache).

-include("simple_cache.hrl").

%% API
-export([new/1,
         delete/1,
         list/0,
         ops_info/1,
         ops_list/1,
         set/3, set/4,
         sync_set/3, sync_set/4,
         cond_set/5,
         lookup/2, lookup/3,
         flush/1, flush/2,
         sync_flush/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec new(cache_name()) -> ok.
new(Name) ->
    ok = simple_cache_sup:start_server(Name).

-spec delete(cache_name()) -> ok.
delete(Name) ->
    ok = simple_cache_sup:stop_server(Name).

-spec list() -> [cache_name()].
list() ->
    simple_cache_sup:servers().

-spec ops_info(cache_name()) -> list().
ops_info(Name) ->
    simple_cache_server:ops_info(Name).

-spec ops_list(cache_name()) -> list().
ops_list(Name) ->
    simple_cache_server:ops_list(Name).

-spec set(cache_name(), any(), any()) -> ok.
set(Name, Key, Value) ->
    simple_cache_server:set(Name, Key, Value).

-spec sync_set(cache_name(), any(), any()) -> any().
sync_set(Name, Key, Value) ->
    simple_cache_server:sync_set(Name, Key, Value).

-spec set(cache_name(), any(), any(), expire()) -> ok | {error, invalid_expire, any()}.
set(Name, Key, _Value, 0) ->
    simple_cache_server:flush(Name, Key);
set(Name, Key, Value, Expires) when is_number(Expires) ->
    simple_cache_server:set(Name, Key, Value, Expires);
set(_Name, _Key, _Value, Expires) ->
    {error, invalid_expire, Expires}.

-spec sync_set(cache_name(), any(), any(), expire()) -> any().
sync_set(Name, Key, _Value, 0) ->
    simple_cache_server:sync_flush(Name, Key);
sync_set(Name, Key, Value, Expires) when is_number(Expires) ->
    simple_cache_server:sync_set(Name, Key, Value, Expires);
sync_set(_Name, _Key, _Value, Expires) ->
    {error, invalid_expire, Expires}.

-spec cond_set(cache_name(), any(), any(), conditional(), expire()) -> any().
cond_set(Name, Key, Value, Conditional, Expires) when Expires > 0 ->
    simple_cache_server:cond_set(Name, Key, Value, Conditional, Expires).

-spec lookup(cache_name(), any()) -> {error,not_found} | {ok, any()}.
lookup(Name, Key) ->
    simple_cache_server:lookup(Name, Key).

-spec lookup(cache_name(), any(), any()) -> {ok,_}.
lookup(Name, Key, Default) ->
    simple_cache_server:lookup(Name, Key, Default).

-spec flush(cache_name(), any()) -> ok.
flush(Name, Key) ->
    simple_cache_server:flush(Name, Key).

-spec flush(cache_name()) -> ok.
flush(Name) ->
    simple_cache_server:flush(Name).

-spec sync_flush(cache_name()) -> ok.
sync_flush(Name) ->
    simple_cache_server:sync_flush(Name).
