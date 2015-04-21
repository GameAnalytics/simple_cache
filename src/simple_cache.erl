-module(simple_cache).

-include("simple_cache.hrl").

%% API
-export([new/1,
         delete/1,
         list/0]).

-export([ops_info/0, ops_info/1,
         ops_list/0, ops_list/1,
         set/2, set/3,
         sync_set/2, sync_set/3,
         setex/3, setex/4,
         sync_setex/3, sync_setex/4,
         cond_set/4, cond_set/5,
         get/1, get/2,
         get_def/2, get_def/3,
         clear/1, clear/2,
         sync_clear/1, sync_clear/2,
         clear_all/0, clear_all/1,
         sync_clear_all/0, sync_clear_all/1]).

-export_type([expire/0,
              conditional/0,
              cache_name/0]).

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

-spec ops_info() -> list().
ops_info() ->
    ops_info(?DEFAULT_CACHE).

-spec ops_info(cache_name()) -> list().
ops_info(Name) ->
    simple_cache_server:ops_info(Name).

-spec ops_list() -> list().
ops_list() ->
    ops_list(?DEFAULT_CACHE).

-spec ops_list(cache_name()) -> list().
ops_list(Name) ->
    simple_cache_server:ops_list(Name).

-spec set(any(), any()) -> ok.
set(Key, Value) ->
    set(?DEFAULT_CACHE, Key, Value).

-spec set(cache_name(), any(), any()) -> ok.
set(Name, Key, Value) ->
    simple_cache_server:set(Name, Key, Value).

-spec sync_set(any(), any()) -> any().
sync_set(Key, Value) ->
    sync_set(?DEFAULT_CACHE, Key, Value).

-spec sync_set(cache_name(), any(), any()) -> any().
sync_set(Name, Key, Value) ->
    simple_cache_server:sync_set(Name, Key, Value).


-spec setex(any(), any(), expire()) -> ok | {error, invalid_expire, any()}.
setex(Key, Value, Expires) ->
    setex(?DEFAULT_CACHE, Key, Value, Expires).

-spec setex(cache_name(), any(), any(), expire()) -> ok | {error, invalid_expire, any()}.
setex(Name, Key, _Value, 0) ->
    simple_cache_server:clear(Name, Key);
setex(Name, Key, Value, Expires) when is_number(Expires) ->
    simple_cache_server:set(Name, Key, Value, Expires);
setex(_Name, _Key, _Value, Expires) ->
    {error, invalid_expire, Expires}.

-spec sync_setex(any(), any(), expire()) -> any().
sync_setex(Key, Value, Expires) ->
    sync_setex(?DEFAULT_CACHE, Key, Value, Expires).

-spec sync_setex(cache_name(), any(), any(), expire()) -> any().
sync_setex(Name, Key, _Value, 0) ->
    simple_cache_server:sync_clear(Name, Key);
sync_setex(Name, Key, Value, Expires) when is_number(Expires) ->
    simple_cache_server:sync_set(Name, Key, Value, Expires);
sync_setex(_Name, _Key, _Value, Expires) ->
    {error, invalid_expire, Expires}.

-spec cond_set(any(), any(), conditional(), expire()) -> any().
cond_set(Key, Value, Conditional, Expires) ->
    cond_set(?DEFAULT_CACHE, Key, Value, Conditional, Expires).

-spec cond_set(cache_name(), any(), any(), conditional(), expire()) -> any().
cond_set(Name, Key, Value, Conditional, Expires) when Expires > 0 ->
    simple_cache_server:cond_set(Name, Key, Value, Conditional, Expires).

-spec get(any()) -> {error, not_found} | {ok, any()}.
get(Key) ->
    get(?DEFAULT_CACHE, Key).

-spec get(cache_name(), any()) -> {error, not_found} | {ok, any()}.
get(Name, Key) ->
    simple_cache_server:get(Name, Key).

-spec get_def(any(), any()) -> {ok, _}.
get_def(Key, Default) ->
    get_def(?DEFAULT_CACHE, Key, Default).

-spec get_def(cache_name(), any(), any()) -> {ok, _}.
get_def(Name, Key, Default) ->
    simple_cache_server:get(Name, Key, Default).

-spec clear(any()) -> ok.
clear(Key) ->
    clear(?DEFAULT_CACHE, Key).

-spec clear(cache_name(), any()) -> ok.
clear(Name, Key) ->
    simple_cache_server:clear(Name, Key).

-spec sync_clear(any()) -> ok.
sync_clear(Key) ->
    sync_clear(?DEFAULT_CACHE, Key).

-spec sync_clear(cache_name(), any()) -> ok.
sync_clear(Name, Key) ->
    simple_cache_server:sync_clear(Name, Key).

-spec clear_all() -> ok.
clear_all() ->
    clear_all(?DEFAULT_CACHE).

-spec clear_all(cache_name()) -> ok.
clear_all(Name) ->
    simple_cache_server:clear_all(Name).

-spec sync_clear_all() -> ok.
sync_clear_all() ->
    sync_clear_all(?DEFAULT_CACHE).

-spec sync_clear_all(cache_name()) -> ok.
sync_clear_all(Name) ->
    simple_cache_server:sync_clear_all(Name).
