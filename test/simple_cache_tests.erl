-module(simple_cache_tests).
-include_lib("eunit/include/eunit.hrl").
-include("simple_cache.hrl").

%% =============================================================================
simple_cache_test_() ->
    {setup,
         fun setup/0,
         fun teardown/1,
        [
         {"Set/get test",            fun test_set_get/0},
         {"Get default test",        fun test_get_default/0},
         {"Set zero expire",         fun test_zero_expire/0},
         {"Set invalid expire",      fun test_invalid_expire/0},
         {"Expire test",             fun test_expire/0},
         {"Flush entry test",        fun test_clear/0},
         {"Flush all test",          fun test_clear_all/0},
         {"Sync clear all test",     fun test_sync_clear_all/0},
         {"Info operations test",    fun test_ops_info/0},
         {"List operations test",    fun test_ops_list/0},
         {"Sync set/get test",       fun test_sync_set_get/0},
         {"Sync set zero expire",    fun test_sync_zero_expire/0},
         {"Sync expire test",        fun test_sync_expire/0},
         {"Sync set invalid expire", fun test_sync_invalid_expire/0},
         {"Conditional set",         fun test_conditional_set/0},
         {"Multiple caches",         fun test_multiple_caches/0}
        ]
    }.

%% =============================================================================

setup() ->
    application:start(simple_cache).

teardown(_) ->
    application:stop(simple_cache).

test_set_get() ->
    simple_cache:set(<<"foo">>, <<"bar">>),
    sleep(),
    ?assertEqual({ok, <<"bar">>}, simple_cache:get_def(<<"foo">>, 1)).

test_zero_expire() ->
    simple_cache:set(<<"foo">>, <<"bar">>),
    sleep(),
    ?assertEqual(ok, simple_cache:setex(<<"foo">>, <<"bar">>, 0)),
    sleep(500),
    ?assertEqual({error, not_found}, simple_cache:get(<<"foo">>)).

test_invalid_expire() ->
    ?assertEqual({error, invalid_expire,foo}, simple_cache:setex(<<"foo">>, <<"bar">>, foo)).

test_get_default() ->
    simple_cache:sync_clear_all(),
    ?assertEqual({ok, <<"bar1">>}, simple_cache:get_def(<<"foo">>, <<"bar1">>)).

test_clear() ->
    simple_cache:set(<<"foo1">>, <<"bar1">>),
    simple_cache:set(<<"foo2">>, <<"bar2">>),
    simple_cache:set(<<"foo3">>, <<"bar3">>),
    sleep(),
    simple_cache:clear(<<"foo2">>),
    simple_cache:clear(<<"foo3">>),
    sleep(),
    ?assertEqual({ok, <<"bar1">>}, simple_cache:get(<<"foo1">>)).

test_clear_all() ->
    simple_cache:set(<<"foo1">>, <<"bar1">>),
    simple_cache:set(<<"foo2">>, <<"bar2">>),
    simple_cache:set(<<"foo3">>, <<"bar3">>),
    simple_cache:clear_all(),
    sleep(),
    ?assertEqual({error, not_found}, simple_cache:get(<<"foo1">>)).

test_sync_clear_all() ->
    simple_cache:sync_set(<<"foo1">>, <<"bar1">>),
    ?assertEqual({ok, <<"bar1">>}, simple_cache:get(<<"foo1">>)),
    ok = simple_cache:sync_clear_all(),
    ?assertEqual({error, not_found}, simple_cache:get(<<"foo1">>)).

test_expire() ->
    simple_cache:setex(<<"foo1">>, <<"bar1">>, 1),
    sleep(1500),
    simple_cache:set(<<"foo2">>, <<"bar2">>),
    sleep(),
    ?assertEqual({error, not_found}, simple_cache:get(<<"foo1">>)),
    ?assertEqual({ok, <<"bar2">>}, simple_cache:get(<<"foo2">>)).

test_ops_info() ->
    simple_cache:sync_setex(<<"foo">>, <<"bar">>, 2),
    ?assertEqual(1, proplists:get_value(size, simple_cache:ops_info())).

test_ops_list() ->
    simple_cache:sync_setex(<<"foo">>, <<"bar">>, 2),
    ?assertEqual([{<<"foo">>,<<"bar">>, 2}], simple_cache:ops_list()).

test_sync_set_get() ->
    simple_cache:sync_set(<<"foo">>, <<"bar">>),
    ?assertEqual({ok, <<"bar">>}, simple_cache:get_def(<<"foo">>, 1)).

test_sync_zero_expire() ->
    simple_cache:sync_set(<<"foo">>, <<"bar">>),
    ?assertEqual(ok, simple_cache:sync_setex(<<"foo">>, <<"bar">>, 0)),
    ?assertEqual({error, not_found}, simple_cache:get(<<"foo">>)).

test_sync_expire() ->
    simple_cache:sync_setex(<<"foo1">>, <<"bar1">>, 1),
    simple_cache:sync_set(<<"foo2">>, <<"bar2">>),

    sleep(1500),

    ?assertEqual({error, not_found}, simple_cache:get(<<"foo1">>)),
    ?assertEqual({ok, <<"bar2">>}, simple_cache:get(<<"foo2">>)).

test_sync_invalid_expire() ->
    ?assertEqual({error, invalid_expire, foo}, simple_cache:sync_setex(<<"foo">>, <<"bar">>, foo)).

test_conditional_set() ->
    ok = simple_cache:sync_set(<<"foo">>, <<"bar">>),
    ?assertEqual({ok, <<"bar">>}, simple_cache:get(<<"foo">>)),
    ?assertEqual({ok, false}, simple_cache:cond_set(<<"foo">>, <<"baz">>,
                                                    fun (<<"bar">>) -> false end, infinity)),
    ?assertEqual({ok, <<"bar">>}, simple_cache:get(<<"foo">>)),
    ?assertEqual({ok, true}, simple_cache:cond_set(<<"foo">>, <<"baz">>,
                                                   fun (<<"bar">>) -> true end, infinity)),
    ?assertEqual({ok, <<"baz">>}, simple_cache:get(<<"foo">>)).

test_multiple_caches() ->
    simple_cache:new(first),
    simple_cache:new(second),

    ?assertEqual(lists:sort([?DEFAULT_CACHE, first, second]),
                 lists:sort(simple_cache:list())),

    simple_cache:sync_set(first, a, 1),
    simple_cache:sync_set(second, b, 2),

    ?assertEqual({ok, 1}, simple_cache:get(first, a)),
    ?assertEqual({ok, 2}, simple_cache:get(second, b)),
    ?assertNotEqual({ok, 1}, simple_cache:get(second, a)),
    ?assertNotEqual({ok, 2}, simple_cache:get(first, b)),

    simple_cache:sync_clear(first, a),
    simple_cache:sync_clear(second, b),

    ?assertEqual({error, not_found}, simple_cache:get(first, a)),
    ?assertEqual({error, not_found}, simple_cache:get(second, b)),

    simple_cache:delete(first),
    simple_cache:delete(second),

    sleep(),

    ?assertEqual({error, cache_not_found}, simple_cache:get(first, a)),
    ?assertEqual({error, cache_not_found}, simple_cache:get(second, b)).

%%=============================================================================
%% Internal functionality
%%=============================================================================

sleep() ->
    timer:sleep(100).

sleep(Ms) ->
    timer:sleep(Ms).
