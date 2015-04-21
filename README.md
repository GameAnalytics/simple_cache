simple_cache
============

Simple Memory-based Erlang cache service using ETS. It provides async and sync
acesss API (check for `sync_` prefix).

Usage
============

Include it into `rebar.config`:
```erlang
{simple_cache, "",
  {git, "git@github.com:GameAnalytics/simple_cache.git", {branch, "master"}}}
```

Start OTP application:
```erlang
ok = application:start(simple_cache).
```

Insert/update value (optional expiration in ms):
```erlang
Name = cache_name, % Has to be an atom!
ok = simple_cache:set(Name, <<"foo">>, <<"bar">>),
ok = simple_cache:set(Name, <<"foo">>, <<"bar">>, 5000),
ok = simple_cache:sync_set(Name, <<"foo">>, <<"bar">>),
ok = simple_cache:sync_set(Name, <<"foo">>, <<"bar">>, infinity),
```

Insert/update value based on predicate result:
```erlang
PredFun = fun (<<"bar">>) -> false end,
{ok, false} = simple_cache:cond_set(Name, <<"foo">>, <<"baz">>, PredFun, infinity).
```

Get value by key (optional default value):
```erlang
{ok, <<"bar">>} = simple_cache:lookup(Name, <<"foo">>),
{ok, <<"bar">>} = simple_cache:lookup(Name, <<"foo">>, <<"default">>),
```

Remove cached values all or by key:
```erlang
ok = simple_cache:flush(Name).
ok = simple_cache:sync_flush(Name).
ok = simple_cache:flush(Name, <<"foo">>).
```

Operations helpers:
```erlang
simple_cache:ops_info(Name).
simple_cache:ops_list(Name).
simple_cache:list().          % List of all registered caches
```

For more information about usage refer to tests.

Credits
============
* Andrew Stanton - https://github.com/Refefer
* Gustav Simonsson - https://github.com/Gustav-Simonsson
* Christian Lundgren - https://github.com/chrisavl
* Wasif Malik - https://github.com/wmalik
* Sukumar Yethadka - https://github.com/sthadka
