# simple_cache

Simple Memory-based Erlang cache service using ETS. It provides async and sync
acesss API (check for `sync_` prefix).

## Usage

Include it into `rebar.config`:
```erlang
{simple_cache, "",
  {git, "git@github.com:GameAnalytics/simple_cache.git", {branch, "master"}}}
```

Start OTP application:
```erlang
ok = application:start(simple_cache).
```

### Single cache

If you plan to use a single instance of simple_cache,

Insert/update value:
```erlang
ok = simple_cache:set(<<"foo">>, <<"bar">>),       % Returns immediately
ok = simple_cache:sync_set(<<"foo">>, <<"bar">>),  % Blocks until cache is set
```

Insert/update value with expiration in ms:
```erlang
ok = simple_cache:setex(<<"foo">>, <<"bar">>, 5000),
ok = simple_cache:sync_setex(<<"foo">>, <<"bar">>, infinity),
```

Insert/update value based on predicate result:
```erlang
PredFun = fun (<<"bar">>) -> false end,
{ok, false} = simple_cache:cond_set(<<"foo">>, <<"baz">>, PredFun, infinity).
```

Get value by key:
```erlang
{ok, <<"bar">>} = simple_cache:get(<<"foo">>),
```

Get value by key with default value given:
```erlang
{ok, <<"bar">>} = simple_cache:get_def(<<"foo">>, <<"default">>),
```

Remove cached values all or by key:
```erlang
ok = simple_cache:clear_all().
ok = simple_cache:sync_clear_all().
ok = simple_cache:clear(<<"foo">>).
ok = simple_cache:sync_clear(<<"foo">>).
```

Operations helpers:
```erlang
simple_cache:ops_info().
simple_cache:ops_list().
```

For more information about usage refer to tests.

### Multiple caches

If you plan to use a many instances of simple_cache,

Insert/update value:
```erlang
Name = cache_name, % Has to be an atom!
ok = simple_cache:set(Name, <<"foo">>, <<"bar">>),
ok = simple_cache:sync_set(Name, <<"foo">>, <<"bar">>),
```

Insert/update value with expiration in ms:
```erlang
ok = simple_cache:set(Name, <<"foo">>, <<"bar">>, 5000),
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
ok = simple_cache:clear(Name).
ok = simple_cache:sync_clear(Name).
ok = simple_cache:clear(Name, <<"foo">>).
```

Operations helpers:
```erlang
simple_cache:ops_info(Name).
simple_cache:ops_list(Name).
simple_cache:list().          % List of all registered caches
```

For more information about usage refer to tests.

## Credits
* Andrew Stanton - https://github.com/Refefer
* Gustav Simonsson - https://github.com/Gustav-Simonsson
* Christian Lundgren - https://github.com/chrisavl
* Wasif Malik - https://github.com/wmalik
* Sukumar Yethadka - https://github.com/sthadka
