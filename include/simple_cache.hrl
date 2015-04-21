-type expire() :: infinity | non_neg_integer().
-type conditional() :: fun((any()) -> boolean()).
-type cache_name() :: atom().

-define(DEFAULT_CACHE, default_simple_cache).
