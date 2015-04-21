-module(simple_cache_server).
-behaviour(gen_server).

-include("simple_cache.hrl").

-define(ETS_OPTIONS, [{read_concurrency, true},
                      {write_concurrency, true},
                      named_table,
                      protected]).

-record(state, {table}).

%%=============================================================================
%% API Function Exports
%%=============================================================================
-export([start_link/1,
         ops_info/1,
         ops_list/1,
         sync_set/3,
         set/3, set/4,
         sync_set/4,
         cond_set/5,
         get/2, get/3,
         clear/2,
         sync_clear/2,
         clear_all/1,
         sync_clear_all/1]).

%%=============================================================================
%% gen_server Function Exports
%%=============================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%=============================================================================
%% API Function Definitions
%%=============================================================================
-spec start_link(cache_name()) -> ignore | {error, term()} | {ok, pid()}.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

-spec ops_info(cache_name()) -> list().
ops_info(Name) ->
    ets:info(Name).

-spec ops_list(cache_name()) -> list().
ops_list(Name) ->
    ets:tab2list(Name).

-spec set(cache_name(), any(), any()) -> ok.
set(Name, Key, Value) ->
    gen_server:cast(Name, {set, Key, Value, infinity}).

-spec sync_set(cache_name(), any(), any()) -> any().
sync_set(Name, Key, Value) ->
    gen_server:call(Name, {set, Key, Value, infinity}).

-spec set(cache_name(), any(), any(), simple_cache:expire()) -> ok.
set(Name, Key, Value, Expires) ->
    gen_server:cast(Name, {set, Key, Value, Expires}).

-spec sync_set(cache_name(), any(), any(), simple_cache:expire()) -> any().
sync_set(Name, Key, Value, Expires) ->
    gen_server:call(Name, {set, Key, Value, Expires}).

-spec cond_set(cache_name(), any(), any(), simple_cache:conditional(),
               simple_cache:expire()) -> {ok, boolean()}.
cond_set(Name, Key, Value, Conditional, Expires) ->
    gen_server:call(Name, {set, Key, Value, Conditional, Expires}).

-spec get(cache_name(), any()) -> {error, not_found} | {ok, any()}.
get(Name, Key) -> get_by_key(Name, Key, undefined).

-spec get(cache_name(), any(), any()) -> {ok,_}.
get(Name, Key, Default) -> get_by_key(Name, Key, Default).

-spec clear(cache_name(), any()) -> ok.
clear(Name, Key) ->
    gen_server:cast(Name, {clear, Key}),
    ok.

-spec sync_clear(cache_name(), any()) -> ok.
sync_clear(Name, Key) ->
    gen_server:call(Name, {sync_clear, Key}).

-spec clear_all(cache_name()) -> ok.
clear_all(Name) ->
    gen_server:cast(Name, clear_all),
    ok.

-spec sync_clear_all(cache_name()) -> ok.
sync_clear_all(Name) ->
    gen_server:call(Name, sync_clear_all).

%%=============================================================================
%% gen_server Function Definitions
%%=============================================================================
init([Name]) ->
    Name = ets:new(Name, ?ETS_OPTIONS),
    {ok, #state{table = Name}}.

handle_call(ops_list, _From,  #state{table = Table} = State) ->
    {reply, ets:tab2list(Table), State};
handle_call({set, Key, Value, infinity}, _From, #state{table = Table} = State) ->
    true = insert(Table, Key, Value, infinity),
    {reply, ok, State};
handle_call({set, Key, Value, Expires}, _From, #state{table = Table} = State) ->
    _Ref = insert(Table, Key, Value, Expires),
    {reply, ok, State};
handle_call({set, Key, Value, Conditional, Expires}, _From, #state{table = Table} = State) ->
    Test = case ?MODULE:get(Table, Key) of
               {ok, OldValue}     -> Conditional(OldValue);
               {error, not_found} -> true
           end,
    Test andalso insert(Table, Key, Value, Expires),
    {reply, {ok, Test}, State};
handle_call({sync_clear, Key}, _From, #state{table = Table} = State) ->
    ets:delete(Table, Key),
    {reply, ok, State};
handle_call(sync_clear_all, _From, #state{table = Table} = State) ->
    true = ets:delete_all_objects(Table),
    {reply, ok, State}.

handle_cast({set, Key, Value, infinity}, #state{table = Table} = State) ->
    true = insert(Table, Key, Value, infinity),
    {noreply, State};
handle_cast({set, Key, Value, Expires}, #state{table = Table} = State) ->
    _Ref = insert(Table, Key, Value, Expires),
    {noreply, State};
handle_cast({clear, Key}, #state{table = Table} = State) ->
    ets:delete(Table, Key),
    {noreply, State};
handle_cast(clear_all, #state{table = Table} = State) ->
    ets:delete_all_objects(Table),
    {noreply, State}.

handle_info({expire, Key}, #state{table = Table} = State) ->
    ets:delete(Table, Key),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%=============================================================================
%% Internal functionality
%%=============================================================================

insert(Table, Key, Value, infinity) ->
    true = ets:insert(Table, {Key, Value, infinity});
insert(Table, Key, Value, Expires) ->
    true = ets:insert(Table, {Key, Value, Expires}),
    _Ref = erlang:send_after(1000 * Expires, Table, {expire, Key}).

get_by_key(Table, Key, Default) ->
    case catch ets:lookup(Table, Key) of
        {'EXIT', {badarg, _}} ->
            {error, cache_not_found};
        Value ->
            get_value(Value, Default)
    end.

get_value([], undefined) ->
    {error, not_found};
get_value([], Default) ->
    {ok, Default};
get_value([{_Key, Value, _Expires}], _Default) ->
    {ok, Value}.
