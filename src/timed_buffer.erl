-module(timed_buffer).

-behaviour(gen_server).

-export([start_link/3, start_link/4, add/2, flush/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {flush_fun, flush_count, flush_timeout, timer, items = [], item_count = 0}).

%% Public API

start_link(FlushFun, FlushCount, FlushTimeout) ->
    gen_server:start_link(?MODULE, [FlushFun, FlushCount, FlushTimeout], []).

start_link(Name, FlushFun, FlushCount, FlushTimeout) ->
    gen_server:start_link(Name, ?MODULE, [FlushFun, FlushCount, FlushTimeout], []).

add(Item, Server) ->
    gen_server:call(Server, {add, Item}).

flush(Server) ->
    gen_server:call(Server, flush).

%% Callbacks

init([FlushFun, FlushCount, FlushTimeout]) ->
    {ok, #state{flush_fun = FlushFun, flush_count = FlushCount, flush_timeout = FlushTimeout}}.

handle_call({add, Item}, _From, State = #state{ items = [], flush_timeout = FlushTimeout, timer = undefined }) ->
    % this is the first item to be queued
    % so we start the flush timer
    Timer = erlang:send_after(FlushTimeout, self(), flush),
    handle_call({add, Item}, _From, State#state{ timer = Timer });

handle_call({add, Item}, _From, State) ->
    NewQueue = [Item | State#state.items],
    NewState = State#state{ items = NewQueue, item_count = State#state.item_count + 1 },
    {reply, ok, maybe_flush(NewState)};

handle_call(flush, _From, State) ->
    {reply, ok, flush_if_any(State)}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, State) ->
    {noreply, flush_if_any(State)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

maybe_flush(State = #state{ flush_count = _SameCount, item_count = _SameCount }) ->
    flush_if_any(State);
maybe_flush(State) ->
    State.

flush_if_any(State = #state{ items = [] }) ->
    cancel_timer(State);
flush_if_any(State = #state{ flush_fun = FlushFun, items = Items }) ->
    FlushFun(Items),
    cancel_timer(State#state{ items = [], item_count = 0}).

cancel_timer(State = #state{ timer = undefined }) ->
    State;
cancel_timer(State = #state{ timer = Timer }) ->
    erlang:cancel_timer(Timer),
    State#state{ timer = undefined }.
