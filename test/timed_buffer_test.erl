-module(timed_buffer_test).
-include_lib("etest/include/etest.hrl").

-compile(export_all).

-define(WAITFOR(Msg),
    timer:tc(
      fun () ->
            receive
                Msg ->
                    true;
                Else ->
                    Else
            after 100 ->
                    timeout
            end
        end
     )).

test_timer() ->
    Buffer = buffer(3, 10),
    timed_buffer:add(x, Buffer),
    timed_buffer:add(x, Buffer),
    {Time, MatchingMsg} = ?WAITFOR([x, x]),
    io:format("Time: ~p\n", [Time]),
    ?assert(MatchingMsg),
    ?assert(Time < 15000).

test_count() ->
    Buffer = buffer(3, 10),
    timed_buffer:add(x, Buffer),
    timed_buffer:add(x, Buffer),
    timed_buffer:add(x, Buffer),
    {Time, MatchingMsg} = ?WAITFOR([x, x, x]),
    ?assert(MatchingMsg),
    ?assert(Time < 10).

test_cancel_timers() ->
    Buffer = buffer(3, 10),
    timed_buffer:add(x, Buffer),
    timed_buffer:add(x, Buffer),
    timed_buffer:add(x, Buffer),
    {_, true} = ?WAITFOR([x, x, x]),
    {_, MatchingMsg} = ?WAITFOR(undefined),
    ?assert_equal(timeout, MatchingMsg).

test_count_and_timers() ->
    Buffer = buffer(3, 10),
    timed_buffer:add(x, Buffer),
    timed_buffer:add(x, Buffer),
    timed_buffer:add(x, Buffer),
    {_, true} = ?WAITFOR([x, x, x]),
    {_, MatchingMsg} = ?WAITFOR(undefined),
    ?assert_equal(timeout, MatchingMsg),
    timed_buffer:add(y, Buffer),
    {Time2, MatchingMsg2} = ?WAITFOR([y]),
    ?assert(MatchingMsg2),
    ?assert(Time2 < 15000).

test_flush() ->
    Buffer = buffer(3, 10),
    timed_buffer:add(x, Buffer),
    timed_buffer:flush(Buffer),
    {Time, MatchingMsg} = ?WAITFOR([x]),
    ?assert(MatchingMsg),
    ?assert(Time < 10).


buffer(FlushCount, FlushTimeout) ->
    Self         = self(),
    FlushFun     = fun(Buffer) -> Self ! Buffer end,
    {ok, Buffer} = timed_buffer:start_link(FlushFun, FlushCount, FlushTimeout),
    Buffer.
