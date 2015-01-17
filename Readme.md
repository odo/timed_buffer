# timed_buffer

timed_buffer is an Erlang gen_server that accumlutes some values
and then sends them off to a function after a certain time has passed
or a certain number of elements are in the buffer.

This is useful if you want to forward data to other parts of your application
in batches but want to have an maximum time any piece stays in the buffer.

## Installation

```
git clone git@github.com:odo/timed_buffer.git
./rebar get com
```

# Example Usage

We are creating a buffer that flushes after 5 elements or after 1000 ms.

`erl -pz ebin`

```erlang
1> FlushFun = fun(Elements) -> io:format("flush with ~p\n", [Elements]) end.
#Fun<erl_eval.6.90072148>
2> {ok, Buffer} = timed_buffer:start_link(FlushFun, 5, 1000).
{ok,<0.35.0>}
3> [timed_buffer:add(x, Buffer) || _<-lists:seq(1, 5)].
flush with [x,x,x,x,x]
[ok,ok,ok,ok,ok]
4> [timed_buffer:add(x, Buffer) || _<-lists:seq(1, 6)].
flush with [x,x,x,x,x]
[ok,ok,ok,ok,ok,ok]
--- one second passes ---
flush with [x]
```
