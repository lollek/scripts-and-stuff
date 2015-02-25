-module(main).
-export([main/1]).

main([Arg1|[]]) ->
  {N, _} = string:to_integer(Arg1),
  io:fwrite("~w~n", [fib(N)]);
main([]) ->
  fib_print(0).

fib_print(N) when N >= 10 -> ok;
fib_print(N) when N >= 0 ->
  io:fwrite("~w~c~w~n", [fib(N), 9, fib(N+10)]),
  fib_print(N+1).

fib([X, Y, 0]) -> [X, Y, 0];
fib([X, Y, Z]) -> fib([Y, Y + X, Z - 1]);
fib(N) ->
  [X, _, _] = fib([0, 1, N]),
  X.

% TAIL INFO:
% Name: Fibonacci Sequence
% Language: Erlang
% State: Done
%
% Prints out numbers from the fibonacci sequence
%
%
% Example: escript fibonacci.erl 42
%
