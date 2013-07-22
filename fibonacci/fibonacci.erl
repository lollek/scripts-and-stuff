-module(fibonacci).
-export([fib/1]).

fib([X, Y, 0]) ->
    [X, Y, 0];
fib([X, Y, Z]) ->
    fib([Y, Y + X, Z - 1]);
fib(N) ->
    [X, _, _] = fib([0, 1, N]),
    X.

% TAIL INFO:
% Name: Fibonacci Sequence
% Language: Erlang
% Compile: c(fibonacci). / erlc fibonacci.erl
% State: Done
%
% Prints out numbers from the fibonacci sequence
%
%
% Example: fibonacci:fib(42).
%
