-module(fibonacci).
-export([fib/1]).

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) ->
    fib(N-1) + fib(N-2).

% TAIL INFO:
% Name: Fibonacci Sequence
% Language: Erlang
% Compile: c(fibonacci).
% State: Done
%
% Prints out numbers from the fibonacci sequence
%
%
% Example: fibonacci:fib(42).
%
