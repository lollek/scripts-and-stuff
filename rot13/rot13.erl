-module(rot13).
-export([rot13/1]).

rot([]) ->
    io:format(" ");
rot([Letter|Tail]) ->
    if
        $a =< Letter, Letter =< $m -> io:format("~c", [Letter+13]);
        $n =< Letter, Letter =< $z -> io:format("~c", [Letter-13]);
        $A =< Letter, Letter =< $M -> io:format("~c", [Letter+13]);
        $N =< Letter, Letter =< $Z -> io:format("~c", [Letter-13]);
        true -> io:format("~c", [Letter])
    end,
    rot(Tail).

rot13([]) -> 
    io:format("~n"),
    init:stop();
rot13([Arg|Tail]) ->
    rot(Arg),
    rot13(Tail).

% TAIL INFO:
% Name: ROT13
% Language: Erlang
% Compile: erlc rot13.erl
% State: Done
%
% Rotates a string
%
%
% Example: erl -noshell -run rot13 rot13 Hello World \!
%
