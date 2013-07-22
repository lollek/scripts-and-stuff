-module(guess_number).
-export([guess_number/0]).

guess_number(Solution, Num) ->
    io:format("Guess ~w: ", [Num]),
    {ok, [Guess]} = io:fread("", "~d"),

    if
        Guess == Solution ->
            io:fwrite("Correct! You have won!\n"),
            init:stop();
        Num == 5 ->
            io:format("Haha, I won! The number was ~w~n", [Solution]),
            init:stop();
        Guess > Solution ->
            io:fwrite("Too high! Try again!\n"),
            guess_number(Solution, Num + 1);
        true -> % Guess < Solution
            io:fwrite("Too low! Try again!\n"),
            guess_number(Solution, Num + 1)
    end.

guess_number() ->
    random:seed(now()),
    Solution = random:uniform(100),
    
    io:fwrite("Guess-a-number game!\n"),
    io:fwrite("I am thinking of a number between 1 and 100.\n"),
    io:fwrite("You have 5 tries to guess it correctly or I win.\n"),
    io:fwrite("What's your guess?\n"),
    guess_number(Solution, 1).

    
% TAIL INFO:
% Name: Guess Number
% Language: Erlang
% Compile: erlc guess_number.erl
% State: Done
%
% Play guess-a-number game
%
%
% Example: erl -noshell -run guess_number guess_number
%
