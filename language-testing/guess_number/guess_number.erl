-module(main).
-export([main/1]).

guess(Solution, Num) ->
  io:format("Guess ~w: ", [Num]),
  {ok, [Guess]} = io:fread("", "~d"),
  case Guess of
    Solution ->
      io:fwrite("Correct! You have won!\n"),
      ok;
    Wrong when Num == 5 ->
      io:fwrite("Haha, I won! The number was ~w~n", [Solution]),
      ok;
    Wrong when Wrong > Solution ->
      io:fwrite("Too high! Try again!\n"),
      guess(Solution, Num +1);
    Wrong when Wrong < Solution ->
      io:fwrite("Too low! Try again!\n"),
      guess(Solution, Num +1)
  end.

main(_) ->
  random:seed(now()),
  Solution = random:uniform(100),

  io:fwrite("Guess-a-number game!~n"
            "I am thinking of a number between 1 and 100.~n"
            "You have 5 tries to guess it correctly or I win.~n"
            "What's your guess?~n"),
  guess(Solution, 1).

% TAIL INFO:
% Name: Guess Number
% Language: Erlang
% State: Done
%
% Play guess-a-number game
%
%
% Example: escript guess_number.erl
%
