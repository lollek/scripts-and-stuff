#! /usr/bin/env lua

math.randomseed(os.time())
local solution = math.floor(math.random(1, 100))

print [[
Guess-a-number game!
I'm thinking of a number between 1 and 100,
you have 5 tries to guess it correctly.
What's your guess?
]]

local guess, guess_num = 0, 0
while 1 do
  guess_num = guess_num + 1
  print(string.format("Guess %d: ", guess_num))
  guess = io.read("*number")

  if guess < solution then
    print "Too low! Try again!"
  elseif guess > solution then
    print "Too high! Try again!"
  elseif guess == solution then
    print "Correct! You won!"
    os.exit()
  end

  if guess_num > 4 then
    print(string.format("Haha, I won! The number was %d!", solution))
    os.exit()
  end
end

--[[ TAIL INFO:
Name: Guess Number
Language: Python3
State: Done

Play guess-a-number game



Example ./guess_number.py
]]
