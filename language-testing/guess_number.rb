#! /usr/bin/env ruby

$stdout.sync = true

target_number = rand(100) + 1
try_num = 0

puts "Guess-a-number game!"
puts "I am thinking of a number between 1 and 100. "
puts "You have 5 tries to guess it correctly or I win."
puts "What\'s your guess? "


while 1 do
  
  print "Guess #{try_num += 1}: "
  attempt = gets.to_i
  
  if attempt == target_number
    puts "Correct! You won!"
    break
  elsif try_num == 5
    puts "Haha, I won! The number was #{target_number}"
    break
  elsif attempt > target_number
    puts "Too high! Try again!"
  elsif attempt < target_number
    puts "Too low! Try again!"
  end

end

=begin TAIL INFO:
Name: Guess Number
Language: Ruby
State: Done

Play guess-a-number game



Example: ./guess_number.rb
=end
