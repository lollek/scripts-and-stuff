#! /usr/bin/env ruby

def fib(rot)
  
  old_value = 1
  new_value = 0

  while rot > 0 do
    old_value, new_value = new_value, new_value + old_value
    rot -= 1
  end

  return new_value
  
end

# If no arguments, run 20 fibs:
if ARGV.length == 0 then
  (0...10).each {|i| puts "#{fib(i)}\t#{fib(i + 10)}"}

# Else, fib the first argument:
else
  puts fib(ARGV[0].to_i)

end

=begin
Name: Fibonacci Sequence
Language: Ruby
State: Done

Prints out numbers from the fibonacci sequence


Example: ./fibonacci.rb
Example2: ./fibonacci.rb 42
=end
