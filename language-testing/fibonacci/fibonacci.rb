#! /usr/bin/env ruby

def fib(rot)
  old_value = 1
  new_value = 0

  while rot > 0
    old_value, new_value = new_value, new_value + old_value
    rot -= 1
  end

  new_value
end

# If no arguments; run 20 fibs, else; fib the first argument:
if ARGV.length == 0 then (0...10).each { |i| puts "#{fib(i)}\t#{fib(i + 10)}" }
else                     puts fib(ARGV[0].to_i)
end

# Name: Fibonacci Sequence
# Language: Ruby
# State: Done
#
# Prints out numbers from the fibonacci sequence
#
#
# Example: ./fibonacci.rb
# Example2: ./fibonacci.rb 42
