#! /usr/bin/env ruby

$stdout.sync = true

def fib(rot)
  
  old_value = 1
  new_value = 0

  while rot > 0 do
    old_value, new_value = new_value, new_value + old_value
    rot -= 1
  end

  print new_value
  
end

# If no arguments, run 20 fibs:
if ARGV.length == 0 then
  (0...10).each {|i| fib(i); print "\t"; fib(i + 10); print "\n"}

# Else, fib the first argument:
else
  fib(ARGV[0].to_i)
  puts
end

=begin
Name: Fibonacci Sequence
Language: Ruby
State: Done

Prints out numbers from the fibonacci sequence


Example: ./fibonacci.rb
Example2: ./fibonacci.rb 42
=end
