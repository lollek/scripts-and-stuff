#! /usr/bin/env ruby
# Prints out numbers from the fibonacci sequence
# Example usage: ./fibonacci.rb
# Example usage: ./fibonacci.rb 42

def fib(n)
  old_value = 1
  new_value = 0
  n.times { old_value, new_value = new_value, new_value + old_value }
  new_value
end

# If no arguments; run 20 fibs, else; fib the first argument:
if __FILE__ == $0
  if ARGV.length == 0
    (0...10).each { |i| puts "#{fib(i)}\t#{fib(i + 10)}" }
  else
    puts(fib(ARGV[0].to_i))
  end
end
