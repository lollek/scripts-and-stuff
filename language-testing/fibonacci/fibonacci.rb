#! /usr/bin/env ruby

if ARGV.length == 1 && ARGV[0] == "test"
  require 'test/unit'
  class TestClass < Test::Unit::TestCase
    def testfun
      assert_equal(fib(0), 0)
      assert_equal(fib(1), 1)
      assert_equal(fib(2), 1)
      assert_equal(fib(42), 267914296)
    end
  end
end

def fib(rot)
  old_value = 1
  new_value = 0
  rot.times { old_value, new_value = new_value, new_value + old_value }
  new_value
end

# If no arguments; run 20 fibs, else; fib the first argument:
if ARGV.length == 0
  (0...10).each { |i| puts "#{fib(i)}\t#{fib(i + 10)}" }
else
  puts(fib(ARGV[0].to_i))
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
