#! /usr/bin/env ruby

require 'test/unit'
require '../fibonacci.rb'

class TestClass < Test::Unit::TestCase
  def testfun
    assert_equal(fib(0), 0)
    assert_equal(fib(1), 1)
    assert_equal(fib(2), 1)
    assert_equal(fib(42), 267914296)
  end
end
