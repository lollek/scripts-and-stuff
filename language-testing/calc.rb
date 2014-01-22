#! /usr/bin/env ruby
# Postfix calculator
# E.g. `./calc.rb 1 2 +` gives 3
# x is used instead of * because of bash

class String
  def isdigit?
    self =~ /[[:digit:]]+/
  end
end

stack = []

ARGV.each do |sym|
  if sym.isdigit?
    stack.push sym.to_i
  elsif stack.length < 2
    puts "Error! Stack is too small! (#{stack.length})"
    exit 1
  elsif sym == 'x' 
    stack.push (stack.pop * stack.pop)
  elsif sym == '/' 
    tmp = stack.pop
    stack.push (stack.pop / tmp)
  elsif sym == '+' 
    stack.push (stack.pop + stack.pop)
  elsif sym == '-' 
    tmp = stack.pop
    stack.push (stack.pop - tmp)
  else
    puts "Error! Unknown symbol"
    exit 1
  end
end

if stack.length > 1
  puts "Ignored #{stack.length() -1} trailing characters: #{stack[1..-1]}"
end
puts stack[0]
