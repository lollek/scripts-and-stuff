#! /usr/bin/env ruby
# This application replaces all words with hodor.
# Usage: ./hodor.rb 'hello world!'
# Usage: echo "hello world!" | ./hodor.rb

def hodor(str)
  str.gsub(/[a-zA-Z0-9]+/, 'hodor')
end

if ARGV.length > 0
  puts hodor(ARGV.join(' '))
else
  $stdin.each { |line| puts hodor(line) }
end
