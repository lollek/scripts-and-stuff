#! /usr/bin/env ruby

def rot13(sentence)
  puts sentence.tr("A-Za-z", "N-ZA-Mn-za-m")
end

# No arguments means a pipe:
if ARGV.length == 0 
then $stdin.each {|x| rot13 x}
# Otherwise, we'll rot13 the arguments:
else rot13(ARGV.join(" "))
end

=begin TAIL INFO:
Name ROT13
Language: Ruby
State: Done

Rot13 a string


Example: ./rot13.rb hello world
Example2: echo "Hello world" | ./rot13.rb
=end
