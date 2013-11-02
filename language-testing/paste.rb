#! /usr/bin/env ruby

require 'optparse'

def paste(filename1, filename2, delim)
  file1 = open(filename1)
  file2 = open(filename2)

  while 1
    line1 = file1.gets
    line2 = file2.gets

    if line1 or line2 then
      print line1.chomp if line1
      print delim
      print line2 if line2

    else
      file1.close
      file2.close
      break
    end
  end
end

delim = " "
OptionParser.new do |opts|
  opts.banner = "Usage: #{$0} file1 file2 [-d DELIM]"
  opts.on("-d DELIM", "delimiter to separate lines") {|d| delim = d}
end.parse!

if ARGV.length == 2 then
  paste ARGV[0], ARGV[1], delim
else
  puts "Bad arguments, see #{$0} -h"
end

=begin TAIL INFO
Name: Paste
Language: Ruby
State: Done

This is an answer to a python-test question at my school, basically:
It should take two files as argument and paste them with delim in between
It should be possible to change the delimiter with -d
The files must be read line-by-line
Example: ./paste.rb A.TXT B.TXT -d1
=end
