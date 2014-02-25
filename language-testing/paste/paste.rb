#! /usr/bin/env ruby


def paste(filename1, filename2, delim)
  file1 = open(filename1)
  file2 = open(filename2)

  while 1
    line1 = file1.gets
    line2 = file2.gets

    if line1 || line2
      print line1.chomp if line1
      print delim
      print line2.chomp if line2
      puts
    else
      file1.close
      file2.close
      break
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require 'optparse'

  delim = ' '
  OptionParser.new do |opts|
    opts.banner = "Usage: #{$PROGRAM_NAME} file1 file2 [-d DELIM]"
    opts.on('-d DELIM' , 'delimiter to separate lines') { |d| delim = d }
  end.parse!

  if ARGV.length == 2
    paste ARGV[0], ARGV[1], delim
  else
    puts "Bad arguments, see #{$PROGRAM_NAME} -h"
  end
end

# Name: Paste
# Language: Ruby
# State: Done
#
# This is an answer to a python-test question at my school, basically:
# It should take two files as argument and paste them with delim in between
# It should be possible to change the delimiter with -d
# The files must be read line-by-line
# Example: ./paste.rb A.TXT B.TXT -d1
