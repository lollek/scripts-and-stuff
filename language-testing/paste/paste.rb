#! /usr/bin/env ruby


def paste(filename1, filename2, delim)
  File.open filename1 do |file1|
    File.open filename2 do |file2|
      line1, line2 = file1.gets, file2.gets
      while line1 || line2
        puts "#{line1.chomp if line1}#{delim}#{line2.chomp if line2}"
        line1, line2 = file1.gets, file2.gets
      end
    end
  end
end

def usage
  $stderr.puts "Bad arguments, see #{$PROGRAM_NAME} -h"
  exit 1
end

if __FILE__ == $PROGRAM_NAME
  require 'optparse'

  delim = ' '
  begin
    OptionParser.new do |opts|
      opts.banner = "Usage: #{$PROGRAM_NAME} file1 file2 [-d DELIM]"
      opts.on('-d DELIM' , 'delimiter to separate lines') { |d| delim = d }
    end.parse!
  rescue
    usage
  end

  usage unless ARGV.length == 2
  paste ARGV[0], ARGV[1], delim
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
