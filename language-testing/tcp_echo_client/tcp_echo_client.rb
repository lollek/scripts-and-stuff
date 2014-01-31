#! /usr/bin/env ruby

require 'socket'

def send_receive_data hostname, port, content
  TCPSocket.open hostname, port do |socket|
    socket.puts content
    puts socket.gets
  end
end

def die msg
  $stderr.puts "Usage: #{$0} hostname port\nError: #{msg}" 
  exit 1
end

if __FILE__ == $0
  case ARGV.length
  when 0 then die "No hostname given!"
  when 1 then die "No port given!"
  when 2 then
    die "Port contains non-numbers!" unless /^\d+$/ =~ ARGV[1]
    send_receive_data ARGV[0], ARGV[1].to_i, $stdin.read
  else die "Too many arguments given!"
  end
end
