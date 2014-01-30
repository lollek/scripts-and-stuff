#! /usr/bin/env ruby

require 'socket'

def send_receive_data hostname, port, content
  TCPSocket.open hostname, port do |socket|
    socket.puts content
    puts socket.gets
  end
end

def fail msg
  $stderr.puts "Usage: #{$0} hostname port\nError: #{msg}" 
  exit 1
end

if __FILE__ == $0
  hostname = nil
  port = nil

  ARGV.each do |arg|
    if not hostname
      hostname = arg
    elsif not port
      fail "Port contains non-numbers!" if not /^\d+$/ =~ arg
      port = arg.to_i
    else
      fail "Too many arguments!"
    end
  end

  fail "No hostname given!" if not hostname
  fail "No port given!" if not port
  send_receive_data hostname, port, $stdin.read
end
