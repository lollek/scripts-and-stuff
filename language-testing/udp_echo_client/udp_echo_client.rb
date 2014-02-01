#! /usr/bin/env ruby

require 'socket'

def send_receive_data hostname, port, content
  UDPSocket.open do |sock|
    sock.send content, 0, hostname, port
    data, _ = sock.recvfrom(1024)
    puts data
  end
end

def die msg
  $stderr.puts "Usage: #{$0} hostname port\nError: #{msg}" 
  exit 1
end

if __FILE__ == $0
  die "No hostname given!" unless ARGV.length > 0
  die "No port given!" unless ARGV.length > 1
  die "Port contains non-numbers!" unless /^\d+$/ =~ ARGV[1]
  die "Too many arguments given!" unless ARGV.length == 2
  send_receive_data ARGV[0], ARGV[1].to_i, $stdin.read
end
