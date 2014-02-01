#! /usr/bin/env ruby

require 'socket'

def start_echo_server port
  socky = UDPSocket.new
  socky.bind("", port)
  loop do
    data, client = socky.recvfrom 1024
    _, client_port, client_host, _ = client
    socky.send data, 0, client_host, client_port

    File.open "udp_echo_server.log", "a" do |f|
      f.puts data
    end
  end
end

def die error
  $stderr.puts "Usage: #{$0} port\n#{error}"
  exit 1
end

if __FILE__ == $0
  die "No port given!" unless ARGV.length > 0
  die "Error: Port contains non-digits!" unless ARGV[0] =~ /^\d+$/
  start_echo_server ARGV[0].to_i
end
