#! /usr/bin/env ruby

require 'socket'

def start_echo_server port
  socky = TCPServer.new port
  loop do
    client = socky.accept
    data = client.gets
    client.puts data
    client.close

    File.open "tcp_echo_server.log", "a" do |f|
      f.puts data
    end
  end
end

if __FILE__ == $0
  if ARGV.length == 1
    if ARGV[0] =~ /^\d+$/
      start_echo_server ARGV[0].to_i
    else
      $stderr.puts "Usage: #{$0} port"
      $stderr.puts "Error: Port contains non-digits!"
      exit 1
    end
  else
    $stderr.puts "Usage: #{$0} port"
    exit 1
  end
end
