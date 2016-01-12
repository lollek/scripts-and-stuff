#! /usr/bin/env ruby

require 'cgi'
require 'socket'
require 'json'

def shellcheck_build_request(shellcode)
  return nil if shellcode.length == 0
  contents = "script=#{CGI.escape shellcode}"

  "POST /shellcheck.php HTTP/1.1\r\n" \
  "Host: www.shellcheck.net\r\n" \
  "Connection: close\r\n" \
  "Content-Length: #{contents.size}\r\n" \
  "Content-Type: application/x-www-form-urlencoded\r\n" \
  'User-Agent: ShellCheck ' \
  "(https://github.com/lollek/scripts-and-stuff)\r\n\r\n" +
  contents
end

def shellcheck_output(text, level, indentation)
  color = { 'error' => "\033[1;31m",
            'warning' => "\033[1;33m",
            'info' => "\033[1;36m",
            'normal' => "\033[0m" }
  puts color[level] + ' ' * (indentation - 1) + "^-- #{text}" + color['normal']
end

def shellcheck_explain(shellcode, data)
  current_line = 0

  data.each do |issue|
    while issue['line'] > current_line
      puts shellcode.shift
      current_line += 1
    end
    shellcheck_output issue['message'], issue['level'], issue['column']
  end

  puts shellcode.join("\n") unless shellcode.empty?
end

def shellcheck(shellcode)
  request = shellcheck_build_request shellcode
  return if request.nil?

  returned_data =
    TCPSocket.open 'shellcheck.net', 80 do |socket|
      socket.puts request
      # We want the line after the \r\n\r\n
      while socket.gets != "\r\n"; end
      socket.gets
    end

  shellcheck_explain shellcode.split("\n"), JSON.parse(returned_data)
end

if __FILE__ == $PROGRAM_NAME
  if ARGV.length == 0
    shellcheck $stdin.read
  elsif ARGV.length == 1 && ARGV[0][0] != '-'
    begin
      shellcheck File.read(ARGV[0])
    rescue => e
      $stderr.puts e
    end
  else
    $stderr.puts "Usage: #{$PROGRAM_NAME} FILE\n\n" \
                 'If no FILE is given, data is read from stdin'
  end
end
