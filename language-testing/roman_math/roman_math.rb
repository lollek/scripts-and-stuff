#! /usr/bin/env ruby

def rome_from_int(dec)
  rome = ''

  while dec >= 1000 do rome << 'M'  && dec -= 1000 end
  while dec >= 900  do rome << 'CM' && dec -= 900 end
  while dec >= 500  do rome << 'D'  && dec -= 500 end
  while dec >= 400  do rome << 'CD' && dec -= 400 end
  while dec >= 100  do rome << 'C'  && dec -= 100 end
  while dec >= 90   do rome << 'XC' && dec -= 90 end
  while dec >= 50   do rome << 'L'  && dec -= 50 end
  while dec >= 40   do rome << 'XL' && dec -= 40 end
  while dec >= 10   do rome << 'X'  && dec -= 10 end
  while dec >= 9    do rome << 'IX' && dec -= 9 end
  while dec >= 5    do rome << 'V'  && dec -= 5 end
  while dec >= 4    do rome << 'IV' && dec -= 4 end
  while dec >= 1    do rome << 'I'  && dec -= 1 end

  rome
end

def int_from_rome(rome)
  results = 0

  (0...rome.size).each do |i|
    case rome[i]
    when 'M' then results += 1000
    when 'D' then results += 500
    when 'C' then results += (%w'M D'.member? rome[i + 1]) ? -100 : 100
    when 'L' then results += 50
    when 'X' then results += (%w'C L'.member? rome[i + 1]) ? -10 : 10
    when 'V' then results += 5
    when 'I' then results += (%w'X V'.member? rome[i + 1]) ? -1 : 1
    else
      $stderr.puts "Bad character found (#{ rome[i] })! Exiting.."
      exit 1
    end
  end
  results
end

def usage
  $stderr.puts "Usage: #{ $PROGRAM_NAME } command number number\n\n" \
               "Example usage: #{ $PROGRAM_NAME } + XVI IV\n" \
               "Example usage: #{ $PROGRAM_NAME } - VI IV"
  exit 1
end

if __FILE__ == $PROGRAM_NAME
  if ARGV.length == 3
    cmd = ARGV[0]
    arg1 = ARGV[1] =~ /^\d+$/ ? ARGV[1].to_i : int_from_rome(ARGV[1])
    arg2 = ARGV[2] =~ /^\d+$/ ? ARGV[2].to_i : int_from_rome(ARGV[2])

    case cmd
    when '-' then results = arg1 - arg2
    when '+' then results = arg1 + arg2
    else          usage
    end

    puts "#{ rome_from_int arg1 } #{ cmd } #{ rome_from_int arg2 }" \
         " = #{ rome_from_int results }\n" \
         "#{ arg1 } #{ cmd } #{ arg2 } = #{ results }"
  else
    usage
  end
end
