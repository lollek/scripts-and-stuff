#! /usr/bin/env ruby

def rome_from_int(int)

  rome = ""

  while int >= 1000 do rome += "M"; int -= 1000 end
  while int >= 900 do rome += "CM"; int -= 900 end
  while int >= 500 do rome += "D"; int -= 500 end
  while int >= 400 do rome += "CD"; int -= 400 end
  while int >= 100 do rome += "C"; int -= 100 end
  while int >= 90 do rome += "XC"; int -= 90 end
  while int >= 50 do rome += "L"; int -= 50 end
  while int >= 40 do rome += "XL"; int -= 40 end
  while int >= 10 do rome += "X"; int -= 10 end
  while int >= 9 do rome += "IX"; int -= 9 end
  while int >= 5 do rome += "V"; int -= 5 end
  while int >= 4 do rome += "IV"; int -= 4 end
  while int >= 1 do rome += "I"; int -= 1 end

  return rome
end

def int_from_rome(rome)

  int = 0
  tmp = 0
  
  rome.each_char.each do |z|
    case z
    when 'M'
      if tmp == 100 then int += 900; tmp = 0
      else int += (tmp + 1000); tmp = 0
      end
    when 'D'
      if tmp == 100 then int += 400; tmp = 0
      else int += (tmp + 500); tmp = 0
      end
    when 'C'
      if tmp == 10 then int += 90; tmp = 0
      else tmp += 100
      end
    when 'L'
      if tmp == 10 then int += 40; tmp = 0
      else int += (tmp + 50); tmp = 0
      end
    when 'X'
      if tmp == 1 then int += 9;  tmp = 0
      else tmp += 10
      end
    when 'V'
      if tmp == 1 then int += 4; tmp = 0
      else int += (tmp + 5); tmp = 0
      end
    when 'I' then tmp += 1
    end
  end

  int += tmp

  return int
end


if ARGV.length == 3
  if "0"[0] <= ARGV[1][0] && ARGV[1][0] <= "9"[0]
    arg1 = ARGV[1].to_i
  else
    arg1 = int_from_rome(ARGV[1])
  end
  if "0"[0] <= ARGV[2][0] && ARGV[2][0] <= "9"[0]
    arg2 = ARGV[2].to_i
  else
    arg2 = int_from_rome(ARGV[2])
  end
  
  case ARGV[0]
  when "+"
    puts "#{rome_from_int(arg1)} + #{rome_from_int(arg2)} = #{rome_from_int(arg1 + arg2)}"
    puts "#{arg1} + #{arg2} + #{arg1 + arg2}"
  when "-"
    puts "#{rome_from_int(arg1)} - #{rome_from_int(arg2)} = #{rome_from_int(arg1 - arg2)}"
    puts "#{arg1} - #{arg2} = #{arg1 - arg2}"
  end
end

=begin
Name: Roman Math
Language: Ruby
State: Done w/ bugs

Do addition / subtraction in decimal and roman numbers and display the result in both


Example: ./roman_math.rb + XVI IV
Example2: ./roman_math.rb + 16 4
=end
