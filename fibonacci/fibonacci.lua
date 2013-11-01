#! /usr/bin/env lua

function fib(rotations)
  local old_value = 1
  local new_value = 0

  for i = rotations, 0, -1 do
    new_value, old_value = new_value + old_value, new_value
  end

  return new_value
end

if #arg == 1 then
  print(fib(arg[1]))
else
  for i = 1,10 do
    print(fib(i) .. "\t" .. fib(i+10))
  end
end
