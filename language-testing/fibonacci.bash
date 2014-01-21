#! /bin/bash

fib() {
  old_val=1
  new_val=0
  tmp=0

  for ((i=0; i < $1; i++)); do
    tmp=$new_val
    new_val=$(( old_val + new_val ))
    old_val=$tmp
  done
  echo $new_val
}

if [[ -z $1 ]]; then
  range="$(seq 1 20)"
else
  range="$1"
fi

for num in $range; do
  fib "$num"
done
    
## Tail info:
# Name: Fibonacci Sequence
# Language: Python3
# State: Done
# 
# Prints out numbers from the fibonacci sequence
# 
# 
# Example: ./fibonacci.py
# Example2: ./fibonacci.py 42
# 
