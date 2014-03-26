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
  echo -n $new_val
}

if [[ -z $1 ]]; then
  for ((r=0; r < 10; ++r)); do
    fib $r
    echo -en "\t"
    fib $((r + 10))
    echo
  done
else
  fib $1
  echo
fi
