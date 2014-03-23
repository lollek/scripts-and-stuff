#! /bin/bash
set -e

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

test_fib() {
  echo "Running test-cases"
  [[ $(fib 0) == 0 ]] || (echo "Error @ fib 0" && exit 1)
  [[ $(fib 1) == 1 ]] || (echo "Error @ fib 0" && exit 1)
  [[ $(fib 2) == 1 ]] || (echo "Error @ fib 0" && exit 1)
  [[ $(fib 42) == 267914296 ]] || (echo "Error @ fib 0" && exit 1)
  echo "Success!"
}

if [[ -z $1 ]]; then
  for ((r=0; r < 10; ++r)); do
    fib $r
    echo -en "\t"
    fib $((r + 10))
    echo
  done
elif [[ $1 == "test" ]]; then
  test_fib
else
  fib $1
fi
