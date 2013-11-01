#! /bin/sh

fib() {
    old_val=1
    new_val=0
    tmp=0
    r=$1

    while [ $r -gt 0 ]; do
        tmp=$new_val
        new_val=$(( old_val + new_val ))
        old_val=$tmp
        r=$(( $r - 1 ))
    done
    echo $new_val
}

if [ -z $1 ]; then
    for i in `seq 1 20`; do fib $i; done
else
    fib $1
fi
    
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
