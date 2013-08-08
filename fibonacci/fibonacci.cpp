#include <iostream>
#include <cstdlib>

unsigned long fib (int i) 
{
  unsigned long oldval = 1;
  unsigned long newval = 0;
  unsigned long tmp;

  while (--i >= 0)
  {
    tmp = oldval;
    oldval = newval;
    newval = tmp + oldval;
  }
  return newval;
}

int main (int argc, char **argv) 
{

  // Fib the first argument, if possible
  if (argc == 2) 
  {
    std::cout << fib(atoi(argv[1])) << std::endl;
    return 0;
  }

  // Otherwise, run 20 fibs
  for (int i = 0; i < 10; i++)
    std::cout << fib(i) << "\t" << fib(i+10) << std::endl;
  return 0;
}

// TAIL INFO:
// Name: Fibonacci Sequence
// Language: C++
// Compile: g++ fibonacci.cpp -o fibonacci_cpp
// State: Done
// Created: 2013-08-08
//
// Prints out numbers from the fibonacci sequence
// 
// Example1: ./fibonacci_cpp
// Example2: ./fibonacci_cpp 42
