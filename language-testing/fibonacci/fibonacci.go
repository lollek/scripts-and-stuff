package main

import (
  "os"
  "fmt"
  "strconv"
  "log"
)

func fib (rot int) int {
  old_value := 1
  new_value := 0

  for i := 0; i < rot; i++ {
    old_value, new_value = new_value, new_value + old_value
  }

  return new_value
}

func main() {
  if len(os.Args) == 2 {
    i, err := strconv.Atoi(os.Args[1])
    if err != nil {
      log.Fatal(err);
    }
    fmt.Printf("%d\n", fib(i))

  } else {
    for i := 0; i < 10; i++ {
      fmt.Printf("%d\t%d\n", fib(i), fib(i + 10))
    }
  }
}
