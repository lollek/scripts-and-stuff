package test;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

import se.iix.Fibonacci;

public class FibonacciTest {

  @Test
  public void testfun() {
    assertEquals("0", Fibonacci.fib(0), 0);
    assertEquals("1", Fibonacci.fib(1), 1);
    assertEquals("2", Fibonacci.fib(2), 1);
    assertEquals("42", Fibonacci.fib(42), 267914296);
  }

}
