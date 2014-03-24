package se.iix;

/**
 * Fibonacci
 * Prints out numbers from the fibonacci sequence
 */
public class Fibonacci {
  public static void main(String[] args) {
    /* If there's an argument, fib it!
     * else, run 20 fibs */
    if (args.length > 0) {
      System.out.println(fib(Integer.parseInt(args[0])));
    } else {
      for (int i = 0; i < 10; i++) {
        System.out.println(fib(i) + "\t" + fib(i+10));
      }
    }
  }

  public static int fib(int n) {
    int tmp;
    int old_value = 1;
    int new_value = 0;
    for (int i = 0; i < n; ++i) {
      tmp = old_value;
      old_value = new_value;
      new_value = tmp + old_value;
    }
    return new_value;
  }
}
