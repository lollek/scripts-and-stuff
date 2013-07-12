
public class fibonacci {
  public static void main(String[] args) {
    
    int i;

    /* If there's an argument, fib it! */
    if (args.length == 1) {
      fib(Integer.parseInt(args[0]));
      System.out.println();
      return;
    }

    /* Else, run 20 fibs */
    for (i = 0; i < 10; i++) {
      fib(i);
      System.out.print("\t");
      fib(i+10);
      System.out.println();
    }
    return;
  }

  private static void fib(int rotations) {

    int tmp;
    int old_value = 1;
    int new_value = 0;

    while (rotations > 0) {
      tmp = old_value;
      old_value = new_value;
      new_value = tmp + old_value;
      rotations -= 1;
    }
    
    System.out.print(new_value);
  }
}

/* TAIL INFO:
Name: Fibonacci Sequence
Language: Java
Compile: javac fibonacci.java
State: Done

Prints out numbers from the fibonacci sequence

Example: java fibonacci
Example2: java fibonacci 42
*/
