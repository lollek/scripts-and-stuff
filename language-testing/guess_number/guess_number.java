
import java.util.Random;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

class guess_number {
  public static void main(String[] args) {
    
    System.out.println("Guess-a-number game!");
    System.out.println("I am thinking of a number between 1 and 100");
    System.out.print("What's your guess? ");
    System.out.flush();
    
    int target = new Random().nextInt(100) + 1;
    int current = 0;
    BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

    while (current != target) {
      
      try {
        current = Integer.parseInt(stdin.readLine());
      } catch (IOException e) {}
      
      if (current < target)
        System.out.print("Too Low! Try again: ");
      else if (current > target)
        System.out.print("Too High! Try again: ");
      else {
        System.out.println("Correct! You won!");
        return;
      }
      System.out.flush();
    }
  }
}

/* TAIL INFO:
Name: Guess Number
Language: Java
Compile: javac guess_number.java
State: Done

Play guess-a-number game


Example: java guess_number
*/
