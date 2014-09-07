import java.io.*;

public class Rot13 {
  public static void main(String[] args) throws IOException {

    /* join and rot args */
    if (args.length > 0) {
      StringBuilder sb = new StringBuilder();
      sb.append(args[0]);
      for (int i = 1; i < args.length; ++i) {
        sb.append(" ");
        sb.append(args[i]);
      }
      rot13(sb.toString());

    /* rot for stdin */
    } else {
      BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
      String line;
      while ((line = in.readLine()) != null) {
        rot13(line);
      }
    }
  }

  public static void rot13(String string) {
    char[] cstr = string.toCharArray();
    for (int i = 0; i < cstr.length; ++i) {
      if      ('a' <= cstr[i] && cstr[i] <= 'm') { cstr[i] += 13; }
      else if ('n' <= cstr[i] && cstr[i] <= 'z') { cstr[i] -= 13; }
      else if ('A' <= cstr[i] && cstr[i] <= 'M') { cstr[i] += 13; }
      else if ('N' <= cstr[i] && cstr[i] <= 'Z') { cstr[i] -= 13; }
    }
    System.out.println(new String(cstr));
  }
}
