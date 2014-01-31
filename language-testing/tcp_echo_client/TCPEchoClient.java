import java.io.BufferedReader;
import java.io.BufferedOutputStream;
import java.io.InputStreamReader;
import java.io.IOException;

import java.net.Socket;
import java.net.UnknownHostException;

public class TCPEchoClient {

  String hostname;
  int port;

  Socket sock;
  BufferedReader stdin;
  BufferedOutputStream stdout;

  public TCPEchoClient (String hostname_, int port_) {
    hostname = hostname_;
    port = port_;
  }

  public void sendAndReceive(String data) {
    try {
      sock= new Socket(hostname, port);
      stdin = new BufferedReader(new InputStreamReader(sock.getInputStream()));
      stdout = new BufferedOutputStream(sock.getOutputStream());
      stdout.write(data.getBytes("UTF-8"));
      stdout.flush();
      while ((data = stdin.readLine()) != null)
        System.out.println(data);
      sock.close();

    } catch (UnknownHostException e) {
      System.out.println("Error :" + e.getMessage());
    } catch (IOException e) {
      System.out.println("Error : " + e.getMessage());
    }
  }

  public static void die(String error) {
    System.out.println("Usage: TCPEchoClient hostname port");
    System.out.println("Error: " + error);
    System.exit(1);
  }

  public static void main(String[] args) {
    switch (args.length) {
      case 0: die("No hostname given!"); break;
      case 1: die("No port given!"); break;
      case 2:
        byte[] data = new byte[1024];
        try {
          System.in.read(data);
        } catch (IOException e) {
          System.out.println("Error raised while reading from stdin!");
          System.exit(1);
        }
        int port = -1;
        try {
          port = Integer.parseInt(args[1]);
        } catch (java.lang.NumberFormatException e) {
          System.out.println("Error: Port contains non-digits!");
          System.exit(1);
        }
        TCPEchoClient s = new TCPEchoClient(args[0], port);
        s.sendAndReceive(new String(data));
        break;
      default: die("Too many arguments given!"); break;
    }
  }
}
