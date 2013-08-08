with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Command_Line;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Command_Line;

procedure Fibonacci is
   
   function GetArg return Positive is
   begin
      return Positive'Value (Argument (1));
   end GetArg;
   
   function Fib (N : Natural) return Natural is
      Oldval : Natural := 1;
      Newval : Natural := 0;
      Tmp    : Natural;
   begin
      for I in 1..N loop
         Tmp := Oldval;
         Oldval := Newval;
         Newval := Tmp + Oldval;
      end loop;
      return Newval;
   end Fib;
   
begin
   if Argument_Count = 1 then
      Put ( Fib ( GetArg));
   else
        for I in 0..9 loop
           Put (Fib(I));
           Put (Fib(I+10));
           New_Line;
        end loop;
   end if;
end Fibonacci;
