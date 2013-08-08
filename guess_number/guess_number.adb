with 
  Ada.Text_IO, 
  Ada.Integer_Text_IO, 
  Ada.Numerics.Discrete_Random;

use 
  Ada.Text_IO, 
  Ada.Integer_Text_IO;

procedure Guess_Number is
   
   --declare
   type Rand_Range is range 1..100;
   package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
   Seed : Rand_Int.Generator;
   Solution, Target, Attempt : Integer;
   
begin
   
   Put_Line ("Guess-a-number game!");
   Put_Line ("I am thinking of a number between 1 and 100\n");
   Put_Line ("You have 5 tries to guess it correctly or I win\n");
   Put_Line ("What's your guess?");
   
   Rand_Int.Reset(seed);
   Solution := Integer'Value(Rand_Range'Image(Rand_Int.Random(Seed)));
   Target := 0;
   Attempt := 0;
   
   while (Solution /= Target) loop
      Attempt := Attempt + 1;
      Put ("Guess " & Integer'Image(Attempt) & ": ");
      Get (Target);
      
      if Solution = Target then
         Put_Line ("Correct! You have won!");
         exit;
         
      elsif Attempt = 5 then
         Put_Line ("Haha, I won! The number was " & Integer'Image(Solution));
         exit;
         
      elsif Target > Solution then
         Put_Line ("Too high! Try again!");
         
      elsif Target < Solution then
         Put_Line ("Too low! Try again!");
         
      end if;
      
   end loop;
   
end Guess_Number;

-- TAIL INFO:
-- Name: Guess Number
-- Language: Ada
-- Compile: gnatmake guess_number.adb
-- State: Done
-- Created: 2013-08-08
--
-- Play guess-a-number game
--
-- Example: ./guess_number
--
