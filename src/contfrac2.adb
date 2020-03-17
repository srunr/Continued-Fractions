with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Extended_Real;
with Extended_Real.IO;
procedure Contfrac2 is

   type Real is digits 15;

   Start_time, End_time : Time;
   Exec_time : Time_Span;

   package Ext_Real is new Extended_Real(Real);
   use Ext_Real;
   package rio is new Ada.Text_IO.Float_IO (Real);
   use rio;
   package iio is new Ada.Text_IO.Integer_IO (E_Integer);
   use iio;
   package eio is new Ext_Real.IO;
   use eio;

   -- generic
   --    with function A (N : in Natural)  return e_Real;
   --    with function B (N : in Positive) return e_Real;
   -- function Continued_Fraction (Steps : in Natural) return e_Real;

   -- function Continued_Fraction (Steps : in Natural) return e_Real is
   --    function A (N : in Natural)  return e_Real is (Make_Extended(A(N)));
   --    function B (N : in Positive) return e_Real is (Make_Extended(B(N)));

   --    Fraction : e_Real := Make_Extended(0.0);
   -- begin
   --     for N in reverse Natural range 1 .. Steps loop
   --        Fraction := B(N) / A(N) + Fraction; -- Ada.Text_IO.Put("Fraction("& N'Image & ") = "); Ada.Text_IO.Put(e_Real_Image(Fraction)); Ada.Text_IO.New_line;
   --     end loop;
   --     return A(0) + Fraction;
   --  end Continued_Fraction;

   package Square_Root_Of_2 is
      function Continued_Fraction (Steps : in Natural) return e_Real;
   end Square_Root_Of_2;

   package body Square_Root_Of_2 is

      function Continued_Fraction (Steps : in Natural) return e_Real is
         function A (N : in Natural)  return e_Real is
         begin
            return Make_Extended((if N = 0 then 1.0 else 2.0));
         end A;

         function B (N : in Positive) return e_Real is
         begin
            return Make_Extended(1.0);
         end B;

         Fraction : e_Real := Make_Extended(0.0);
      begin
         for N in reverse Natural range 1 .. Steps loop
            Fraction := B(N) / (A(N) + Fraction); -- Ada.Text_IO.Put("Fraction("& N'Image & ") = "); Ada.Text_IO.Put(e_Real_Image(Fraction)); Ada.Text_IO.New_line;
         end loop;
         return A(0) + Fraction;
      end Continued_Fraction;

   end Square_Root_Of_2;


   package Napiers_Constant is
      function Continued_Fraction (Steps : in Natural) return e_Real;
   end Napiers_Constant;

   package body Napiers_Constant is
      -- function A (N : in Natural)  return e_Real is (if N = 0 then 2 else N);
      -- function B (N : in Positive) return e_Real is (if N = 1 then 1 else N-1);

      function Continued_Fraction (Steps : in Natural) return e_Real is
         function A (N : in Natural)  return e_Real is
         begin
            return Make_Extended((if N = 0 then 2.0 else Real(N)));
         end A;

         function B (N : in Positive) return e_Real is
         begin
            return Make_Extended((if N = 1 then 1.0 else Real(N-1)));
         end B;

         Fraction : e_Real := Make_Extended(0.0);
      begin
         for N in reverse Natural range 1 .. Steps loop
            Fraction := B(N) / (A(N) + Fraction); -- Ada.Text_IO.Put("Fraction("& N'Image & ") = "); Ada.Text_IO.Put(e_Real_Image(Fraction)); Ada.Text_IO.New_line;
         end loop;
         return A(0) + Fraction;
      end Continued_Fraction;

      -- function Estimate is new Continued_Fraction (A, B);
   end Napiers_Constant;

   package Pi is
      function Continued_Fraction (Steps : in Natural) return e_Real;
   end Pi;

   package body Pi is
      -- function A (N : in Natural)  return Natural is  (if N = 0 then 3 else 6);
      -- function B (N : in Positive) return Natural is ((2 * N - 1) ** 2);

      function Continued_Fraction (Steps : in Natural) return e_Real is
         function A (N : in Natural)  return e_Real is
         begin
            return Make_Extended((if N = 0 then 3.0 else 6.0));
         end A;

         function B (N : in Positive) return e_Real is
         begin
            return Make_Extended((2.0 * Real(N) - 1.0) ** 2);
         end B;

         Fraction : e_Real := Make_Extended(0.0);
      begin
         for N in reverse Natural range 1 .. Steps loop
            Fraction := B(N) / (A(N) + Fraction); -- Ada.Text_IO.Put("Fraction("& N'Image & ") = "); Ada.Text_IO.Put(e_Real_Image(Fraction)); Ada.Text_IO.New_line;
         end loop;
         return A(0) + Fraction;
      end Continued_Fraction;

      -- function Estimate is new Continued_Fraction (A, B);
   end Pi;

   -- package Scalar_Text_IO is new Ada.Text_IO.Float_IO (e_Real);

   use Ada.Text_IO;


begin -- Contfrac
   Put("Square_Root_Of_2 = ");
   Start_time := clock;
   Put(e_Real_Image(Square_Root_Of_2.Continued_Fraction(200)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put("Napiers_Constant = ");
   Start_time := clock;
   Put (e_Real_Image(Napiers_Constant.Continued_Fraction(200)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put("Pi = ");
   Start_time := clock;
   Put (e_Real_Image(Pi.Continued_Fraction(10000)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
end Contfrac2;
