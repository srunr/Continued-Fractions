with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
procedure Contfrac is

   type Scalar is digits 15;

   Start_time, End_time : Time;
   Exec_time : Time_Span;

   generic
      type Scalar is digits <>;
      with function A (N : in Natural)  return Natural;
      with function B (N : in Positive) return Natural;
   function Continued_Fraction (Steps : in Natural) return Scalar;

   function Continued_Fraction (Steps : in Natural) return Scalar is
      function A (N : in Natural)  return Scalar is (Scalar (Natural'(A (N))));
      function B (N : in Positive) return Scalar is (Scalar (Natural'(B (N))));

      Fraction : Scalar := 0.0;
   begin
      for N in reverse Natural range 1 .. Steps loop
         Fraction := B (N) / (A (N) + Fraction);
      end loop;
      return A (0) + Fraction;
   end Continued_Fraction;

   package Square_Root_Of_2 is
      function A (N : in Natural)  return Natural is (if N = 0 then 1 else 2);
      function B (N : in Positive) return Natural is (1);

      function Estimate is new Continued_Fraction (Scalar, A, B);
   end Square_Root_Of_2;

   package Napiers_Constant is
      function A (N : in Natural)  return Natural is (if N = 0 then 2 else N);
      function B (N : in Positive) return Natural is (if N = 1 then 1 else N-1);

      function Estimate is new Continued_Fraction (Scalar, A, B);
   end Napiers_Constant;

   package Pi is
      function A (N : in Natural)  return Natural is  (if N = 0 then 3 else 6);
      function B (N : in Positive) return Natural is ((2 * N - 1) ** 2);

      function Estimate is new Continued_Fraction (Scalar, A, B);
   end Pi;

   package Scalar_Text_IO is new Ada.Text_IO.Float_IO (Scalar);
   use Ada.Text_IO, Scalar_Text_IO;
begin -- Contfrac
   Put("Square_Root_Of_2 = ");
   Start_time := clock;
   Put(Square_Root_Of_2.Estimate (200), Exp => 0);
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put("Napiers_Constant = ");
   Start_time := clock;
   Put (Napiers_Constant.Estimate (200), Exp => 0);
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put("Pi = ");
   Start_time := clock;
   Put (Pi.Estimate (10000),             Exp => 0);
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
end Contfrac;
