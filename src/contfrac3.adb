with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Extended_Real;
with Extended_Real.IO;
procedure Contfrac3 is

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

   generic
      type Scalar is digits <>;
      with function A (N : in Natural)  return Scalar;
      with function B (N : in Positive) return Scalar;
      with function Make_Ext (S : in Scalar) return e_Real;
   function Continued_Fraction (Steps : in Natural) return e_Real;

   function Continued_Fraction (Steps : in Natural) return e_Real is
      function A (N : in Natural)  return e_Real is (Make_Ext(A(N)));
      function B (N : in Positive) return e_Real is (Make_Ext(B(N)));

      Fraction : e_Real := Make_Ext(0.0);
   begin
      for N in reverse Natural range 1 .. Steps loop
         Fraction := B(N) / (A(N) + Fraction);
      end loop;
      return A (0) + Fraction;
   end Continued_Fraction;

   generic
      type Scalar is digits <>;
      with function Make_Ext (S : in Scalar) return e_Real;
   package Square_Root_Of_2 is
      function A (N : in Natural)  return Scalar is (Scalar((if N = 0 then 1 else 2)));
      function B (N : in Positive) return Scalar is (Scalar(1));

      function Estimate is new Continued_Fraction(Scalar, A, B, Make_Ext);
   end Square_Root_Of_2;

   package Ext_Square_Root_Of_2 is new Square_Root_Of_2(Real, Make_Extended);

   generic
      type Scalar is digits <>;
      with function Make_Ext (S : in Scalar) return e_Real;
   package Napiers_Constant is
      function A (N : in Natural)  return Scalar is (Scalar(if N = 0 then 2 else N));
      function B (N : in Positive) return Scalar is (Scalar(if N = 1 then 1 else N-1));

      function Estimate is new Continued_Fraction(Scalar, A, B, Make_Ext);
   end Napiers_Constant;

   package Ext_Napiers_Constant is new Napiers_Constant(Real, Make_Extended);

   generic
      type Scalar is digits <>;
      with function Make_Ext (S : in Scalar) return e_Real;
   package Pi is
      function A (N : in Natural)  return Scalar is (Scalar(if N = 0 then 3 else 6));
      function B (N : in Positive) return Scalar is (Scalar(((2 * N - 1) ** 2)));

      function Estimate is new Continued_Fraction(Scalar, A, B, Make_Ext);
   end Pi;

   package Ext_Pi is new Pi(Real, Make_Extended);

   -- package Scalar_Text_IO is new Ada.Text_IO.Float_IO (Scalar);
   use Ada.Text_IO;

begin -- Contfrac
   Put("Square_Root_Of_2 = ");
   Start_time := clock;
   Put(e_Real_Image(Ext_Square_Root_Of_2.Estimate (200)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put("Napiers_Constant = ");
   Start_time := clock;
   Put (e_Real_Image(Ext_Napiers_Constant.Estimate (200)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put("Pi = ");
   Start_time := clock;
   Put (e_Real_Image(Ext_Pi.Estimate (10000)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
end Contfrac3;
