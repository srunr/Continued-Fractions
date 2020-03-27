with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Extended_Real;
with Extended_Real.IO;
procedure Contfrac5 is

   type Real is digits 15;

   Start_time, End_time : Time;
   Exec_time : Time_Span;

   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      with function A (N : in Natural)  return Scalar;
      with function B (N : in Positive) return Scalar;
      with package Ext_Real is new Extended_Real(Scalar,nr_of_digits);
   function Continued_Fraction (Steps : in Natural) return Ext_Real.e_Real;

   function Continued_Fraction (Steps : in Natural) return Ext_Real.e_Real is
      use Ext_Real;
      function A (N : in Natural)  return e_Real is (Make_Extended(A(N)));
      function B (N : in Positive) return e_Real is (Make_Extended(B(N)));

      Fraction : e_Real := Make_Extended(0.0);
   begin
      for N in reverse Natural range 1 .. Steps loop
         Fraction := B(N) / (A(N) + Fraction);
      end loop;
      return A (0) + Fraction;
   end Continued_Fraction;

   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      with package Ext_Real is new Extended_Real(Scalar, nr_of_digits);
   package Square_Root_Of_2 is
      function A (N : in Natural)  return Scalar is (Scalar((if N = 0 then 1 else 2)));
      function B (N : in Positive) return Scalar is (Scalar(1));
      function Estimate is new Continued_Fraction(Scalar, nr_of_digits, A, B, Ext_Real);
      function e_Real_Value( str : in String) return Ext_Real.e_Real;
   end Square_Root_Of_2;

   package body Square_Root_Of_2 is
      package Square_Root_Of_2_IO is new Ext_Real.IO;

      function e_Real_Value( str : in String) return Ext_Real.e_Real is
         rval : Ext_Real.e_Real := Ext_Real."+"(0);
         Last : Natural := 0;
      begin
         Square_Root_Of_2_IO.e_Real_Val(str, rval, Last);
         return rval;
      end e_Real_Value;

   end Square_Root_Of_2;

   package Ext_Real_Square_Root_Of_2 is new Extended_Real(Real,30); use Ext_Real_Square_Root_Of_2;
   package Ext_Real_Square_Root_Of_2_IO is new Ext_Real_Square_Root_Of_2.IO; use Ext_Real_Square_Root_Of_2_IO;
   package Ext_Square_Root_Of_2 is new Square_Root_Of_2(Real, 30, Ext_Real_Square_Root_Of_2); use Ext_Square_Root_Of_2;

   SquareRootOf2_30 : constant String := "1.41421356237309504880168872421";
   -- source : https://www.wolframalpha.com/input/?i=sqr%282%29+30+digits
   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      with package Ext_Real is new Extended_Real(Scalar, nr_of_digits);
   package Napiers_Constant is
      function A (N : in Natural)  return Scalar is (Scalar(if N = 0 then 2 else N));
      function B (N : in Positive) return Scalar is (Scalar(if N = 1 then 1 else N-1));
      function Estimate is new Continued_Fraction(Scalar, nr_of_digits, A, B, Ext_Real);
      function e_Real_Value( str : in String) return Ext_Real.e_Real;
   end Napiers_Constant;

   package body Napiers_Constant is
         package Napiers_Constant_IO is new Ext_Real.IO;

      function e_Real_Value( str : in String) return Ext_Real.e_Real is
         rval : Ext_Real.e_Real := Ext_Real."+"(0);
         Last : Natural := 0;
      begin
         Napiers_Constant_IO.e_Real_Val(str, rval, Last);
         return rval;
      end e_Real_Value;

   end Napiers_Constant;

   package Ext_Real_Napiers_Constant is new Extended_Real(Real, 60); use Ext_Real_Napiers_Constant;
   package Ext_Real_Napiers_Constant_IO is new Ext_Real_Napiers_Constant.IO; use Ext_Real_Napiers_Constant_IO;
   package Ext_Napiers_Constant is new Napiers_Constant(Real, 60, Ext_Real_Napiers_Constant); use Ext_Napiers_Constant;

   NapiersConstant60 : constant String := "2.71828182845904523536028747135266249775724709369995957496697";
   -- source : https://www.wolframalpha.com/input/?i=exp%281%29+60+digits

   generic
      type Scalar is digits <>;
      ddd : Integer;
      with package Ext_Real is new Extended_Real(Scalar,ddd);
   package Pi is
      function A (N : in Natural)  return Scalar is (Scalar(if N = 0 then 3 else 6));
      function B (N : in Positive) return Scalar is (Scalar(((2 * N - 1) ** 2)));
      function Estimate is new Continued_Fraction(Scalar, ddd, A, B, Ext_Real);
      function e_Real_Value( str : in String) return Ext_Real.e_Real;
   end Pi;

   package body Pi is
      package Pi_IO is new Ext_Real.IO;

      function e_Real_Value( str : in String) return Ext_Real.e_Real is
         rval : Ext_Real.e_Real := Ext_Real."+"(0);
         Last : Natural := 0;
      begin
         Pi_IO.e_Real_Val(str, rval, Last);
         return rval;
      end e_Real_Value;

   end Pi;

   package Ext_Real_Pi is new Extended_Real(Real,90); use Ext_Real_Pi;
   package Ext_Real_Pi_IO is new Ext_Real_Pi.IO; use Ext_Real_Pi_IO;
   package Ext_Pi is new Pi(Real, 90, Ext_Real_Pi);

   generic
      type Scalar is digits <>;
      ddd : Integer;
      with package Ext_Real is new Extended_Real(Scalar,ddd);
   package Pi2 is -- See  https://en.wikipedia.org/wiki/Generalized_continued_fraction
      function A (N : in Natural)  return Scalar is (Scalar((if N = 0 then 0 else 2 * N - 1)));
      function B (N : in Positive) return Scalar is (Scalar(if N = 0 then 0 else (if N = 1 then 4 else (N - 1)**2)));
      function Estimate is new Continued_Fraction(Scalar, ddd, A, B, Ext_Real);
      function e_Real_Value( str : in String) return Ext_Real.e_Real;
   end Pi2;

   package body Pi2 is
      package Pi_IO is new Ext_Real.IO;

      function e_Real_Value( str : in String) return Ext_Real.e_Real is
         rval : Ext_Real.e_Real := Ext_Real."+"(0);
         Last : Natural := 0;
      begin
         Pi_IO.e_Real_Val(str, rval, Last);
         return rval;
      end e_Real_Value;

   end Pi2;

   package Ext_Real_Pi2 is new Extended_Real(Real,90); use Ext_Real_Pi2;
   package Ext_Real_Pi_IO2 is new Ext_Real_Pi2.IO; use Ext_Real_Pi_IO2;
   package Ext_Pi2 is new Pi2(Real, 90, Ext_Real_Pi);

   Pi90 : constant String := "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803483";
   -- source : https://www.wolframalpha.com/input/?i=N%5BPi%2C+90%5D

   generic
      type Scalar is digits <>;
      ddd : Integer;
      with package Ext_Real is new Extended_Real(Scalar,ddd);
   package Golden_Ratio is
      function A (N : in Natural)  return Scalar is (Scalar(1));
      function B (N : in Positive) return Scalar is (Scalar(1));
      function Estimate is new Continued_Fraction(Scalar, ddd, A, B, Ext_Real);
      function e_Real_Value(str : In String) return Ext_Real.e_Real;
   end Golden_Ratio;

   package body Golden_Ratio is
      package Golden_Ratio_IO is new Ext_Real.IO;

      function e_Real_Value( str : in String) return Ext_Real.e_Real is
         rval : Ext_Real.e_Real := Ext_Real."+"(0);
         Last : Natural := 0;
      begin
         Golden_Ratio_IO.e_Real_Val(str, rval, Last);
         return rval;
      end e_Real_Value;

   end Golden_Ratio;

   package Ext_Real_Golden_Ratio is new Extended_Real(Real,50); use Ext_Real_Golden_Ratio;
   package Ext_Real_Golden_Ratio_IO is new Ext_Real_Golden_Ratio.IO; use Ext_Real_Golden_Ratio_IO;
   package Ext_Golden_Ratio is new Golden_Ratio(Real, 50, Ext_Real_Golden_Ratio);

   GoldenRatio50 : constant String := "1.6180339887498948482045868343656381177203091798058";
   -- source: https://www.wolframalpha.com/input/?i=N%5BGoldenRatio%2C+50%5D

   use Ada.Text_IO;

begin -- Contfrac

   Put("Square_Root_Of_2(digits: " &  Ext_Real_Square_Root_Of_2.Desired_Decimal_Digit_Precision'Image & ")  = ");
   Start_time := clock;
   Put(e_Real_Image(Ext_Square_Root_Of_2.Estimate (200)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_Line(" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put_line("SquareRootOf2 constant         =  " & SquareRootOf2_30);
   Put_Line("SquareRootOf2_30error : " & e_Real_Image(Ext_Square_Root_Of_2.Estimate(10000) - Ext_Square_Root_Of_2.e_Real_Value(SquareRootOf2_30)));
   new_line;

   Put("Napiers_Constant(digits: " & Ext_Real_Napiers_Constant.Desired_Decimal_Digit_Precision'Image & ") = ");
   Start_time := clock;
   Put (e_Real_Image(Ext_Napiers_Constant.Estimate (200)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_Line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put_line("NapiersConstant constant      =  " & NapiersConstant60);
   Put_Line("NapiersConstant60error : " & e_Real_Image(Ext_Napiers_Constant.Estimate(10000) - Ext_Napiers_Constant.e_Real_Value(NapiersConstant60)));
   new_line;

   Put("Pi(digits: " & Ext_Real_Pi.Desired_Decimal_Digit_Precision'Image & ") = ");
   Start_time := clock;
   Put (e_Real_Image(Ext_Pi.Estimate (10000)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_Line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put_line("Pi constant     =  " & Pi90);
   Put_Line("Pi90error : " & e_Real_Image(Ext_Pi.Estimate(10000) - Ext_Pi.e_Real_Value(Pi90)));
   new_line;

   Put("Pi2(digits: " & Ext_Real_Pi2.Desired_Decimal_Digit_Precision'Image & ") = ");
   Start_time := clock;
   Put (e_Real_Image(Ext_Pi2.Estimate (10000)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_Line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put_line("Pi constant      =  " & Pi90);
   Put_Line("Pi290error : " & e_Real_Image(Ext_Pi2.Estimate(10000) - Ext_Pi2.e_Real_Value(Pi90)));
   new_line;

   Put("Golden_Ratio(digits: " & Ext_Real_Golden_Ratio.Desired_Decimal_Digit_Precision'Image & ") = ");
   Start_time := clock;
   Put (e_Real_Image(Ext_Golden_Ratio.Estimate (20000)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_Line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put_line("Golden_Ratio constant     =  " & GoldenRatio50);
   Put_Line("GoldenRatio50error  : " & e_Real_Image(Ext_Golden_Ratio.Estimate (20000) - Ext_Golden_Ratio.e_Real_Value(GoldenRatio50)));

end Contfrac5;
