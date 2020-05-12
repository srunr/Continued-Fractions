with Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Real_Time; use Ada.Real_Time;
with Extended_Real;
with Extended_Real.IO;
with Extended_Real.Elementary_Functions;
procedure Contfrac7 is

   type Real is digits 15;

   Start_time, End_time : Time;
   Exec_time : Time_Span;

   -- https://en.wikipedia.org/wiki/Continued_fraction

   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      with function A (N : in Natural)  return Scalar;
      with function B (N : in Positive) return Scalar;
      with package Ext_Real is new Extended_Real(Scalar,nr_of_digits);
   Package Generic_Continued_Fraction is

      function Estimate (Steps : in Natural) return Ext_Real.e_Real;
      function e_Real_Value( str : in String) return Ext_Real.e_Real;
      function delta_convergent( Step : in Natural) return Ext_Real.e_Real;
      package Generic_Continued_Fraction_IO is new Ext_Real.IO;

   end Generic_Continued_Fraction;

   Package body Generic_Continued_Fraction is

      function Estimate (Steps : in Natural) return Ext_Real.e_Real is
         use Ext_Real;
         function A (N : in Natural)  return e_Real is (Make_Extended(A(N)));
         function B (N : in Positive) return e_Real is (Make_Extended(B(N)));

         Fraction : e_Real := Make_Extended(0.0);
      begin
         for N in reverse Natural range 1 .. Steps loop
            Fraction := B(N) / (A(N) + Fraction);
         end loop;
         return A (0) + Fraction;
      end Estimate;

      function e_Real_Value( str : in String) return Ext_Real.e_Real is
         rval : Ext_Real.e_Real := Ext_Real."+"(0);
         Last : Natural := 0;
      begin
         Generic_Continued_Fraction_IO.e_Real_Val(str, rval, Last);
         return rval;
      end e_Real_Value;

      function delta_convergent( Step : in Natural) return Ext_Real.e_Real is
         use Ext_Real;
      begin
         return Estimate(Step) - Estimate(Step-1);
      end delta_convergent;

   end Generic_Continued_Fraction;

   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      CF_Steps : Natural;
      with package Ext_Real is new Extended_Real(Scalar,nr_of_digits);
      with function A (N : in Natural; x : in Ext_Real.e_Real) return Ext_Real.e_Real;
      with function B (N : in Positive; x : in Ext_Real.e_Real) return Ext_Real.e_Real;
   Package Generic_Continued_Fraction_eReal_Functions is

      function Estimate (x : in Ext_Real.e_Real; Steps : in Natural := CF_Steps) return Ext_Real.e_Real;
      function e_Real_Value( str : in String) return Ext_Real.e_Real;
      function delta_convergent( x : in Ext_Real.e_Real; Step : in Natural) return Ext_Real.e_Real;
      package Generic_Continued_Fraction_IO is new Ext_Real.IO;

   end Generic_Continued_Fraction_eReal_Functions;

   Package body Generic_Continued_Fraction_eReal_Functions is

      function Estimate (x : in Ext_Real.e_Real; Steps : in Natural := CF_Steps) return Ext_Real.e_Real is
         use Ext_Real;

         Fraction : e_Real := Make_Extended(0.0);
      begin
         for N in reverse Natural range 1 .. Steps loop
            Fraction := B(N,x) / (A(N,x) + Fraction);
         end loop;
         return A (0,x) + Fraction;
      end Estimate;

      function e_Real_Value( str : in String) return Ext_Real.e_Real is
         rval : Ext_Real.e_Real := Ext_Real."+"(0);
         Last : Natural := 0;
      begin
         Generic_Continued_Fraction_IO.e_Real_Val(str, rval, Last);
         return rval;
      end e_Real_Value;

      function delta_convergent( x : in Ext_Real.e_Real; Step : in Natural) return Ext_Real.e_Real is
         use Ext_Real;
      begin
         return Estimate(x,Step) - Estimate(x,Step-1);
      end delta_convergent;

   end Generic_Continued_Fraction_eReal_Functions;

   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      with package Ext_Real is new Extended_Real(Scalar,nr_of_digits);
   package Pi is
      function A (N : in Natural)  return Scalar is (Scalar(if N = 0 then 3 else 6));
      function B (N : in Positive) return Scalar is (Scalar(((2 * N - 1) ** 2)));
      package Ext_Real_Continued_Fraction is new Generic_Continued_Fraction(Scalar, nr_of_digits, A, B, Ext_Real);
   end Pi;

   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      with package Ext_Real is new Extended_Real(Scalar,nr_of_digits);
   package Pi2 is -- See  https://en.wikipedia.org/wiki/Generalized_continued_fraction
      function A (N : in Natural)  return Scalar is (Scalar((if N = 0 then 0 else 2 * N - 1)));
      function B (N : in Positive) return Scalar is (Scalar(if N = 0 then 0 else (if N = 1 then 4 else (N - 1)**2)));
      -- converges linearly to pi with at least 3 decimal digits per 4 terms.
      package Ext_Real_Continued_Fraction is new Generic_Continued_Fraction(Scalar, nr_of_digits, A, B, Ext_Real);
   end Pi2;

   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      FracIter : Integer;
      with package Ext_Real is new Extended_Real(Scalar, nr_of_digits);
   package sinus is
      function AA (N : in Natural; x : in Ext_Real.e_Real) return Ext_Real.e_Real;
      function BB (N : in Positive; x : in Ext_Real.e_Real) return Ext_Real.e_Real;
      package Ext_Real_Continued_Fraction_Function is new Generic_Continued_Fraction_eReal_Functions(Scalar, nr_of_digits, FracIter, Ext_Real, AA, BB);
      package Ext_Pi2 is new Pi2(Scalar, nr_of_digits, Ext_Real);
   end sinus;

   package body sinus is

      pi : Ext_Real.e_Real := Ext_Pi2.Ext_Real_Continued_Fraction.Estimate(FracIter);

      function AA (N : in Natural; x : in Ext_Real.e_Real) return Ext_Real.e_Real is
         use Ext_Real;
         x1 : Ext_Real.e_Real := x - (+2.0)*pi*Make_Extended(Scalar(Scalar'Floor(Make_Real(x/((+2.0)*pi)))));
         x2 : Ext_Real.e_Real := x1*x1;
         rval : Ext_Real.e_Real;
      begin

         if N = 0 then rval := +0;
         elsif N = 1 then rval := +1;
         elsif N = 2 then rval := +6 - x2;
         else
               rval := +Scalar((N+(N-2))*(N+(N-1))) - x2;
         end if;

         return rval;

      end AA;

      function BB (N : in Positive; x : in Ext_Real.e_Real) return Ext_Real.e_Real is
         use Ext_Real;
         -- pi : Ext_Real.e_Real := Ext_Pi2.Ext_Real_Continued_Fraction.Estimate(Integer(4*nr_of_digits/3));
         x1 : Ext_Real.e_Real := x - (+2.0)*pi*Make_Extended(Scalar(Scalar'Floor(Make_Real(x/((+2.0)*pi)))));
         x2 : Ext_Real.e_Real := x1*x1;
         rval : Ext_Real.e_Real;
      begin

         if N = 1 then rval := x1;
         elsif N = 2 then rval := x2;
         elsif N = 3 then rval := (+6)*x2;
         else rval := Make_Extended(Scalar((N+(N-2))*(N+(N-1))))*x2;
         end if;
         return rval;

      end BB;

   end sinus;

   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      with package Ext_Real is new Extended_Real(Scalar, nr_of_digits);
   package Square_Root_Of_2 is
      function A (N : in Natural)  return Scalar is (Scalar((if N = 0 then 1 else 2)));
      function B (N : in Positive) return Scalar is (Scalar(1));
      package Ext_Real_Continued_Fraction is new Generic_Continued_Fraction(Scalar, nr_of_digits, A, B, Ext_Real);
   end Square_Root_Of_2;

   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      with package Ext_Real is new Extended_Real(Scalar, nr_of_digits);
   package Napiers_Constant is
      function A (N : in Natural)  return Scalar is (Scalar(if N = 0 then 2 else N));
      function B (N : in Positive) return Scalar is (Scalar(if N = 1 then 1 else N-1));
      package Ext_Real_Continued_Fraction is new Generic_Continued_Fraction(Scalar, nr_of_digits, A, B, Ext_Real);
   end Napiers_Constant;


   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      with package Ext_Real is new Extended_Real(Scalar,nr_of_digits);
   package Golden_Ratio is
      function A (N : in Natural)  return Scalar is (Scalar(1));
      function B (N : in Positive) return Scalar is (Scalar(1));
      package Ext_Real_Continued_Fraction is new Generic_Continued_Fraction(Scalar, nr_of_digits, A, B, Ext_Real);
   end Golden_Ratio;

   use Ada.Text_IO;

begin -- Contfrac

   declare
      Dec_digits : Natural := 30;
      Fraction_iterations : Natural := 100;
      package Ext_Real_Square_Root_Of_2 is new Extended_Real(Real,Dec_digits); use Ext_Real_Square_Root_Of_2;
      package Ext_Real_Square_Root_Of_2_IO is new Ext_Real_Square_Root_Of_2.IO; use Ext_Real_Square_Root_Of_2_IO;
      package Ext_Square_Root_Of_2 is new Square_Root_Of_2(Real, Dec_digits, Ext_Real_Square_Root_Of_2); use Ext_Square_Root_Of_2;
      SquareRootOf2_30 : constant String := "1.41421356237309504880168872421";
      -- source : https://www.wolframalpha.com/input/?i=sqr%282%29+30+digits
   begin

      Put("Square_Root_Of_2(digits: " &  Ext_Real_Square_Root_Of_2.Desired_Decimal_Digit_Precision'Image & ")  = ");
      Start_time := clock;
      Put(e_Real_Image(Ext_Square_Root_Of_2.Ext_Real_Continued_Fraction.Estimate(Fraction_iterations)));
      End_time := clock;
      Exec_Time := End_Time - Start_Time;
      Put_Line(" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
      Put_line("SquareRootOf2 constant         =  " & SquareRootOf2_30);
      Put_Line("SquareRootOf2_30error : " & Ext_Real_Square_Root_Of_2_IO.e_Real_Image(Ext_Square_Root_Of_2.Ext_Real_Continued_Fraction.Estimate(Fraction_iterations) -
                 Ext_Square_Root_Of_2.Ext_Real_Continued_Fraction.e_Real_Value(SquareRootOf2_30)));
      Put_Line("Delta Convergent(" & Fraction_iterations'Image & ") = " & e_Real_Image(Ext_Square_Root_Of_2.Ext_Real_Continued_Fraction.delta_convergent(Fraction_iterations)));
      new_line;

   end;

   declare
      Dec_digits : Natural := 60;
      Fraction_iterations : Natural := 100;
      package Ext_Real_Napiers_Constant is new Extended_Real(Real, Dec_digits); use Ext_Real_Napiers_Constant;
      package Ext_Real_Napiers_Constant_IO is new Ext_Real_Napiers_Constant.IO; use Ext_Real_Napiers_Constant_IO;
      package Ext_Napiers_Constant is new Napiers_Constant(Real, Dec_digits, Ext_Real_Napiers_Constant); use Ext_Napiers_Constant;

      NapiersConstant60 : constant String := "2.71828182845904523536028747135266249775724709369995957496697";
      -- source : https://www.wolframalpha.com/input/?i=exp%281%29+60+digits
   begin

      Put("Napiers_Constant(digits: " & Ext_Real_Napiers_Constant.Desired_Decimal_Digit_Precision'Image & ") = ");
      Start_time := clock;
      Put (e_Real_Image(Ext_Napiers_Constant.Ext_Real_Continued_Fraction.Estimate(Fraction_iterations)));
      End_time := clock;
      Exec_Time := End_Time - Start_Time;
      Put_Line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
      Put_line("NapiersConstant constant      =  " & NapiersConstant60);
      Put_Line("NapiersConstant60error : " & Ext_Real_Napiers_Constant_IO.e_Real_Image(Ext_Napiers_Constant.Ext_Real_Continued_Fraction.Estimate(Fraction_iterations) -
                 Ext_Napiers_Constant.Ext_Real_Continued_Fraction.e_Real_Value(NapiersConstant60)));
      Put_Line("Delta Convergent(" & Fraction_iterations'Image & ") = " & e_Real_Image(Ext_Napiers_Constant.Ext_Real_Continued_Fraction.delta_convergent(Fraction_iterations)));
      new_line;

   end;

   declare
      Dec_digits : Natural := 90;
      Fraction_iterations : Natural := 100;
      package Ext_Real_Pi is new Extended_Real(Real,Dec_digits); use Ext_Real_Pi;
      package Ext_Real_Pi_IO is new Ext_Real_Pi.IO; use Ext_Real_Pi_IO;
      package Ext_Pi is new Pi(Real, Dec_digits, Ext_Real_Pi); use Ext_Pi;
      Pi90 : constant String := "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803483";
      -- source : https://www.wolframalpha.com/input/?i=N%5BPi%2C+90%5D
   begin

      Put("Pi(digits: " & Ext_Real_Pi.Desired_Decimal_Digit_Precision'Image & ") = ");
      Start_time := clock;
      Put (e_Real_Image(Ext_Pi.Ext_Real_Continued_Fraction.Estimate (Fraction_iterations)));
      End_time := clock;
      Exec_Time := End_Time - Start_Time;
      Put_Line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
      Put_line("Pi constant     =  " & Pi90);
      Put_Line("Pi90error : " & Ext_Real_Pi_IO.e_Real_Image(Ext_Pi.Ext_Real_Continued_Fraction.Estimate(Fraction_iterations) -
                 Ext_Pi.Ext_Real_Continued_Fraction.e_Real_Value(Pi90)));
      Put_Line("Delta Convergent(" & Fraction_iterations'Image & ") = " & e_Real_Image(Ext_Pi.Ext_Real_Continued_Fraction.delta_convergent(Fraction_iterations)));
      new_line;
   end;

   declare
      use Ada.Numerics;
      Dec_digits : Natural := 90;
      Fraction_iterations : Natural := 100;
      package Pi2_Real_Elementary_Functions is new Generic_Elementary_Functions(Real); use Pi2_Real_Elementary_Functions;
      package Ext_Real_Pi2 is new Extended_Real(Real,Dec_digits); use Ext_Real_Pi2;
      Package Ext_Real_Pi2_Elementary_Functions is new Ext_Real_Pi2.Elementary_Functions( Sqrt => Pi2_Real_Elementary_Functions.Sqrt,
                                                                                          Log => Pi2_Real_Elementary_Functions.Log,
                                                                                          Exp => Pi2_Real_Elementary_Functions.Exp,
                                                                                          Arcsin =>Pi2_Real_Elementary_Functions.Arcsin,
                                                                                          Arctan => Pi2_Real_Elementary_Functions.Arctan); use Ext_Real_Pi2_Elementary_Functions;
      package Ext_Real_Pi2_IO is new Ext_Real_Pi2.IO; use Ext_Real_Pi2_IO;
      package Ext_Pi2 is new Pi2(Real, Dec_digits, Ext_Real_Pi2); use Ext_Pi2;

      Pi90 : constant String := "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803483";
      -- source : https://www.wolframalpha.com/input/?i=N%5BPi%2C+90%5D
   begin

      Put("Pi2(digits: " & Ext_Real_Pi2.Desired_Decimal_Digit_Precision'Image & ") = ");
      Start_time := clock;
      Put (e_Real_Image(Ext_Pi2.Ext_Real_Continued_Fraction.Estimate(Fraction_iterations)));
      End_time := clock;
      Exec_Time := End_Time - Start_Time;
      Put_Line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
      Put_line("Pi constant      =  " & Pi90);
      Put_Line("Pi290error : " & Ext_Real_Pi2_IO.e_Real_Image(Ext_Pi2.Ext_Real_Continued_Fraction.Estimate(Fraction_iterations) -
                 Ext_Pi2.Ext_Real_Continued_Fraction.e_Real_Value(Pi90)));
      Put_Line("Delta Convergent(" & Fraction_iterations'Image & ") = " & e_Real_Image(Ext_Pi2.Ext_Real_Continued_Fraction.delta_convergent(Fraction_iterations)));
      Put_Line("Pi2 - e_Pi  = " & Ext_Real_Pi2_IO.e_Real_Image(Ext_Pi2.Ext_Real_Continued_Fraction.Estimate(Fraction_iterations) - e_Pi));
      new_line;
   end;

   declare
      Dec_digits : Natural := 50;
      Fraction_iterations : Natural := 100;
      package Ext_Real_Golden_Ratio is new Extended_Real(Real,Dec_digits); use Ext_Real_Golden_Ratio;
      package Ext_Real_Golden_Ratio_IO is new Ext_Real_Golden_Ratio.IO; use Ext_Real_Golden_Ratio_IO;
      package Ext_Golden_Ratio is new Golden_Ratio(Real, Dec_digits, Ext_Real_Golden_Ratio); use Ext_Golden_Ratio;
      GoldenRatio50 : constant String := "1.6180339887498948482045868343656381177203091798058";
      -- source: https://www.wolframalpha.com/input/?i=N%5BGoldenRatio%2C+50%5D
   begin
      Put("Golden_Ratio(digits: " & Ext_Real_Golden_Ratio.Desired_Decimal_Digit_Precision'Image & ") = ");
      Start_time := clock;
      Put (e_Real_Image(Ext_Golden_Ratio.Ext_Real_Continued_Fraction.Estimate(Fraction_iterations)));
      End_time := clock;
      Exec_Time := End_Time - Start_Time;
      Put_Line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
      Put_line("Golden_Ratio constant     =  " & GoldenRatio50);
      Put_Line("GoldenRatio50error  : " & Ext_Real_Golden_Ratio_IO.e_Real_Image(Ext_Golden_Ratio.Ext_Real_Continued_Fraction.Estimate(Fraction_iterations) -
                 Ext_Golden_Ratio.Ext_Real_Continued_Fraction.e_Real_Value(GoldenRatio50)));
      Put_Line("Delta Convergent(" & Fraction_iterations'Image & ") = " & e_Real_Image(Ext_Golden_Ratio.Ext_Real_Continued_Fraction.delta_convergent(Fraction_iterations)));
      new_line;
   end;

   declare
      use Ada.Numerics;
      Dec_digits : Natural := 90;
      Fraction_iterations : Natural := 100;
      package Sin_Real_Elementary_Functions is new Generic_Elementary_Functions(Real); use Sin_Real_Elementary_Functions;
      package Ext_Real_Sin is new Extended_Real(Real,Dec_digits); use Ext_Real_Sin;
      Package Ext_Real_Sin_Elementary_Functions is new Ext_Real_Sin.Elementary_Functions( Sqrt => Sin_Real_Elementary_Functions.Sqrt,
                                                                                          Log => Sin_Real_Elementary_Functions.Log,
                                                                                          Exp => Sin_Real_Elementary_Functions.Exp,
                                                                                          Arcsin => Sin_Real_Elementary_Functions.Arcsin,
                                                                                          Arctan => Sin_Real_Elementary_Functions.Arctan); use Ext_Real_Sin_Elementary_Functions;
      package Ext_Real_Sin_IO is new Ext_Real_Sin.IO; use Ext_Real_Sin_IO;
      package Ext_Sin is new sinus(Real, Dec_digits, Fraction_iterations, Ext_Real_Sin); use Ext_Sin;
      -- x : Real := Ada.Numerics.pi/2.0;
      Pi90 : constant String := "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803483";
      -- source : https://www.wolframalpha.com/input/?i=N%5BPi%2C+90%5D
      x : e_Real := Ext_Sin.Ext_Real_Continued_Fraction_Function.e_Real_Value(Pi90)/(+6.0);
   begin

      Put("Sinus_ContFrac(digits: " & Ext_Real_Sin.Desired_Decimal_Digit_Precision'Image & ")( x = " & e_Real_Image(x) & ") = ");
      Start_time := clock;
      Put (e_Real_Image(Ext_Sin.Ext_Real_Continued_Fraction_Function.Estimate(x)));
      End_time := clock;
      Exec_Time := End_Time - Start_Time;
      Put_Line(" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
      Put_Line("Sinus90error : " & Ext_Real_Sin_IO.e_Real_Image(Ext_Sin.Ext_Real_Continued_Fraction_Function.Estimate(x) -
                 Ext_Real_Sin_Elementary_Functions.Sin(Ext_Real_Sin_Elementary_Functions.e_Pi/(+6.0),(+2.0)*Ext_Real_Sin_Elementary_Functions.e_Pi)));
      Put_Line("Delta Convergent(" & Fraction_iterations'Image & ") = " & e_Real_Image(Ext_Sin.Ext_Real_Continued_Fraction_Function.delta_convergent(x,Fraction_iterations)));
      Put_Line("Ext_Real_Sin.Elementary_Functions.sin(" &  Ext_Real_Sin_IO.e_Real_Image(Ext_Real_Sin_Elementary_Functions.e_Pi/(+6.0)) &") = " &
                 Ext_Real_Sin_IO.e_Real_Image(Ext_Real_Sin_Elementary_Functions.Sin(Ext_Real_Sin_Elementary_Functions.e_Pi/(+6.0),(+2.0)*Ext_Real_Sin_Elementary_Functions.e_Pi)));

      Start_time := clock;
      Put("Ext_Real_Sin.Elementary_Functions.sin(pi/6) - 1/2 = " & Ext_Real_Sin_IO.e_Real_Image(
          Ext_Real_Sin_Elementary_Functions.Sin(Ext_Real_Sin_Elementary_Functions.e_Pi/(+6.0),(+2.0)*Ext_Real_Sin_Elementary_Functions.e_Pi) - (+1.0)/(+2.0)));
      End_time := clock;
      Exec_Time := End_Time - Start_Time;
      Put_Line(" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
      Put_Line("Ext_Real_Sin.Elementary_Functions.Arcsin(1/2) * 6.0 = " &
                 Ext_Real_Sin_IO.e_Real_Image((+6.0)*Ext_Real_Sin_Elementary_Functions.Arcsin((+1.0)/(+2.0))));
      Put_Line("Ext_Real_Sin.Elementary_Functions.Arcsin(1/2) * 6.0 - Pi90 = " &
                 Ext_Real_Sin_IO.e_Real_Image((+6.0)*Ext_Real_Sin_Elementary_Functions.Arcsin((+1.0)/(+2.0)) - Ext_Sin.Ext_Real_Continued_Fraction_Function.e_Real_Value(Pi90)));
      new_line;
   end;

end Contfrac7;
