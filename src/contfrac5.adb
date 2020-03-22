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
   end Square_Root_Of_2;

   package Ext_Real_Square_Root_Of_2 is new Extended_Real(Real,30);
   package Ext_Real_Square_Root_Of_2_IO is new Ext_Real_Square_Root_Of_2.IO; use Ext_Real_Square_Root_Of_2_IO;
   package Ext_Square_Root_Of_2 is new Square_Root_Of_2(Real, 30, Ext_Real_Square_Root_Of_2);

   generic
      type Scalar is digits <>;
      nr_of_digits : Integer;
      with package Ext_Real is new Extended_Real(Scalar, nr_of_digits);
   package Napiers_Constant is
      function A (N : in Natural)  return Scalar is (Scalar(if N = 0 then 2 else N));
      function B (N : in Positive) return Scalar is (Scalar(if N = 1 then 1 else N-1));
      function Estimate is new Continued_Fraction(Scalar, nr_of_digits, A, B, Ext_Real);
   end Napiers_Constant;

   package Ext_Real_Napiers_Constant is new Extended_Real(Real, 60);
   package Ext_Real_Napiers_Constant_IO is new Ext_Real_Napiers_Constant.IO; use Ext_Real_Napiers_Constant_IO;
   package Ext_Napiers_Constant is new Napiers_Constant(Real, 60, Ext_Real_Napiers_Constant);

   generic
      type Scalar is digits <>;
      ddd : Integer;
      with package Ext_Real is new Extended_Real(Scalar,ddd);
   package Pi is
      function A (N : in Natural)  return Scalar is (Scalar(if N = 0 then 3 else 6));
      function B (N : in Positive) return Scalar is (Scalar(((2 * N - 1) ** 2)));
      function Estimate is new Continued_Fraction(Scalar, ddd, A, B, Ext_Real);
   end Pi;

   package Ext_Real_Pi is new Extended_Real(Real,90);
   package Ext_Real_Pi_IO is new Ext_Real_Pi.IO; use Ext_Real_Pi_IO;
   package Ext_Pi is new Pi(Real, 90, Ext_Real_Pi);

   -- package Scalar_Text_IO is new Ada.Text_IO.Float_IO (Scalar);
   use Ada.Text_IO;

begin -- Contfrac
   Put("Square_Root_Of_2(digits: " &  Ext_Real_Square_Root_Of_2.Desired_Decimal_Digit_Precision'Image & ")  = ");
   Start_time := clock;
   Put(e_Real_Image(Ext_Square_Root_Of_2.Estimate (200)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put("Napiers_Constant(digits: " & Ext_Real_Napiers_Constant.Desired_Decimal_Digit_Precision'Image & ") = ");
   Start_time := clock;
   Put (e_Real_Image(Ext_Napiers_Constant.Estimate (200)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
   Put("Pi(digits: " & Ext_Real_Pi.Desired_Decimal_Digit_Precision'Image & ") = ");
   Start_time := clock;
   Put (e_Real_Image(Ext_Pi.Estimate (10000)));
   End_time := clock;
   Exec_Time := End_Time - Start_Time;
   Put_line (" Execution time : " & Duration'Image (To_Duration(Exec_Time)) & " seconds ");
end Contfrac5;
