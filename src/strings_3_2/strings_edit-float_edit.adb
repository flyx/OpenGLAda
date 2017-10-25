--                                                                    --
--  package Strings_Edit            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--  Strings_Edit.Float_Edit                        Spring, 2000       --
--                                                                    --
--                                Last revision :  22:44 07 Apr 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.IO_Exceptions;
with Ada.Numerics.Elementary_Functions;
with Strings_Edit.Integers;
pragma Elaborate_All (Strings_Edit.Integers);

use Ada.IO_Exceptions;
use Ada.Numerics.Elementary_Functions;
use Strings_Edit.Integers;

package body Strings_Edit.Float_Edit is
   --
   -- Base-radix logarithms of the number of  valid  digits  in  machine
   -- radix, i.e. how many base-radix digits  one  machine  radix  digit
   -- represents.
   --
   Logs : constant array (NumberBase) of Float :=
            (  2 => Log (Float (Number'Machine_Radix), 2.0),
               3 => Log (Float (Number'Machine_Radix), 3.0),
               4 => Log (Float (Number'Machine_Radix), 4.0),
               5 => Log (Float (Number'Machine_Radix), 5.0),
               6 => Log (Float (Number'Machine_Radix), 6.0),
               7 => Log (Float (Number'Machine_Radix), 7.0),
               8 => Log (Float (Number'Machine_Radix), 8.0),
               9 => Log (Float (Number'Machine_Radix), 9.0),
              10 => Log (Float (Number'Machine_Radix), 10.0),
              11 => Log (Float (Number'Machine_Radix), 11.0),
              12 => Log (Float (Number'Machine_Radix), 12.0),
              13 => Log (Float (Number'Machine_Radix), 13.0),
              14 => Log (Float (Number'Machine_Radix), 14.0),
              15 => Log (Float (Number'Machine_Radix), 15.0),
              16 => Log (Float (Number'Machine_Radix), 16.0)
            );
   --
   -- GetExponentField -- Calculate the length of the exponent
   --
   --	 Base	- Of the number
   --
   -- This  function  calculates  how  many  digits  are  necessary   to
   -- represent any Base-radix exponent in decimal the format.
   --
   -- Returns :
   --
   --	 The exponent length
   --
   function GetExponentField (Base : NumberBase) return Natural is
   begin
      if Number'Machine_Emax > -Number'Machine_Emin then
         return
            Natural
            (  Log (Float (Number'Machine_Emax) * Logs (Base), 10.0)
            +  0.5
            );
      else
         return
            Natural
            (  Log (Float (-Number'Machine_Emin) * Logs (Base), 10.0)
            +  0.5
            );
      end if;
   end GetExponentField;
   --
   -- The exponent lengths for different Bases
   --
   ExponentField : constant array (NumberBase) of Natural :=
                      (  GetExponentField (2),  GetExponentField (3),
                         GetExponentField (4),  GetExponentField (5),
                         GetExponentField (6),  GetExponentField (7),
                         GetExponentField (8),  GetExponentField (9),
                         GetExponentField (10), GetExponentField (11),
                         GetExponentField (12), GetExponentField (13),
                         GetExponentField (14), GetExponentField (15),
                         GetExponentField (16)
                      );
   --
   -- GetExponent -- Get the exponent part from the string
   --
   --	    Source  - The string
   --	    Pointer - Within the string
   --	    Value   - Of the exponent (output)
   --
   -- This procedure scans the Source string starting from  the  Pointer
   -- position  for  the  exponent  part of a number. Spaces are skipped
   -- first. The exponent can be introduced by either E or e. Its  value
   -- is  a decimal number returned via Value. If no valid exponent part
   -- present Value is set to zero (the Pointer remains intact).
   --
   procedure GetExponent
             (  Source   : in String;
                Pointer  : in out Integer;
                Value    : out Integer
             )  is
      Index : Integer := Pointer;
   begin
      Get (Source, Index);
      if (  Index > Source'Last
         or else
            (  Source (Index) /= 'E'
            and
               Source (Index) /= 'e'
         )  )
      then
         Value := 0;
      else
         Index := Index + 1;
         Get (Source, Index);
         Get (Source, Index, Value, 10, -1000, +1000, True, True);
         Pointer := Index;
      end if;
   exception
      when End_Error | Data_Error | Layout_Error | Constraint_Error =>
         Value := 0;
   end GetExponent;

   procedure Get
             (  Source   : in String;
                Pointer  : in out Integer;
                Value    : out Number'Base;
                Base     : in NumberBase := 10;
                First    : in Number'Base := Number'Base'First;
                Last     : in Number'Base := Number'Base'Last;
                ToFirst  : in Boolean := False;
                ToLast   : in Boolean := False
             )  is
      Radix        : constant Number'Base := Number'Base (Base);
      Digit        : Natural;
      Exponent     : Integer;
      ExponentPart : Integer;
      Mantissa     : Number'Base := 0.0;
      Multiplicand : Number'Base := 1.0;
      Sign         : Integer := 0;
      Index        : Integer := Pointer;
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      if Pointer <= Source'Last then
         case Source (Index) is
            when '-' =>
               Index := Index + 1;
               Sign := -1;
               Get (Source, Index);
            when '+' =>
               Index := Index + 1;
               Sign := 1;
               Get (Source, Index);
            when others =>
               null;
         end case;
      end if;
      ExponentPart := Index;
      Exponent := Index;
      while Index <= Source'Last loop
         Digit := GetDigit (Source (Index));
         exit when Digit >= Base;
         Mantissa := Mantissa + Number'Base (Digit) * Multiplicand;
         Multiplicand := Multiplicand / Radix;
         Index := Index + 1;
      end loop;
      Exponent := Index - Exponent - 1;
      if Index + 1 <= Source'Last and then Source (Index) = '.' then
         Index := Index + 1;
         Digit := GetDigit (Source (Index));
         if Digit < Base then
            loop
               Mantissa :=
                  Mantissa + Number'Base (Digit) * Multiplicand;
               Multiplicand := Multiplicand / Radix;
               Index := Index + 1;
               exit when Index > Source'Last;
               Digit := GetDigit (Source (Index));
               exit when Digit >= Base;
            end loop;
         else
            Index := Index - 1;
         end if;
      end if;
      if 0 = Sign then
         if Pointer = Index then
            raise End_Error; -- No fraction part - no number
         end if;
         Sign := 1;
      else
         if Pointer = Index - 1 then
            raise Data_Error; -- Only a sign
         end if;
      end if;
      if (  ExponentPart = Index - 1
         and then
            Source (ExponentPart) = '.'
         )
      then
         raise Data_Error; -- Only decimal point is present
      end if;
      GetExponent (Source, Index, ExponentPart);
      if Mantissa > 0.0 then
         begin
            Mantissa := Number'Base (Sign) * Mantissa;
            Exponent := ExponentPart + Exponent;
            ExponentPart :=
               Integer (Float (Number'Machine_Emax) * Logs (Base) * 0.7);
            if Exponent >= ExponentPart then
               Multiplicand := Radix ** ExponentPart;
               loop
                  Mantissa := Mantissa * Multiplicand;
                  Exponent := Exponent - ExponentPart;
                  exit when Exponent < ExponentPart;
               end loop;
            elsif Exponent <= -ExponentPart then
               ExponentPart := -ExponentPart;
               Multiplicand := Radix ** ExponentPart;
               loop
                  Mantissa := Mantissa * Multiplicand;
                  Exponent := Exponent - ExponentPart;
                  exit when Exponent > ExponentPart;
               end loop;
            end if;
            Mantissa := Mantissa * Radix ** Exponent;
            if not Mantissa'Valid then
               if Sign > 0 and ToLast then
                  Value   := Last;
                  Pointer := Index;
                  return;
               elsif Sign < 0 and ToFirst then
                  Value   := First;
                  Pointer := Index;
                  return;
               end if;
               raise Constraint_Error;
            end if;
         exception
            when Numeric_Error | Constraint_Error =>
               if Sign > 0 and ToLast then
                  Value   := Last;
                  Pointer := Index;
                  return;
               elsif Sign < 0 and ToFirst then
                  Value   := First;
                  Pointer := Index;
                  return;
               end if;
               raise Constraint_Error;
         end;
      end if;
      if Mantissa < First then
         if ToFirst then
            Mantissa := First;
         else
            raise Constraint_Error;
         end if;
      elsif Mantissa > Last then
         if ToLast then
            Mantissa := Last;
         else
            raise Constraint_Error;
         end if;
      end if;
      Value := Mantissa;
      Pointer := Index;
   end Get;

   function Value
            (  Source  : in String;
               Base    : in NumberBase := 10;
               First   : in Number'Base := Number'First;
               Last    : in Number'Base := Number'Last;
               ToFirst : in Boolean := False;
               ToLast  : in Boolean := False
            )  return Number'Base is
      Result  : Number'Base;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Result, Base, First, Last, ToFirst, ToLast);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Number'Base;
                Base        : in NumberBase := 10;
                PutPlus     : in Boolean := False;
                RelSmall    : in Positive := MaxSmall;
                AbsSmall    : in Integer := -MaxSmall;
                Field       : in Natural := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  is
      --
      -- Sign -- Make the sign of a number
      --
      --    Value   - Of the number
      --    PutPlus - Always put sign, if true
      --
      -- Returns :
      --
      --    The string to be used as the sign
      --
      function Sign
               (  Value   : Number'Base;
                  PutPlus : Boolean
               )  return String is
      begin
         if Value < 0.0 then
            return "-";
         elsif PutPlus then
            return "+";
         else
            return "";
         end if;
      end Sign;
      --
      -- PutExponent -- Make the exponent part
      --
      --    Value  - Of the exponent part
      --    Base   - Of the mantissa
      --
      -- The  exponent  part  is  written in the fixed length format. It
      -- always  has  sign.  Leading  zeros are added to make the length
      -- fixed. The space is added in front of  the  exponent  part  for
      -- bases 15 and 16.
      --
      -- Returns :
      --
      --    The string containing the exponent part
      --
      function PutExponent
               (  Value : Integer;
                  Base  : NumberBase
               )  return String is
         Result   : String (1..32);
         Exponent : Integer := Value;
         Pointer  : Integer := Result'First;
         Sign     : Character := '+';
      begin
         if Exponent < 0 then
            Sign := '-';
            Exponent := -Value;
         end if;
         Put
         (  Result,
            Pointer,
            Exponent,
            Field   => ExponentField (Base),
            PutPlus => False,
            Justify => Right,
            Fill    => '0'
         );
         if Base >= 15 then
            return " e" & Sign & Result (Result'First..Pointer - 1);
         else
            return 'E' & Sign & Result (Result'First..Pointer - 1);
         end if;
      end PutExponent;

      Precision : Integer :=   -- The maximal available precision
                     Integer
                     (  Float (Number'Machine_Mantissa)
                     *  Logs (Base)
                     );
      Mantissa  : Number'Base := abs Value;
      Exponent  : Integer := 0;
      Increment : Integer := 0;
      Radix     : constant Number'Base := Number'Base (Base);
   begin
      if Mantissa <= 0.0 then
         Put (Destination, Pointer, "0", Field, Justify, Fill);
         return;
      end if;
      if RelSmall < Precision then
         Precision := RelSmall;
      end if;
      --
      -- Normalization. The goal is to bring the mantissa into the range
      -- ]1..1/Base]
      --
      while Mantissa < 1.0 / Radix loop
         Mantissa := Mantissa * Radix;
         Exponent := Exponent - 1;
      end loop;
      while Mantissa >= 1.0 loop
         Mantissa := Mantissa / Radix;
         Exponent := Exponent + 1;
      end loop;
      if Exponent - AbsSmall < Precision then
         Precision := Exponent - AbsSmall;
         Increment := 1;
      end if;
      --
      -- Rounding
      --
      Mantissa := Mantissa + 0.5 / Radix ** Precision;
      if Mantissa >= 1.0 then
         Mantissa  := Mantissa / Radix;
         Exponent  := Exponent + 1;
	 Precision := Precision + Increment;
      end if;
      if Precision > 0 then
         declare
            ExponentPart : constant String :=
                           PutExponent (Exponent - 1, Base);
            MantissaPart : String (1..Precision);
            Digit        : Natural;
         begin
            --
            -- Convert the Mantissa to character string
            --
            for Index in MantissaPart'range loop
               Mantissa := Mantissa * Radix;
               Digit := Integer (Mantissa);
               if Number'Base (Digit) > Mantissa then
                  Digit := Digit - 1;
               end if;
               Mantissa := Mantissa - Number'Base (Digit);
               MantissaPart (Index) := Figures (Digit + Figures'First);
            end loop;
            if (  Exponent < - ExponentPart'Length
               or Exponent > Precision
               )
            then
               --
               -- Format X.XXXXXeYYY
               --
	         Put
               (  Destination,
	            Pointer,
                  (  Sign (Value, PutPlus)
                  &  MantissaPart (MantissaPart'First)
                  &  '.'
                  &  MantissaPart
                     (  MantissaPart'First + 1
                     .. MantissaPart'Last
                     )
                  &  ExponentPart
		      ),
                  Field,
                  Justify,
                  Fill
               );
            elsif Exponent = Precision then
               --
               -- Format: XXXXXXX
               --
               Put
               (  Destination,
                  Pointer,
                  Sign (Value, PutPlus) & MantissaPart,
                  Field,
                  Justify,
                  Fill
	         );
            elsif Exponent > 0 then
               --
               -- Format: XXXX.XXX
               --
               Put
               (  Destination,
                  Pointer,
                  (  Sign (Value, PutPlus)
                  &  MantissaPart
                     (  MantissaPart'First
                     .. MantissaPart'First + Exponent - 1
                     )
                  &  '.'
                  &  MantissaPart
                     (  MantissaPart'First + Exponent
                     .. MantissaPart'Last
                  )  ),
                  Field,
                  Justify,
                  Fill
               );
            else
               --
               -- Format: 0.000XXXXX
               --
               Put
               (  Destination,
                  Pointer,
                  (  Sign (Value, PutPlus)
                  &  "0."
                  &  String'(1..-Exponent => '0')
                  &  MantissaPart
                  ),
                  Field,
                  Justify,
                  Fill
               );
            end if;
         end;
      else
         --
         -- Zero
	   --
         Put (Destination, Pointer, "0", Field, Justify, Fill);
      end if;
   end Put;

   function Image
            (  Value    : in Number'Base;
               Base     : in NumberBase := 10;
               PutPlus  : in Boolean    := False;
               RelSmall : in Positive   := MaxSmall;
               AbsSmall : in Integer    := -MaxSmall
            )  return String is
      Text    : String (1..80);
      Pointer : Integer := Text'First;
   begin
      Put (Text, Pointer, Value, Base, PutPlus, RelSmall, AbsSmall);
      return Text (Text'First..Pointer - 1);
   end Image;

end Strings_Edit.Float_Edit;
