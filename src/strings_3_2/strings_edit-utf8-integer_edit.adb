--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.                          Luebeck            --
--        Integer_Edit.Superscript_Edit            Spring, 2005       --
--  Interface                                                         --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Strings_Edit.UTF8.Integer_Edit is

   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Number'Base;
                Base    : Script_Base := 10;
                First   : Number'Base := Number'First;
                Last    : Number'Base := Number'Last;
                ToFirst : Boolean     := False;
                ToLast  : Boolean     := False
             )  is
      Radix   : constant Number'Base := Number'Base (Base);
      Min     : constant Number'Base := Number'Base'First / Radix;
      Max     : constant Number'Base := Number'Base'Last  / Radix;
      Result  : Number'Base := 0;
      Digit   : Number'Base;
      Sign_Of : Sign;
      Start   : Integer;
      Index   : Integer := Pointer;

      procedure Skip is
         pragma Inline (Skip);
      begin
         loop
            Get_Digit (Source, Index, Natural (Digit));
            exit when Digit > Radix;
         end loop;
         Pointer := Index;
      end Skip;

      procedure Neg_Overflow is
         pragma Inline (Neg_Overflow);
      begin
         if not ToFirst then
            raise Constraint_Error;
         end if;
         Value := First;
         Skip;
      end Neg_Overflow;

      procedure Pos_Overflow is
         pragma Inline (Pos_Overflow);
      begin
         if not ToLast then
            raise Constraint_Error;
         end if;
         Value := Last;
         Skip;
      end Pos_Overflow;
   begin
      if Index < Source'First then
         raise Layout_Error;
      end if;
      if Index > Source'Last then
         if Index - 1 > Source'Last then
            raise Layout_Error;
         else
            raise End_Error;
         end if;
      end if;
      --
      -- Get sign
      --
      Get_Sign (Source, Index, Sign_Of);
      Start := Index;
      if Sign_Of = Minus then
         while Index <= Source'Last loop
            Get_Digit (Source, Index, Natural (Digit));
            exit when Digit >= Radix;
            if Result < Min then
               Neg_Overflow;
               return;
            end if;
            Result := Result * Radix;
            if Result < Number'Base'First + Digit then
               Neg_Overflow;
               return;
            end if;
            Result := Result - Digit;
         end loop;
      else
         while Index <= Source'Last loop
            Get_Digit (Source, Index, Natural (Digit));
            exit when Digit >= Radix;
            if Result > Max then
               Pos_Overflow;
               return;
            end if;
            Result := Result * Radix;
            if Result > Number'Base'Last - Digit then
               Pos_Overflow;
               return;
            end if;
            Result := Result + Digit;
         end loop;
      end if;
      if Start = Index then
         if Sign_Of = None then
            raise End_Error;
         else
            raise Data_Error;
         end if;
      end if;
      if Result < First then
         if ToFirst then
            Result := First;
         else
            raise Constraint_Error;
         end if;
      elsif Result > Last then
         if ToLast then
            Result := Last;
         else
            raise Constraint_Error;
         end if;
      end if;
      Value := Result;
      Pointer := Index;
   end Get;

   function Value
            (  Source  : String;
               Base    : Script_Base := 10;
               First   : Number'Base := Number'First;
               Last    : Number'Base := Number'Last;
               ToFirst : Boolean := False;
               ToLast  : Boolean := False
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
                Value       : Number'Base;
                Base        : Script_Base := 10;
                PutPlus     : Boolean     := False
             )  is
      Radix  : constant Number'Base := Number'Base (Base);
      Result : String (1..256);
      Accum  : Number'Base := Value;
      Index  : Positive    := Result'Last;
   begin
      if Accum > 0 then
         loop
            Put_Digit (Result, Index, Script_Digit (Accum rem Radix));
            Accum := Accum / Radix;
            exit when Accum = 0;
         end loop;
         if PutPlus then
            Put_Sign (Result, Index, Plus);
         end if;
      elsif Accum < 0 then
         loop
            Put_Digit
            (  Result,
               Index,
               Script_Digit (-(Accum rem Radix))
            );
            Accum := Accum / Radix;
            exit when Accum = 0;
         end loop;
         Put_Sign (Result, Index, Minus);
      else
         Put_Digit (Result, Index, 0);
      end if;
      Put
      (  Destination,
         Pointer,
         Result (Index + 1..Result'Last)
      );
   end Put;

   function Image
            (  Value   : Number'Base;
               Base    : Script_Base := 10;
               PutPlus : Boolean := False
            )  return String is
      Text    : String (1..256);
      Pointer : Integer := Text'First;
   begin
      Put (Text, Pointer, Value, Base, PutPlus);
      return Text (Text'First..Pointer - 1);
   end Image;

end Strings_Edit.UTF8.Integer_Edit;
