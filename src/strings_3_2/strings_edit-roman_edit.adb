--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Roman_Edit                     Luebeck            --
--  Implementation                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  21:03 21 Apr 2009  --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.IO_Exceptions;       use Ada.IO_Exceptions;

package body Strings_Edit.Roman_Edit is
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Roman;
                First   : in Roman := Roman'First;
                Last    : in Roman := Roman'Last;
                ToFirst : in Boolean := False;
                ToLast  : in Boolean := False
             )  is
      Result : Integer := 0;
      Index  : Integer := Pointer;
      function Get (Letter : Character) return Boolean;
      pragma Inline (Get);
      function Get (Letter : Character) return Boolean is
      begin
         if (  Letter /= '.'
            and then
               Index <= Source'Last
            and then
               To_Lower (Source (Index)) = Letter
            )
         then
            Index := Index + 1;
            return True;
         else
            return False;
         end if;
      end Get;
      function Get (I, V, X : Character) return Integer is
      begin
         if Get (I) then
            if Get (I) then
               if Get (I) then
                  return 3;
               else
                  return 2;
               end if;
            elsif Get (V) then
               return 4;
            elsif Get (X) then 
               return 9;
            else
               return 1;
            end if;
         elsif Get (V) then
            if Get (I) then
               if Get (I) then
                  if Get (I) then
                     return 8;
                  else
                     return 7;
                  end if;
               else
                  return 6;
               end if;
            else
               return 5;
            end if;
         else
            return 0;
         end if;
      end Get;
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
      Result := Get ('m', '.', '.');
      Result := Result * 10 + Get ('c', 'd', 'm');
      Result := Result * 10 + Get ('x', 'l', 'c');
      Result := Result * 10 + Get ('i', 'v', 'x');
      if Index = Pointer then
         raise End_Error;
      end if;
      if Result < Integer (First) then
         if ToFirst then
            Value := First;
         else
            raise Constraint_Error;
         end if;
      elsif Result > Integer (Last) then
         if ToLast then
            Value := Last;
         else
            raise Constraint_Error;
         end if;
      else
         Pointer := Index;
         Value := Roman (Result);
      end if;
   end Get;

   function Value
            (  Source  : in String;
               First   : in Roman := Roman'First;
               Last    : in Roman := Roman'Last;
               ToFirst : in Boolean := False;
               ToLast  : in Boolean := False
            )  return Roman is
      Result  : Roman;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Result, First, Last, ToFirst, ToLast);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Roman;
	        LowerCase   : in Boolean := False;
                Field       : in Natural := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  is
      subtype Digit is Integer range 0..9;
      type Decimal is array (Integer range 1..4) of Digit;
      Encoded : Decimal;
      Result  : String (1..16);
      Index   : Integer := Result'First;
      Number  : Integer := Integer (Value);
      procedure Put (Letter : Character);
      pragma Inline (Put);
      procedure Put (Letter : Character) is
      begin
         Result (Index) := Letter;
         Index := Index + 1;
      end Put;
      procedure Put (Figure : Digit; I, V, X : Character) is
      begin
         case Figure is
            when 0 => null;
            when 1 => Put (I);
            when 2 => Put (I); Put (I);
            when 3 => Put (I); Put (I); Put (I);
            when 4 => Put (I); Put (V);
            when 5 => Put (V);
            when 6 => Put (V); Put (I);
            when 7 => Put (V); Put (I);  Put (I);
            when 8 => Put (V); Put (I);  Put (I);  Put (I);
            when 9 => Put (I); Put (X);
         end case;
      end Put;
   begin
      for Position in reverse Decimal'Range loop
         Encoded (Position) := Number mod 10;
         Number := Number / 10;
      end loop;
      if LowerCase then
         Put (Encoded (1), 'm', 'm', 'm');
         Put (Encoded (2), 'c', 'd', 'm');
         Put (Encoded (3), 'x', 'l', 'c');
         Put (Encoded (4), 'i', 'v', 'x');
      else
         Put (Encoded (1), 'M', 'M', 'M');
         Put (Encoded (2), 'C', 'D', 'M');
         Put (Encoded (3), 'X', 'L', 'C');
         Put (Encoded (4), 'I', 'V', 'X');
      end if;
      Put
      (  Destination,
         Pointer,
         Result (Result'First..Index - 1),
         Field,
         Justify,
         Fill
      );
   end Put;

   function Image
            (  Value     : in Roman;
               LowerCase : in Boolean := False
            )  return String is
      Text    : String (1..16);
      Pointer : Integer := Text'First;
   begin
      Put (Text, Pointer, Value, LowerCase);
      return Text (Text'First..Pointer - 1);
   end Image;

end Strings_Edit.Roman_Edit;
