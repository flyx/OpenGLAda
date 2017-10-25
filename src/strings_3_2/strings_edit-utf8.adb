--                                                                    --
--  package Strings_Edit.UTF8       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2005       --
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

with Ada.IO_Exceptions;    use Ada.IO_Exceptions;

package body Strings_Edit.UTF8 is

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out UTF8_Code_Point
             )  is
      Accum : UTF8_Code_Point'Base;
      Code  : UTF8_Code_Point'Base;
      Index : Integer := Pointer;
      Last  : Boolean := False;
   begin
      if Index < Source'First then
         raise Layout_Error;
      end if;
      if Index > Source'Last then
         if Index = Source'Last + 1 then
            raise End_Error;
         else
            raise Layout_Error;
         end if;
      end if;
      Code := UTF8_Code_Point (Character'Pos (Source (Index)));
      case Code is
         when 0..16#7F# => -- 1 byte (ASCII)
            Value   := Code;
            Pointer := Index + 1;
            return;
         when 16#C2#..16#DF# => -- 2 bytes
            Accum := (Code and 16#1F#) * 2**6;
            Last  := True;
         when 16#E0# => -- 3 bytes
            Index := Index + 1;
            if Index >= Source'Last then
               raise Data_Error;
            end if;
            Code := UTF8_Code_Point (Character'Pos (Source (Index)));
            if Code not in 16#A0#..16#BF# then
               raise Data_Error;
            end if;
            Accum := (Code and 16#3F#) * 2**6;
            Last  := True;
         when 16#E1#..16#EF# => -- 3 bytes
            Accum := (Code and 16#0F#) * 2**12;
         when 16#F0# => -- 4 bytes
            Index := Index + 1;
            if Index >= Source'Last then
               raise Data_Error;
            end if;
            Code := UTF8_Code_Point (Character'Pos (Source (Index)));
            if Code not in 16#90#..16#BF# then
               raise Data_Error;
            end if;
            Accum := (Code and 16#3F#) * 2**12;
         when 16#F1#..16#F3# => -- 4 bytes
            Accum := (Code and 16#07#) * 2**18;
            Index := Index + 1;
            if Index >= Source'Last then
               raise Data_Error;
            end if;
            Code := UTF8_Code_Point (Character'Pos (Source (Index)));
            if Code not in 16#80#..16#BF# then
               raise Data_Error;
            end if;
            Accum := Accum or (Code and 16#3F#) * 2**12;
         when 16#F4# => -- 4 bytes
            Accum := (Code and 16#07#) * 2**18;
            Index := Index + 1;
            if Index >= Source'Last then
               raise Data_Error;
            end if;
            Code := UTF8_Code_Point (Character'Pos (Source (Index)));
            if Code not in 16#80#..16#8F# then
               raise Data_Error;
            end if;
            Accum := Accum or (Code and 16#3F#) * 2**12;
         when others =>
            raise Data_Error;
      end case;

      if not Last then
         Index := Index + 1;
         if Index >= Source'Last then
            raise Data_Error;
         end if;
         Code := UTF8_Code_Point (Character'Pos (Source (Index)));
         if Code not in 16#80#..16#BF# then
            raise Data_Error;
         end if;
         Accum := Accum or (Code and 16#3F#) * 2**6;
      end if;

      Index := Index + 1;
      if Index > Source'Last then
         raise Data_Error;
      end if;
      Code := UTF8_Code_Point (Character'Pos (Source (Index)));
      if Code not in 16#80#..16#BF# then
         raise Data_Error;
      end if;
      Value   := Accum or (Code and 16#3F#);
      Pointer := Index + 1;
   end Get;

   procedure Get_Backwards
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out UTF8_Code_Point
             )  is
      First  : Integer := Pointer;
      Last   : Integer;
      Result : UTF8_Code_Point;
   begin
      if First <= Source'First then
         if First < Source'First then
            raise Layout_Error;
         else
            raise End_Error;
         end if;
      end if;
      if First > Source'Last + 1 then
         raise Layout_Error;
      end if;
      loop
         First := First - 1;
         exit when Character'Pos (Source (First)) not in 16#80#..16#BF#;
         if First = Source'First then
            raise Data_Error;
         end if;
      end loop;
      Last := First;
      Get (Source, Last, Result);
      if Last /= Pointer then
         raise Data_Error;
      end if;
      Pointer := First;
      Value   := Result;
   end Get_Backwards;

   function Image (Value : UTF8_Code_Point) return String is
      Result  : String (1..4);
      Pointer : Integer := Result'First;
   begin
      Put (Result, Pointer, Value);
      return Result (1..Pointer - 1);
   end Image;

   function Length (Source : String) return Natural is
      Count : Natural := 0;
      Accum : UTF8_Code_Point;
      Index : Integer := Source'First;
   begin
      while Index <= Source'Last loop
         Get (Source, Index, Accum);
         Count := Count + 1;
      end loop;
      return Count;
   end Length;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : UTF8_Code_Point
             )  is
   begin
      if Pointer not in Destination'Range then
         raise Layout_Error;
      end if;
      if Value <= 16#7F# then
         Destination (Pointer) := Character'Val (Value);
         Pointer := Pointer + 1;
      elsif Value <= 16#7FF# then
         if Pointer >= Destination'Last then
            raise Layout_Error;
         end if;
         Destination (Pointer) :=
            Character'Val (16#C0# or Value / 2**6);
         Destination (Pointer + 1) :=
            Character'Val (16#80# or (Value and 16#3F#));
         Pointer := Pointer + 2;
      elsif Value <= 16#FFFF# then
         if 1 >= Destination'Last - Pointer then
            raise Layout_Error;
         end if;
         Destination (Pointer) :=
            Character'Val (16#E0# or Value / 2**12);
         Destination (Pointer + 1) :=
            Character'Val (16#80# or (Value / 2**6 and 16#3F#));
         Destination (Pointer + 2) :=
            Character'Val (16#80# or (Value and 16#3F#));
         Pointer := Pointer + 3;
      else
         if 2 >= Destination'Last - Pointer then
            raise Layout_Error;
         end if;
         Destination (Pointer) :=
            Character'Val (16#F0# or Value / 2**18);
         Destination (Pointer + 1) :=
            Character'Val (16#80# or (Value / 2**12 and 16#3F#));
         Destination (Pointer + 2) :=
            Character'Val (16#80# or (Value / 2**6 and 16#3F#));
         Destination (Pointer + 3) :=
            Character'Val (16#80# or (Value and 16#3F#));
         Pointer := Pointer + 4;
      end if;
   end Put;

   procedure Reverse_Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Prefix      : String;
                Position    : Natural
             )  is
   begin
      if (  Pointer not in Destination'Range
         or else
            Pointer - Destination'First < Prefix'Length
         )
      then
         raise Layout_Error;
      end if;
      Destination (Pointer) := Character'Val (Position);
      Pointer := Pointer - 1;
      for Index in reverse Prefix'Range loop
         Destination (Pointer) := Prefix (Index);
         Pointer := Pointer - 1;
      end loop;
   end Reverse_Put;

   procedure Skip
             (  Source  : String;
                Pointer : in out Integer;
                Count   : Natural := 1
             )  is
   begin
      if Count > 0 then
         declare
            Accum : UTF8_Code_Point;
            Index : Integer := Pointer;
         begin
            for Character in 1..Count loop
               Get (Source, Index, Accum);
            end loop;
            Pointer := Index;
         end;
      elsif (  Pointer < Source'First
            or else
               (  Pointer > Source'Last
               and then
                  Pointer - 1 > Source'Last
            )  )
      then
         raise Layout_Error;
      end if;
   end Skip;

   function Value (Source : String) return UTF8_Code_Point is
      Pointer : Integer := Source'First;
      Result  : UTF8_Code_Point;
   begin
      Get (Source, Pointer, Result);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

end Strings_Edit.UTF8;
