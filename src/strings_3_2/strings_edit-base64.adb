--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Base64                         Luebeck            --
--  Implementation                                 Autumn, 2014       --
--                                                                    --
--                                Last revision :  10:03 22 Nov 2014  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Interfaces;         use Interfaces;

package body Strings_Edit.Base64 is

   function From_Base64 (Text : String) return String is
      Result  : String (1..(Text'Length * 3 + 3) / 4);
      This    : Unsigned_16;
      Accum   : Unsigned_16 := 0;
      Bits    : Natural     := 0;
      Pointer : Integer     := 1;
   begin
      for Index in Text'Range loop
         This := Character'Pos (Text (Index));
         case This is
            when Character'Pos ('A')..Character'Pos ('Z') =>
               Accum := Accum * 64 + This - Character'Pos ('A');
            when Character'Pos ('a')..Character'Pos ('z') =>
               Accum := Accum * 64 + This - Character'Pos ('a') + 26;
            when Character'Pos ('0')..Character'Pos ('9') =>
               Accum := Accum * 64 + This - Character'Pos ('0') + 52;
            when Character'Pos ('+') =>
               Accum := Accum * 64 + 62;
            when Character'Pos ('/') =>
               Accum := Accum * 64 + 63;
            when Character'Pos ('=') =>
               if Index = Text'Last then -- 2 bytes accumulated
                  exit when Bits = 2;
               elsif Index + 1 = Text'Last then -- 1 byte accumulate
                  exit when Text (Index + 1) = '=' and then Bits = 4;
               end if;
               Raise_Exception
               (  Data_Error'Identity,
                  "Unexpected character '='"
               );
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Non-Base64 character '" & Text (Index) & '''
               );
         end case;
         Bits := Bits + 6;
         if Bits >= 8 then
            Bits := Bits - 8;
            This := Accum / 2**Bits;
            Result (Pointer) :=
               Character'Val ((Accum / 2**Bits) and 16#FF#);
            Pointer := Pointer + 1;
            Accum   := Accum mod 2**Bits;
         end if;
      end loop;
      return Result (1..Pointer - 1);
   end From_Base64;

   type Base64_Encoding is
      array (Unsigned_16 range 0..63) of Character;
   Base64 : constant Base64_Encoding := "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                        "abcdefghijklmnopqrstuvwxyz" &
                                        "0123456789+/";

   function To_Base64 (Text : String) return String is
      Result  : String (1..(Text'Length + 3 / 3) * 4);
      Pointer : Integer     := 1;
      Bits    : Natural     := 0;
      Accum   : Unsigned_16 := 0;
   begin
      for Index in Text'Range loop
         Accum := Accum * 256 + Character'Pos (Text (Index));
         Bits  := Bits + 8;
         if Bits >= 12 then
            Bits := Bits - 6;
            Result (Pointer) :=
               Base64 ((Accum / 2**Bits) and 2#11_1111#);
            Accum   := Accum mod 2**Bits;
            Pointer := Pointer + 1;
         end if;
         if Bits >= 6 then
            Bits := Bits - 6;
            Result (Pointer) :=
               Base64 ((Accum / 2**Bits) and 2#11_1111#);
            Accum   := Accum mod 2**Bits;
            Pointer := Pointer + 1;
         end if;
      end loop;
      case Bits is
         when 2 =>
            Result (Pointer) := Base64 (Accum * 2**4);
            Pointer := Pointer + 1;
            Result (Pointer) := '=';
            Pointer := Pointer + 1;
            Result (Pointer) := '=';
            Pointer := Pointer + 1;
         when 4 =>
            Result (Pointer) := Base64 (Accum * 2**2);
            Pointer := Pointer + 1;
            Result (Pointer) := '=';
            Pointer := Pointer + 1;
         when others =>
            null;
      end case;
      return Result (1..Pointer - 1);
   end To_Base64;

end Strings_Edit.Base64;
