--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Streams.                       Luebeck            --
--        Generic_Integer                          Autumn, 2014       --
--  Implementation                                                    --
--                                Last revision :  16:33 18 Nov 2016  --
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

package body Strings_Edit.Streams.Generic_Integer is

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Number
             )  is
      Result   : Number;
      Power    : Natural := 6;
      Negative : Boolean;
      This     : Stream_Element;
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - 1 > Data'Last
         )  )
      then
         Raise_Exception
         (  Layout_Error'Identity,
            "Pointer is out of bounds"
         );
      end if;
      if Pointer <= Data'Last then
         This    := Data (Pointer);
         Pointer := Pointer + 1;
         begin
            Result := Number ((This and 2#0111_1110#) / 2);
            Negative := 0 /= (This and 1);
            if 0 = (This and 16#80#) then
               if Negative then
                  Value := -Result - 1;
               else
                  Value := Result;
               end if;
               return;
            end if;
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Number is too large"
               );
         end;
         while Pointer <= Data'Last loop
            This    := Data (Pointer);
            Pointer := Pointer + 1;
            begin
               Result := Result + 2**Power * Number (This and 16#7F#);
               if 0 = (This and 16#80#) then
                  if Negative then
                     Value := -Result - 1;
                  else
                     Value := Result;
                  end if;
                  return;
               end if;
            exception
               when Constraint_Error =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Number is too large"
                  );
            end;
            Power := Power + 7;
         end loop;
      end if;
      Raise_Exception
      (  End_Error'Identity,
         "Non-terminated chain sequence"
      );
   end Get;

   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Number is
      Result   : Number  := 0;
      Old      : Number  := 0;
      Power    : Natural := 6;
      Negative : Boolean;
      Buffer   : Stream_Element_Array (1..1);
      This     : Stream_Element renames Buffer (1);
      Last     : Stream_Element_Offset;
   begin
      Read (Stream.all, Buffer, Last);
      if Last = 1 then
         begin
            Result := Number ((This and 2#0111_1110#) / 2);
            Negative := 0 /= (This and 1);
            if 0 = (This and 16#80#) then
               if Negative then
                  return -Result - 1;
               else
                  return Result;
               end if;
            end if;
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Number is too large"
               );
         end;
         loop
            Read (Stream.all, Buffer, Last);
            exit when Last /= 1;
            begin
               Result := Result + 2**Power * Number (This and 16#7F#);
               if 0 = (This and 16#80#) then
                  if Negative then
                     return -Result - 1;
                  else
                     return Result;
                  end if;
               end if;
            exception
               when Constraint_Error =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Number is too large"
                  );
            end;
            Power := Power + 7;
         end loop;
      end if;
      Raise_Exception
      (  End_Error'Identity,
         "Non-terminated chain sequence"
      );
   end Input;

   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Number
             )  is
      Buffer  : Stream_Element_Array (1..Number'Size + 6 / 7);
      Pointer : Stream_Element_Offset := 1;
   begin
      Put (Buffer, Pointer, Value);
      Write (Stream.all, Buffer (1..Pointer - 1));
   end Output;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Number
             )  is
      Item : Number := Value;
   begin
      if Pointer not in Data'Range then
         Raise_Exception
         (  Layout_Error'Identity,
            "Pointer is out of bounds"
         );
      end if;
      if Item >= 0 then
         Item := Value;
         Data (Pointer) := Stream_Element (Item mod (16#40#)) * 2;
      else
         Item := -(Value + 1);
         Data (Pointer) := Stream_Element (Item mod (16#40#)) * 2 + 1;
      end if;
      Item := Item / 16#40#;
      if Item = 0 then
         Pointer := Pointer + 1;
         return;
      end if;
      Data (Pointer) := Data (Pointer) or 16#80#;
      Pointer := Pointer + 1;
      while Pointer <= Data'Last loop
         Data (Pointer) := Stream_Element (Item mod 16#80#) or 16#80#;
         Item := Item / 16#80#;
         if Item = 0 then
            Data (Pointer) := Data (Pointer) and 16#7F#;
            Pointer := Pointer + 1;
            return;
         end if;
         Pointer := Pointer + 1;
      end loop;
      Raise_Exception (Layout_Error'Identity, "No room for output");
   end Put;

end Strings_Edit.Streams.Generic_Integer;
