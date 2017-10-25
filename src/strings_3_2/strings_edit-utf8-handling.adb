--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Handling                  Luebeck            --
--  Implementation                                 Spring, 2005       --
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

package body Strings_Edit.UTF8.Handling is

   function To_UTF8 (Value : Character) return String is
      Result  : String (1..2);
      Pointer : Integer := Result'First;
   begin
      Put (Result, Pointer, UTF8_Code_Point (Character'Pos (Value)));
      return Result (1..Pointer - 1);
   end To_UTF8;
   
   function To_UTF8 (Value : String) return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Item in Value'Range loop
         Put
         (  Result,
            Pointer,
            UTF8_Code_Point (Character'Pos (Value (Item)))
         );
      end loop;
      return Result (Result'First..Pointer - 1);
   end To_UTF8;

   function To_UTF8 (Value : Wide_Character) return String is
      Result  : String (1..3);
      Pointer : Integer := Result'First;
   begin
      Put
      (  Result,
         Pointer,
         UTF8_Code_Point (Wide_Character'Pos (Value))
      );
      return Result (1..Pointer - 1);
   end To_UTF8;

   function To_UTF8 (Value : Wide_String) return String is
      Result  : String (1..Value'Length * 3);
      Pointer : Integer := Result'First;
   begin
      for Item in Value'Range loop
         Put
         (  Result,
            Pointer,
            UTF8_Code_Point (Wide_Character'Pos (Value (Item)))
         );
      end loop;
      return Result (Result'First..Pointer - 1);
   end To_UTF8;

   function To_String (Value : String) return String is
      Result  : String (1..Value'Length);
      Pointer : Integer := Value'First;
      Index   : Integer := Result'First;
      Item    : UTF8_Code_Point;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Item);
         if Item > 16#FF# then
            raise Constraint_Error;
         end if;
         Result (Index) := Character'Val (Item);
         Index := Index + 1;
      end loop;
      return Result (Result'First..Index - 1);
   end To_String;

   function To_String
            (  Value      : String;
               Substitute : Character
            )  return String is
      Result  : String (1..Value'Length);
      Pointer : Integer := Value'First;
      Index   : Integer := Result'First;
      Item    : UTF8_Code_Point;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Item);
         if Item > 16#FF# then
            Result (Index) := Substitute;
         else
            Result (Index) := Character'Val (Item);
         end if;
         Index := Index + 1;
      end loop;
      return Result (Result'First..Index - 1);
   end To_String;

   function To_Wide_String (Value : String) return Wide_String is
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Value'First;
      Index   : Integer := Result'First;
      Item    : UTF8_Code_Point;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Item);
         if Item > 16#FFFF# then
            raise Constraint_Error;
         end if;
         Result (Index) := Wide_Character'Val (Item);
         Index := Index + 1;
      end loop;
      return Result (Result'First..Index - 1);
   end To_Wide_String;

   function To_Wide_String
            (  Value      : String;
               Substitute : Wide_Character
            )  return Wide_String is
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Value'First;
      Index   : Integer := Result'First;
      Item    : UTF8_Code_Point;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Item);
         if Item > 16#FFFF# then
            Result (Index) := Substitute;
         else
            Result (Index) := Wide_Character'Val (Item);
         end if;
         Index := Index + 1;
      end loop;
      return Result (Result'First..Index - 1);
   end To_Wide_String;

end Strings_Edit.UTF8.Handling;
