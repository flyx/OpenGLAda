--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Unchecked_Conversion;

with System;

with GL.API;

package body GL.Objects.Buffer is
   use type Low_Level.Enums.Buffer_Kind;
   use type Low_Level.UInt;
   use type Long;

   function Hash (Key : Low_Level.Enums.Buffer_Kind)
     return Ada.Containers.Hash_Type is
      function Value is new Ada.Unchecked_Conversion
        (Source => Low_Level.Enums.Buffer_Kind, Target => Low_Level.Enum);
   begin
      return Ada.Containers.Hash_Type (Value (Key));
   end Hash;
   
   package Buffer_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type     => Low_Level.Enums.Buffer_Kind,
       Element_Type => Buffer_Object,
       Hash         => Hash,
       Equivalent_Keys => Low_Level.Enums."=");
   use type Buffer_Maps.Cursor;
   
   Current_Buffers : Buffer_Maps.Map;
   

   procedure Bind (Target : Buffer_Target; Object : Buffer_Object'Class) is
      Cursor : Buffer_Maps.Cursor := Current_Buffers.Find (Target.Kind);
   begin
      if Cursor /= Buffer_Maps.No_Element and then
        Buffer_Maps.Element (Cursor).Reference.GL_Id /= Object.Reference.GL_Id
        then
         API.Bind_Buffer (Target.Kind, Object.Reference.GL_Id);
         Check_OpenGL_Error;
         Current_Buffers.Replace_Element (Cursor, Buffer_Object (Object));
      end if;
   end Bind;
   
   procedure Load_To_Buffer (Target : Buffer_Target; Data : Array_Type;
                             Usage  : Buffer_Usage) is
   begin
      API.Buffer_Data (Target.Kind,
        Element_Type'Size * Data'Length / System.Storage_Unit,
        Data (Data'First)'Address, Usage);
      Check_OpenGL_Error;
   end Load_To_Buffer;
   
   procedure Allocate (Target : Buffer_Target; Number_Of_Bytes: Long;
      Usage  : Buffer_Usage) is
   begin
      API.Buffer_Data (Target.Kind, Number_Of_Bytes, System.Null_Address,
                       Usage);
      Check_OpenGL_Error;
   end Allocate;

   overriding procedure Create_Id (Object : in out Buffer_Object) is
      New_Id : Low_Level.UInt := 0;
   begin
      API.Gen_Buffers (1, New_Id);
      Check_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
   end Create_Id;

   overriding procedure Delete_Id (Object : in out Buffer_Object) is
   begin
      API.Delete_Buffers (1, (1 => Object.Reference.GL_Id));
      Check_OpenGL_Error;
   end Delete_Id;

end GL.Objects.Buffer;