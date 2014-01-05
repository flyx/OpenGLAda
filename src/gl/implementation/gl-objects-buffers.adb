--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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
with GL.Enums;

package body GL.Objects.Buffers is
   use type Low_Level.Enums.Buffer_Kind;

   function Hash (Key : Low_Level.Enums.Buffer_Kind)
     return Ada.Containers.Hash_Type is
      function Value is new Ada.Unchecked_Conversion
        (Source => Low_Level.Enums.Buffer_Kind, Target => Low_Level.Enum);
   begin
      return Ada.Containers.Hash_Type (Value (Key));
   end Hash;

   package Buffer_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type     => Low_Level.Enums.Buffer_Kind,
       Element_Type => Buffer'Class,
       Hash         => Hash,
       Equivalent_Keys => Low_Level.Enums."=");
   use type Buffer_Maps.Cursor;

   Current_Buffers : Buffer_Maps.Map;


   procedure Bind (Target : Buffer_Target; Object : Buffer'Class) is
      Cursor : constant Buffer_Maps.Cursor
        := Current_Buffers.Find (Target.Kind);
   begin
      if Cursor = Buffer_Maps.No_Element or else
        Buffer_Maps.Element (Cursor).Reference.GL_Id /= Object.Reference.GL_Id
        then
         API.Bind_Buffer (Target.Kind, Object.Reference.GL_Id);
         Raise_Exception_On_OpenGL_Error;
         if Cursor = Buffer_Maps.No_Element then
            Current_Buffers.Insert (Target.Kind, Object);
         else
            Current_Buffers.Replace_Element (Cursor, Object);
         end if;
      end if;
   end Bind;

   function Current_Object (Target : Buffer_Target) return Buffer'Class is
      Cursor : constant Buffer_Maps.Cursor
        := Current_Buffers.Find (Target.Kind);
   begin
      if Cursor /= Buffer_Maps.No_Element then
         return Buffer_Maps.Element (Cursor);
      else
         raise No_Object_Bound_Exception with Target.Kind'Img;
      end if;
   end Current_Object;

   procedure Load_To_Buffer (Target : Buffer_Target;
                             Data   : Pointers.Element_Array;
                             Usage  : Buffer_Usage) is
      use type C.long;
   begin
      API.Buffer_Data (Target.Kind,
        Pointers.Element'Size * Data'Length / System.Storage_Unit,
        Data (Data'First)'Address, Usage);
      Raise_Exception_On_OpenGL_Error;
   end Load_To_Buffer;

   procedure Allocate (Target : Buffer_Target; Number_Of_Bytes: Long;
      Usage  : Buffer_Usage) is
   begin
      API.Buffer_Data (Target.Kind, Low_Level.SizeIPtr (Number_Of_Bytes),
                       System.Null_Address, Usage);
      Raise_Exception_On_OpenGL_Error;
   end Allocate;
   
   procedure Draw_Elements (Mode : Connection_Mode; Count : Types.Size;
                            Index_Type : Unsigned_Numeric_Type) is
   begin
      API.Draw_Elements (Mode, Count, Index_Type, 0);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Elements;
   
   procedure Map (Target : in out Buffer_Target; Access_Type : Access_Kind;
                  Pointer : out Pointers.Pointer) is
      function Map_Buffer is new API.Loader.Function_With_2_Params
        ("glMapBuffer", Low_Level.Enums.Buffer_Kind, Objects.Access_Kind,
         Pointers.Pointer);
   begin
      Pointer := Map_Buffer (Target.Kind, Access_Type);
      Raise_Exception_On_OpenGL_Error;
   end Map;
   
   procedure Unmap (Target : in out Buffer_Target) is
   begin
      API.Unmap_Buffer (Target.Kind);
      Raise_Exception_On_OpenGL_Error;
   end Unmap;
   
   function Pointer (Target : Buffer_Target) return Pointers.Pointer is
      procedure Buffer_Pointer is new API.Loader.Getter_With_3_Params
        ("glGetBufferPointerv", Low_Level.Enums.Buffer_Kind,
         Enums.Buffer_Pointer_Param, Pointers.Pointer);
      Ret : Pointers.Pointer := null;
   begin
      Buffer_Pointer (Target.Kind, Enums.Buffer_Map_Pointer, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Pointer;
   
   procedure Get_Sub_Data (Target : in out Buffer_Target;
                           Offset : Types.Size;
                           Data   : in out Pointers.Element_Array) is
      procedure Buffer_Sub_Data is new API.Loader.Getter_With_4_Params
        ("glBufferSubData", Low_Level.Enums.Buffer_Kind, Low_Level.IntPtr,
         Low_Level.SizeIPtr, Pointers.Element_Array);
   begin
      Buffer_Sub_Data (Target.Kind, Low_Level.IntPtr (Offset),
                       Low_Level.SizeIPtr (Data'Length), Data);
      Raise_Exception_On_OpenGL_Error;
   end Get_Sub_Data;
   
   function Access_Type (Target : Buffer_Target) return Access_Kind is
      Ret : Access_Kind := Access_Kind'First;
   begin
      API.Get_Buffer_Parameter_Access_Kind (Target.Kind, Enums.Buffer_Access,
                                            Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Access_Type;
      
   function Mapped (Target : Buffer_Target) return Boolean is
      Ret : Low_Level.Bool := Low_Level.Bool (False);
   begin
      API.Get_Buffer_Parameter_Bool (Target.Kind, Enums.Buffer_Mapped, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Mapped;
   
   function Size (Target : Buffer_Target) return Types.Size is
      Ret : Types.Size := 0;
   begin
      API.Get_Buffer_Parameter_Size (Target.Kind, Enums.Buffer_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Size;
   
   function Usage (Target : Buffer_Target) return Buffer_Usage is
      Ret : Buffer_Usage := Buffer_Usage'First;
   begin
      API.Get_Buffer_Parameter_Usage (Target.Kind, Enums.Buffer_Usage, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Usage;

   overriding procedure Initialize_Id (Object : in out Buffer) is
      New_Id : UInt := 0;
   begin
      API.Gen_Buffers (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding procedure Delete_Id (Object : in out Buffer) is
   begin
      API.Delete_Buffers (1, (1 => Object.Reference.GL_Id));
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.Initialized := False;
   end Delete_Id;
   
   procedure Invalidate_Data (Object : in out Buffer) is
   begin 
      API.Invalidate_Buffer_Data (Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Invalidate_Data;
   
   procedure Invalidate_Sub_Data (Object : in out Buffer;
                                  Offset, Length : Long_Size) is
   begin
      API.Invalidate_Buffer_Sub_Data (Object.Reference.GL_Id,
                                      Low_Level.IntPtr (Offset),
                                      Low_Level.SizeIPtr (Length));
      Raise_Exception_On_OpenGL_Error;
   end Invalidate_Sub_Data;

end GL.Objects.Buffers;
