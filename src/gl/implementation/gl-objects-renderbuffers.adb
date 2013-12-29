--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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

with GL.API;
with GL.Enums.Getter;

package body GL.Objects.Renderbuffers is
   function Hash (Key : Low_Level.Enums.Renderbuffer_Kind)
     return Ada.Containers.Hash_Type is
      function Value is new Ada.Unchecked_Conversion
        (Source => Low_Level.Enums.Renderbuffer_Kind, Target => Low_Level.Enum);
   begin
      return Ada.Containers.Hash_Type (Value (Key));
   end Hash;

   package Renderbuffer_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type     => Low_Level.Enums.Renderbuffer_Kind,
       Element_Type => Renderbuffer'Class,
       Hash         => Hash,
       Equivalent_Keys => Low_Level.Enums."=");
   use type Renderbuffer_Maps.Cursor;

   Current_Renderbuffers : Renderbuffer_Maps.Map;

   procedure Allocate (Object : Renderbuffer_Target;
                       Format : Pixels.Internal_Format;
                       Width, Height : Size;
                       Samples : Size := 0) is
   begin
      if Samples = 0 then
         API.Renderbuffer_Storage (Object.Kind, Format, Width, Height);
         Raise_Exception_On_OpenGL_Error;
      else
         API.Renderbuffer_Storage_Multisample (Object.Kind, Samples, Format,
                                               Width, Height);
      end if;
   end Allocate;

   function Width  (Object : Renderbuffer_Target) return Size is
      Value : Int := 0;
   begin
      API.Get_Renderbuffer_Parameter_Int
        (Object.Kind, Enums.Getter.Width, Value);
      return Value;
   end Width;

   function Height (Object : Renderbuffer_Target) return Size is
      Value : Int := 0;
   begin
      API.Get_Renderbuffer_Parameter_Int
        (Object.Kind, Enums.Getter.Height, Value);
      return Value;
   end Height;

   function Internal_Format (Object : Renderbuffer_Target)
                             return Pixels.Internal_Format is
      Value : Pixels.Internal_Format := Pixels.Internal_Format'First;
   begin
      API.Get_Renderbuffer_Parameter_Internal_Format
        (Object.Kind, Enums.Getter.Internal_Format, Value);
      return Value;
   end Internal_Format;

   function Red_Size (Object : Renderbuffer_Target) return Size is
      Value : Int := 0;
   begin
      API.Get_Renderbuffer_Parameter_Int
        (Object.Kind, Enums.Getter.Green_Size, Value);
      return Value;
   end Red_Size;

   function Green_Size (Object : Renderbuffer_Target) return Size is
      Value : Int := 0;
   begin
      API.Get_Renderbuffer_Parameter_Int
        (Object.Kind, Enums.Getter.Green_Size, Value);
      return Value;
   end Green_Size;

   function Blue_Size (Object : Renderbuffer_Target) return Size is
      Value : Int := 0;
   begin
      API.Get_Renderbuffer_Parameter_Int
        (Object.Kind, Enums.Getter.Blue_Size, Value);
      return Value;
   end Blue_Size;

   function Alpha_Size (Object : Renderbuffer_Target) return Size is
      Value : Int := 0;
   begin
      API.Get_Renderbuffer_Parameter_Int
        (Object.Kind, Enums.Getter.Alpha_Size, Value);
      return Value;
   end Alpha_Size;

   function Depth_Size (Object : Renderbuffer_Target) return Size is
      Value : Int := 0;
   begin
      API.Get_Renderbuffer_Parameter_Int
        (Object.Kind, Enums.Getter.Depth_Size, Value);
      return Value;
   end Depth_Size;

   function Stencil_Size (Object : Renderbuffer_Target) return Size is
      Value : Int := 0;
   begin
      API.Get_Renderbuffer_Parameter_Int (Object.Kind,
                                          Enums.Getter.Stencil_Size,
                                          Value);
      return Value;
   end Stencil_Size;

   function Raw_Kind (Object : Renderbuffer_Target)
                      return Low_Level.Enums.Renderbuffer_Kind is
   begin
      return Object.Kind;
   end Raw_Kind;

   procedure Bind (Target : Renderbuffer_Target; Object : Renderbuffer'Class) is
      Cursor : constant Renderbuffer_Maps.Cursor
        := Current_Renderbuffers.Find (Target.Kind);
   begin
      if Cursor = Renderbuffer_Maps.No_Element or else
        Renderbuffer_Maps.Element
          (Cursor).Reference.GL_Id /= Object.Reference.GL_Id
        then
         API.Bind_Renderbuffer (Target.Kind, Object.Reference.GL_Id);
         Raise_Exception_On_OpenGL_Error;
         if Cursor = Renderbuffer_Maps.No_Element then
            Current_Renderbuffers.Insert (Target.Kind, Object);
         else
            Current_Renderbuffers.Replace_Element (Cursor, Object);
         end if;
      end if;
   end Bind;

   function Current (Target : Renderbuffer_Target) return Renderbuffer'Class is
      Cursor : constant Renderbuffer_Maps.Cursor
        := Current_Renderbuffers.Find (Target.Kind);
   begin
      if Cursor = Renderbuffer_Maps.No_Element then
         raise No_Object_Bound_Exception with Target.Kind'Img;
      else
         return Renderbuffer_Maps.Element (Cursor);
      end if;
   end Current;

   procedure Initialize_Id (Object : in out Renderbuffer) is
      New_Id : UInt := 0;
   begin
      API.Gen_Renderbuffers (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   procedure Delete_Id (Object : in out Renderbuffer) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Renderbuffers (1, Arr);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

end GL.Objects.Renderbuffers;
