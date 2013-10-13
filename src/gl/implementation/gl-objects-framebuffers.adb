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

package body GL.Objects.Framebuffers is
   ------------

   function Status (Target : Framebuffer_Target) return Framebuffer_Status is
   begin
      return API.Check_Framebuffer_Status (Target.Kind);
   end Status;

   procedure Attach_Renderbuffer (Target : in out Framebuffer_Target;
                                  Attachment : Attachment_Point;
                                  Object : Renderbuffers.Renderbuffer'Class) is
   begin
      API.Framebuffer_Renderbuffer (Target.Kind, Attachment,
                                    Low_Level.Enums.Renderbuffer,
                                    Object.Raw_Id);
      Check_OpenGL_Error;
   end Attach_Renderbuffer;

   procedure Attach_Texture (Target : in out Framebuffer_Target;
                             Attachment : Attachment_Point;
                             Object : Textures.Texture'Class;
                             Level  : Textures.Mipmap_Level) is
   begin
      API.Framebuffer_Texture (Target.Kind, Attachment, Object.Raw_Id, Level);
      Check_OpenGL_Error;
   end Attach_Texture;

   procedure Invalidate (Target : in out Framebuffer_Target;
                         Attachments : Attachment_List) is
   begin
      API.Invalidate_Framebuffer (Target.Kind, Attachments'Length, Attachments);
      Check_OpenGL_Error;
   end Invalidate;

   procedure Blit (Src_X0, Src_Y0, Src_X1, Src_Y1,
                   Dst_X0, Dst_Y0, Dst_X1, Dst_Y1 : Int;
                   Mask : Buffers.Buffer_Bits;
                   Filter : Textures.Magnifying_Function) is
      use type Low_Level.Bitfield;
      function Convert is new Ada.Unchecked_Conversion
        (Buffers.Buffer_Bits, Low_Level.Bitfield);
      Raw_Bits : Low_Level.Bitfield := Convert (Mask) and 2#0100010100000000#;

   begin
      API.Blit_Framebuffer (Src_X0, Src_Y0, Src_X1, Src_Y1,
                            Dst_X0, Dst_Y0, Dst_X1, Dst_Y1, Raw_Bits, Filter);
      Check_OpenGL_Error;
   end Blit;


   procedure Initialize_Id (Object : in out Framebuffer) is
      New_Id : UInt := 0;
   begin
      API.Gen_Framebuffers (1, New_Id);
      Check_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   procedure Delete_Id (Object : in out Framebuffer) is
      Arr : Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Framebuffers (1, Arr);
      Check_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;


   function Hash (Key : Low_Level.Enums.Framebuffer_Kind)
     return Ada.Containers.Hash_Type is
      function Value is new Ada.Unchecked_Conversion
        (Source => Low_Level.Enums.Framebuffer_Kind, Target => Low_Level.Enum);
   begin
      return Ada.Containers.Hash_Type (Value (Key));
   end Hash;

   package Framebuffer_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type     => Low_Level.Enums.Framebuffer_Kind,
       Element_Type => Framebuffer'Class,
       Hash         => Hash,
       Equivalent_Keys => Low_Level.Enums."=");
   use type Framebuffer_Maps.Cursor;

   Current_Framebuffers : Framebuffer_Maps.Map;

   type Framebuffer_Kind_Array is array (Positive range <>) of
     Low_Level.Enums.Framebuffer_Kind;

   function Backend_Framebuffer_Targets
     (Kind : Low_Level.Enums.Framebuffer_Kind) return Framebuffer_Kind_Array is
   begin
      case Kind is
         when Low_Level.Enums.Read => return (1 => Low_Level.Enums.Read);
         when Low_Level.Enums.Draw => return (1 => Low_Level.Enums.Draw);
         when Low_Level.Enums.Read_Draw =>
            return (1 =>Low_Level.Enums.Draw, 2 => Low_Level.Enums.Read);
      end case;
   end Backend_Framebuffer_Targets;
   pragma Inline (Backend_Framebuffer_Targets);

   procedure Bind (Target : in out Framebuffer_Target;
                   Object : Framebuffer'Class) is
      -- Read_Draw bind to both read and draw framebuffer, we need to set
      -- the current framebuffer objects accordingly.
      Targets : Framebuffer_Kind_Array
        := Backend_Framebuffer_Targets (Target.Kind);

      Cursor : Framebuffer_Maps.Cursor;
   begin
      API.Bind_Framebuffer (Target.Kind, Object.Reference.GL_Id);
      Check_OpenGL_Error;
      for Index in Targets'Range loop
         Cursor := Current_Framebuffers.Find (Targets (Index));
         if Cursor = Framebuffer_Maps.No_Element then
            Current_Framebuffers.Insert (Target.Kind, Object);
         elsif Framebuffer_Maps.Element (Cursor).Reference.GL_Id
           /= Object.Reference.GL_Id then
            Current_Framebuffers.Replace_Element (Cursor, Object);
         end if;
      end loop;
   end Bind;

   function Current (Target : Framebuffer_Target) return Framebuffer'Class is
      Targets : Framebuffer_Kind_Array
        := Backend_Framebuffer_Targets (Target.Kind);

      -- If target is Read_Draw, return the draw framebuffer
      -- (Note: this is necessary because distinct read/draw framebuffers
      -- were added later to the API and therefore might not be available
      -- in the context. So everything needs to work with just Read_Draw).
      Cursor : Framebuffer_Maps.Cursor
        := Current_Framebuffers.Find (Targets (1));
   begin
      if Cursor = Framebuffer_Maps.No_Element then
         raise No_Object_Bound_Exception with Target.Kind'Img;
      else
         return Framebuffer_Maps.Element (Cursor);
      end if;
   end Current;

end GL.Objects.Framebuffers;
