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

private with GL.Low_Level.Enums;

package GL.Objects.Buffer is
   
   type Buffer_Target (<>) is tagged private;
   
   type Buffer_Object is abstract new GL_Object with private;
   
   procedure Bind (Target : Buffer_Target; Object : Buffer_Object'Class);
   
   Array_Buffer              : constant Buffer_Target;
   Element_Array_Buffer      : constant Buffer_Target;
   Pixel_Pack_Buffer         : constant Buffer_Target;
   Pixel_Unpack_Buffer       : constant Buffer_Target;
   Uniform_Buffer            : constant Buffer_Target;
   Texture_Buffer            : constant Buffer_Target;
   Transform_Feedback_Buffer : constant Buffer_Target;
   Copy_Read_Buffer          : constant Buffer_Target;
   Copy_Write_Buffer         : constant Buffer_Target;
   Draw_Indirect_Buffer      : constant Buffer_Target;
   Atomic_Counter_Buffer     : constant Buffer_Target;
   
private
   type Buffer_Target (Kind : Low_Level.Enums.Buffer_Kind) is tagged null record;

   type Buffer_Object is new GL_Object with null record;

   overriding procedure Create_Id (Object : in out Buffer_Object);
   
   overriding procedure Delete_Id (Object : in out Buffer_Object);
   
   Array_Buffer              : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Array_Buffer);
   Element_Array_Buffer      : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Element_Array_Buffer);
   Pixel_Pack_Buffer         : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Pixel_Pack_Buffer);
   Pixel_Unpack_Buffer       : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Pixel_Unpack_Buffer);
   Uniform_Buffer            : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Uniform_Buffer);
   Texture_Buffer            : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Texture_Buffer);
   Transform_Feedback_Buffer : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Transform_Feedback_Buffer);
   Copy_Read_Buffer          : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Copy_Read_Buffer);
   Copy_Write_Buffer         : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Copy_Write_Buffer);
   Draw_Indirect_Buffer      : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Draw_Indirect_Buffer);
   Atomic_Counter_Buffer     : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Atomic_Counter_Buffer);
end GL.Objects.Buffer;