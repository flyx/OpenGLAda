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

with GL.API;
with GL.Low_Level;

package body GL.Objects.Vertex_Arrays is

   procedure Bind (Object : Vertex_Array_Object) is
   begin
      if (Object.Reference = null) then
         API.Bind_Vertex_Array (0);
      else
         API.Bind_Vertex_Array (Object.Reference.GL_Id);
      end if;
      Check_OpenGL_Error;
   end Bind;

   procedure Draw_Arrays (Mode : Connection_Mode; First, Count : Natural) is
   begin
      API.Draw_Arrays (Mode, Int (First), Low_Level.SizeI (Count));
      Check_OpenGL_Error;
   end Draw_Arrays;

   procedure Create_Id (Object : in out Vertex_Array_Object) is
      New_Id : UInt := 0;
   begin
      API.Gen_Vertex_Arrays (1, New_Id);
      Check_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
   end Create_Id;

   procedure Delete_Id (Object : in out Vertex_Array_Object) is
      Arr : Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      if Object.Reference.GL_Id /= 0 then
         API.Delete_Vertex_Arrays (1, Arr);
      end if;
      Check_OpenGL_Error;
   end Delete_Id;
end GL.Objects.Vertex_Arrays;
