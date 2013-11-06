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

with GL.API;
with GL.Low_Level;

package body GL.Objects.Vertex_Arrays is

   Current_Object : Vertex_Array_Object := Null_Array_Object;

   procedure Bind (Object : Vertex_Array_Object) is
   begin
      if Object.Reference = null then
         API.Bind_Vertex_Array (0);
         Current_Object := Null_Array_Object;
      elsif Object /= Current_Array_Object then
         API.Bind_Vertex_Array (Object.Reference.GL_Id);
         Current_Object := Object;
      end if;
      Raise_Exception_On_OpenGL_Error;
   end Bind;

   procedure Draw_Arrays (Mode : Connection_Mode; First, Count : Size) is
   begin
      API.Draw_Arrays (Mode, First, Count);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Arrays;

   procedure Initialize_Id (Object : in out Vertex_Array_Object) is
      New_Id : UInt := 0;
   begin
      API.Gen_Vertex_Arrays (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;
   
   function Current_Array_Object return Vertex_Array_Object is
   begin
      return Current_Object;
   end Current_Array_Object;

   procedure Delete_Id (Object : in out Vertex_Array_Object) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Vertex_Arrays (1, Arr);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;
end GL.Objects.Vertex_Arrays;
