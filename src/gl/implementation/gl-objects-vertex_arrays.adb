--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

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

   procedure Destructor (Reference : not null GL_Object_Reference_Access) is
      Arr : constant Low_Level.UInt_Array := (1 => Reference.GL_Id);
   begin
      API.Delete_Vertex_Arrays (1, Arr);
      Raise_Exception_On_OpenGL_Error;
      Reference.GL_Id := 0;
      Reference.Initialized := Uninitialized;
   end Destructor;

   procedure Initialize_Id (Object : in out Vertex_Array_Object) is
      New_Id : UInt := 0;
   begin
      API.Gen_Vertex_Arrays (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := Allocated;
      Object.Reference.Destructor := Destructor'Access;
   end Initialize_Id;

   function Current_Array_Object return Vertex_Array_Object is
   begin
      return Current_Object;
   end Current_Array_Object;
end GL.Objects.Vertex_Arrays;
