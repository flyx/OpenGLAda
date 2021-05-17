--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;

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

   procedure Draw_Arrays_Instanced (Mode : Connection_Mode;
                                    First, Count, Instances : Size) is
   begin
      API.Draw_Arrays_Instanced (Mode, First, Count, Instances);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Arrays_Instanced;

   procedure Set_Primitive_Restart_Index (Index : UInt) is
   begin
      API.Primitive_Restart_Index (Index);
      Raise_Exception_On_OpenGL_Error;
   end Set_Primitive_Restart_Index;

   overriding
   procedure Internal_Create_Id (Object : Vertex_Array_Object; Id : out UInt) is
      pragma Unreferenced (Object);
   begin
      API.Gen_Vertex_Arrays (1, Id);
      Raise_Exception_On_OpenGL_Error;
   end Internal_Create_Id;

   overriding
   procedure Internal_Release_Id (Object : Vertex_Array_Object; Id : UInt) is
      pragma Unreferenced (Object);
   begin
      API.Delete_Vertex_Arrays (1, (1 => Id));
      Raise_Exception_On_OpenGL_Error;
   end Internal_Release_Id;

   function Current_Array_Object return Vertex_Array_Object is
   begin
      return Current_Object;
   end Current_Array_Object;
end GL.Objects.Vertex_Arrays;
