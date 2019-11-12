--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;

package body GL.Objects.Queries is

   Current_Object : Query_Object := Null_Array_Object;

   overriding
   procedure Internal_Create_Id (Object : Query_Object; Id : out UInt) is
      pragma Unreferenced (Object);
   begin
      Gen_Query_Arrays (1, Id);
      Raise_Exception_On_OpenGL_Error;
   end Internal_Create_Id;

   overriding
   procedure Internal_Release_Id (Object : Query_Object; Id : UInt) is
      pragma Unreferenced (Object);
   begin
      API.Delete_Query_Object (1, (1 => Id));
      Raise_Exception_On_OpenGL_Error;
   end Internal_Release_Id;

   function Current_Query_Object return Query_Object is
   begin
      return Current_Object;
   end Current_Query_Object;

end GL.Objects.Queries;
