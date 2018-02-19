--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package GL.Objects.Vertex_Arrays is

   type Vertex_Array_Object is new GL_Object with private;

   procedure Bind (Object : Vertex_Array_Object);

   procedure Draw_Arrays (Mode : Connection_Mode; First, Count : Size);

   function Current_Array_Object return Vertex_Array_Object;

   -- bind this object to unbind the current array object.
   Null_Array_Object : constant Vertex_Array_Object;
private
   type Vertex_Array_Object is new GL_Object with null record;

   overriding
   procedure Internal_Create_Id (Object : Vertex_Array_Object; Id : out UInt);

   overriding
   procedure Internal_Release_Id (Object : Vertex_Array_Object; Id : UInt);

   Null_Array_Object : constant Vertex_Array_Object
     := Vertex_Array_Object'(Ada.Finalization.Controlled with
        Reference => Reference_To_Null_Object'Access);
end GL.Objects.Vertex_Arrays;
