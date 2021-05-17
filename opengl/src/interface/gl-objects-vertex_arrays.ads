--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package GL.Objects.Vertex_Arrays is

   type Vertex_Array_Object is new GL_Object with private;

   procedure Bind (Object : Vertex_Array_Object);

   procedure Draw_Arrays (Mode : Connection_Mode; First, Count : Size);

   procedure Draw_Arrays_Instanced (Mode : Connection_Mode;
                                    First, Count, Instances : Size);

   function Current_Array_Object return Vertex_Array_Object;

   --  bind this object to unbind the current array object.
   Null_Array_Object : constant Vertex_Array_Object;

   --  Specifies the vertex array element index used to indicate that a new
   --  primitive should be started during rendering. When processing of
   --  vertex-array element indices encounters a value that matches index, no
   --  vertex data is processed, the current graphics primitive is terminated,
   --  and a new one of the identical type is started from the next vertex.
   procedure Set_Primitive_Restart_Index (Index : UInt);
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
