
with GL.Objects.Buffers;
with GL.Types;

with FT.Interfac;

package Texture_Manager is

   subtype V_Buffer is GL.Objects.Buffers.Buffer;
   --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
   subtype Vertex_Array is GL.Types.Singles.Vector4_Array (1 .. 6);

   procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer;
                  Character_Data : in out FT.Interfac.Character_Data_Vector);

end Texture_Manager;
