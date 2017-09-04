
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Textures;
with GL.Types;

with FT.API;

package Texture_Manager is

   subtype V_Buffer is GL.Objects.Buffers.Buffer;
   --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
   subtype Vertex_Array is GL.Types.Singles.Vector4_Array (1 .. 6);

   procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer;
                            aTexture      : in out GL.Objects.Textures.Texture;
                            X, Y          : GL.Types.Single;
                            Scale         : GL.Types.Single;
                            Char          : Character := 'g');
end Texture_Manager;
