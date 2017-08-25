
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Textures;
with GL.Types;

package Texture_Manager is

    subtype V_Buffer is GL.Objects.Buffers.Buffer;

    procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer;
                             aTexture      : in out GL.Objects.Textures.Texture;
                             X, Y: GL.Types.Single; Scale : GL.Types.Single := 1.0);
end Texture_Manager;
