
with Ada.Strings.Unbounded;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Textures;

package My_Buffers is

    subtype tBuffer is GL.Objects.Buffers.Buffer;
    subtype tBuffer_List is GL.Buffers.Explicit_Color_Buffer_List;

    type tTexture_List is array (Natural range <>) of GL.Objects.Textures.Texture;
    type tImage_Sources is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

    procedure Setup_Buffers (Vertex_Buffer  : in out tBuffer;
                             Element_Buffer : in out tBuffer);
    procedure Setup_Textures (Textures      : in out tTexture_List;
                              Images        : tImage_Sources);
end My_Buffers;
