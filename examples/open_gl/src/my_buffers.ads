
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Textures;

package My_Buffers is

    subtype tBuffer is GL.Objects.Buffers.Buffer;
    subtype tBuffer_List is GL.Buffers.Explicit_Color_Buffer_List;

    procedure Setup_Buffers (Vertex_Buffer  : in out  tBuffer;
                             Element_Buffer : in out  tBuffer);
    procedure Setup_Textures (Texture       : in out GL.Objects.Textures.Texture;
                              Image_File_Name : String);
end My_Buffers;
