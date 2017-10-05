
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types; use  GL.Types;

with SOIL;
with SOIL.Images;

with Utilities;
with Vertex_Data;

package body My_Buffers is

    Image_Error : exception;

    --  ------------------------------------------------------------------------

    procedure Make_Texture (Texture  : in out GL.Objects.Textures.Texture;
                            Image_File_Name : String) is
        use GL.Objects.Textures.Targets;
        use GL.Pixels;
        Width       : GL.Types.Size;
        Height      : GL.Types.Size;
        File_Format : SOIL.Image_Save_Type := SOIL.TGA;
        Image       : SOIL.Images.Image;
    begin
        Image.Load (Image_File_Name);
        Width := SOIL.Images.Width (Image);
        Height := SOIL.Images.Height (Image);
        if not SOIL.Images.Loaded (Image) then
            Put_Line ("Load_Image; " & Image_File_Name & " failed to load.");
            raise Image_Error;
        end if;

        Texture.Initialize_Id;
        Texture_2D.Bind (Texture);
        Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
        Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

        Texture_2D.Load_From_Data  (0, RGB, Width, Height, RGB, Unsigned_Byte,
                                     Image.Data);
    exception
        when others =>
            Put_Line ("An exceptiom occurred in Make_Texture.");
            raise;
    end Make_Texture;

    --  ------------------------------------------------------------------------

    procedure Make_Buffer (Target : GL.Objects.Buffers.Buffer_Target;
                           Buffer : in out tBuffer) is
    begin
       Buffer.Initialize_Id;
       Target.Bind (Buffer);
    end Make_Buffer;

    --  ------------------------------------------------------------------------

    procedure Setup_Buffers (Vertex_Buffer  : in out tBuffer;
                             Element_Buffer : in out tBuffer) is
        use GL.Objects.Buffers;

        Stride : GL.Types.Size := GL.Types.Size (Vertex_Data.Vertex_Array_Size);
    begin
       Make_Buffer (GL.Objects.Buffers.Array_Buffer, Vertex_Buffer);
       Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data.Vertices, Static_Draw);

       Make_Buffer (GL.Objects.Buffers.Element_Array_Buffer, Element_Buffer);
       Vertex_Data.Load_Element_Buffer (Element_Array_Buffer,
                                        Vertex_Data.Elements, Static_Draw);

        GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                                 Kind   => GL.Types.Single_Type,
                                                 Stride => Stride, Offset => 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);

        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, GL.Types.Single_Type,
                                                 Stride, 3);
        GL.Attributes.Enable_Vertex_Attrib_Array (1);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup_Buffers.");
            raise;
    end Setup_Buffers;

    --  ------------------------------------------------------------------------

    procedure Setup_Textures (Textures : in out tTexture_List;
                              Images : tImage_Sources) is
        use Ada.Strings.Unbounded;
        Image_Index : Positive := 1;
    begin
        for tex of Textures loop
            Make_Texture (tex, To_String (Images (Image_Index)));
            Put_Line ("Setup_Textures; image " & To_String (Images (Image_Index)) & " loaded.");
            Image_Index := Image_Index + 1;
        end loop;
    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup_Textures.");
            raise;
    end Setup_Textures;

end My_Buffers;
