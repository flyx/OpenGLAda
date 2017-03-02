
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;

with SOIL;
with SOIL.Images;

with Vertex_Data;

package body My_Buffers is

    Image_Error : exception;

    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (Vertex_Data.pVertex_Pointers);
    procedure Load_Element_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (Vertex_Data.pElement_Pointers);

    --  ------------------------------------------------------------------------

    procedure Load_Image (Texture  : in out GL.Objects.Textures.Texture;
                          Image_File_Name : String) is
        use GL.Objects.Textures.Targets;
        use GL.Pixels;
        Width       : GL.Types.Size;
        Height      : GL.Types.Size;
        File_Format : SOIL.Image_Save_Type := SOIL.TGA;
        Image       : SOIL.Images.Image;
    begin
        Image.Initialize;
        Image.Load (Image_File_Name);
        if not SOIL.Images.Loaded (Image) then
            Put_Line ("Load_Image; " & Image_File_Name & " failed to load.");
            raise Image_Error;
        end if;
        Width := SOIL.Images.Width (Image);
        Height := SOIL.Images.Height (Image);
        Put_Line ("Load_Image; Width: " & GL.Types.Size'Image (Width));
        Put_Line ("Load_Image; Height: " & GL.Types.Size'Image (Height));
        Texture.Initialize_Id;
        Texture_2D.Bind (Texture);
        Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
        Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);

        Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
        Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

        Texture_2D.Load_From_Data (Level         => 0, Internal_Format => RGB8,
                                   Width         => Width, Height => Height,
                                   Source_Format => RGBA,
                                   Source_Type   => Unsigned_Byte,
                                   Source        => Image.Data);
    exception
        when others =>
            Put_Line ("An exceptiom occurred in Load_Image.");
            raise;
    end Load_Image;

    --  ------------------------------------------------------------------------

    procedure Make_Buffer (Target : GL.Objects.Buffers.Buffer_Target;
                           Buffer : in out tBuffer) is
    begin
        Buffer.Initialize_Id;
        Target.Bind (Buffer);
    end Make_Buffer;

    --  ------------------------------------------------------------------------

    procedure Setup_Buffers (Vertex_Buffer  : in out  tBuffer;
                             Element_Buffer : in out  tBuffer) is
        use GL.Objects.Buffers;
        use GL.Objects.Textures.Targets;

        Single_Size   : GL.Types.Size := GL.Types.Single'Size;
        Stride        :  GL.Types.Size := 5;
    begin
        Make_Buffer (GL.Objects.Buffers.Array_Buffer, Vertex_Buffer);
        Vertex_Data.Load_Vertex_Buffer (Array_Buffer, Vertex_Data.Vertices, Static_Draw);

        Make_Buffer (GL.Objects.Buffers.Element_Array_Buffer, Element_Buffer);
        Vertex_Data.Load_Element_Buffer (Element_Array_Buffer,
                                         Vertex_Data.Elements, Static_Draw);

        GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 2,
                                                 Kind   => GL.Types.Single_Type,
                                                 Stride => Stride, Offset => 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup_Buffers.");
            raise;
    end Setup_Buffers;

    --  ------------------------------------------------------------------------

    procedure Setup_Textures (Texture : in out GL.Objects.Textures.Texture;
                              Image_File_Name : String) is
        use GL.Objects.Textures.Targets;
    begin
        Load_Image (Texture, Image_File_Name);
        Put_Line ("Setup_Textures; image " & Image_File_Name & "  loaded.");

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup_Textures.");
            raise;
    end Setup_Textures;

end My_Buffers;
