
with System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;
with GL.Objects.Buffers;
with GL.Objects.Textures.Targets;
with GL.Pixels;

--  Extracted from SOIL.adb  Direct_Load_DDS
procedure Load_DDS (File_Name  : String;
                    theTexture : out GL.Objects.Textures.Texture) is
    type DDS_Data is array (GL.Types.UInt range <>) of GL.Types.UByte;

    type DDS_Header is record
        Magic          : String (1 .. 4);
        Height         : GL.Types.UInt;
        Width          : GL.Types.UInt;
        Linear_Size    : GL.Types.UInt;
        Mip_Map_Count  : GL.Types.UInt;
        Four_CC        : String (1 .. 4);
        Byte_Size      : GL.Types.UInt := 128; --  bytes
    end record;

    -- -------------------------------------------------------------------------

    Header_Error : exception;
    Image_Error  : exception;

    -- -------------------------------------------------------------------------

    procedure Load_DDS_Header (File_ID     : Ada.Streams.Stream_IO.File_Type;
                               File_Size   : GL.Types.UInt;
                               Header      : out DDS_Header);
    procedure Load_DDS_Data (File_ID      : Ada.Streams.Stream_IO.File_Type;
                             File_Size    : GL.Types.UInt;
                             Header       : DDS_Header;
                             theTexture   : out GL.Objects.Textures.Texture);

    procedure Load_Mipmaps (Header                        : DDS_Header;
                            Data                          : DDS_Data;
                            Block_Size                    : GL.Types.UInt;
                            Initial_Width, Initial_Height : GL.Types.UInt;
                            Format                        : GL.Pixels.Internal_Format);
    function Test_DW_Four_CC (DW_Four_CC : String) return boolean;

    --  ------------------------------------------------------------------------

    procedure Load_DDS_Data (File_ID     : Ada.Streams.Stream_IO.File_Type;
                             File_Size   : GL.Types.UInt;
                             Header      : DDS_Header;
                             theTexture  : out GL.Objects.Textures.Texture) is
        use Ada.Streams.Stream_IO;
        use GL.Objects.Textures;
        use GL.Pixels;
        use GL.Types;

        Data_Stream       : Stream_Access := Stream (File_ID);
        Block_Size        : UInt;
        Four_CC           : String (1 .. 4);
        Format            : GL.Pixels.Internal_Format;
        Data_Size         : UInt := File_Size - Uint (Index (File_ID));
        Data              : DDS_Data (1 .. GL.Types.UInt (Data_Size));  -- array of bytes
    begin
        Four_CC := Header.Four_CC;
        if Header.Four_CC = "DXT1" then
            Format := Compressed_RGBA_S3TC_DXT1;
        elsif Header.Four_CC = "DXT3" then
            Format := Compressed_RGBA_S3TC_DXT3;
        elsif Header.Four_CC = "DXT5" then
            Format := Compressed_RGBA_S3TC_DXT5;
        else
            Put_Line ("Load_DSS_Data; Invalid S3TC_Type. Four_CC: "
                      & Four_CC);
            raise  Image_Error;
        end if;

        theTexture.Initialize_Id;
        Targets.Texture_2d.Bind (theTexture);
        GL.Pixels.Set_Pack_Alignment (GL.Pixels.Bytes);

        if Format = GL.Pixels.Compressed_RGBA_S3TC_DXT1 then
            Block_Size := 8;
        else
            Block_Size := 16;
        end if;
        DDS_Data'Read (Data_Stream, Data (1 .. File_Size - Uint (Index (File_ID))));
        Load_Mipmaps (Header, Data, Block_Size, Header.Width,
                      Header.Height, Format);
    exception
        when others =>
            Put_Line ("An exception occurred in Load_DDS_Data.");
            raise Image_Error;
    end Load_DDS_Data;

    --  ------------------------------------------------------------------------

    procedure Load_DDS_Header (File_ID   : Ada.Streams.Stream_IO.File_Type;
                               File_Size : GL.Types.UInt;
                               Header    : out DDS_Header) is
        use GL.Types;
        use Ada.Streams.Stream_IO;
        Header_Stream   : Stream_Access := Stream (File_ID);

    begin
        if File_Size > Header.Byte_Size then
            String'Read (Header_Stream, Header.Magic);
            if Header.Magic /= "DDS " then
                Put_Line ("Load_DSS_Header; File is not a DDS file");
                raise Header_Error;
            end if;

            Set_Index (File_ID, Index (File_ID) + 8);
            UInt'Read (Header_Stream, Header.Height);
            UInt'Read (Header_Stream, Header.Width);
            UInt'Read (Header_Stream, Header.Linear_Size);
            Set_Index (File_ID, Index (File_ID) + 4);
            UInt'Read (Header_Stream, Header.Mip_Map_Count);
            Set_Index (File_ID, Index (File_ID) + 52);
            String'Read (Header_Stream, Header.Four_CC);

            if not Test_DW_Four_CC (Header.Four_CC) then
                Put_Line ("Load_DSS_Header; invalid FourCC: " & Header.Four_CC);
                raise Header_Error;
            end if;
        else
            Put_Line ("Load_DSS_Header; This file is not a DDS file.");
            raise Header_Error;
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Load_DDS_Header.");
            raise;
    end Load_DDS_Header;

    --  ------------------------------------------------------------------------

    procedure Load_Mipmaps (Header                        : DDS_Header;
                            Data                          : DDS_Data;
                            Block_Size                    : GL.Types.UInt;
                            Initial_Width, Initial_Height : GL.Types.UInt;
                            Format                        : Gl.Pixels.Internal_Format) is
        use GL.Objects.Textures;
        use GL.Types;
        Width        : UInt := Initial_Width;
        Height       : UInt := Initial_Height;
        Mip_Size     : UInt := ((Width + 3) / 4) * ((Height + 3) / 4) * Block_Size;
        Data_Index   : UInt := 1;
        Level        : UInt := 0;
        Continue     : Boolean := Width > 1 and then Height > 1;
    begin
        while Continue and then Level < Header.Mip_Map_Count loop
            Mip_Size := ((Width + 3) / 4) * ((Height + 3) / 4) * Block_Size;
            --  Load Compressed_Tex_Image_2D into the 2D texture
            Targets.Texture_2D.Load_Compressed (Int (Level), Format,
                                                Int (Width), Int (Height), Int (Mip_Size),
                                                Image_Source (Data (Data_Index)'Address));

            Continue :=  Width > 1 and then Height > 1;
            if Continue then
                Level := Level + 1;
                Data_Index := Data_Index + Mip_Size;
                Width := Width / 2;
                Height := Height / 2;
                --  Deal with Non-Power-Of-Two textures.
                if Width < 1 then
                    Width := 1;
                end if;
                if Height < 1 then
                    Height := 1;
                end if;
            end if;
        end loop;
    exception
        when others =>
            Put_Line ("An exception occurred in Load_Mipmaps.");
            raise Image_Error;
    end Load_Mipmaps;

    --  ------------------------------------------------------------------------

    function Test_DW_Four_CC (DW_Four_CC : String) return boolean is
    begin
        return DW_Four_CC = "DXT1" or else
          DW_Four_CC =  "DXT3" or else
          DW_Four_CC =  "DXT5";
    end Test_DW_Four_CC;

    --  ------------------------------------------------------------------------

    use GL.Types;
    use Ada.Streams.Stream_IO;

    File_ID       : Ada.Streams.Stream_IO.File_Type;
    File_Length   : UInt;
    Header        : DDS_Header;
begin
    Ada.Streams.Stream_IO.Open (File_ID, In_File, File_Name);
    File_Length := UInt (Ada.Streams.Stream_IO.Size (File_ID));

    Load_DDS_Header (File_ID, File_Length, Header);
    Load_DDS_Data (File_ID, File_Length, Header, theTexture);

exception
    when anError : Ada.IO_Exceptions.Name_Error  =>
        --  File not found
        Put_Line ("Load_DDS can't find the file " & File_Name & "!");
        raise;
    when others =>
        Put_Line ("An exception occurred in Load_DDS.");
        raise;
end Load_DDS;
