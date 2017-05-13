
--  This version of DDS loading procedure only supports compressed
--  DDS files.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;
with GL.Objects.Textures.Targets;
with GL.Pixels;

--  Based on SOIL.adb  Direct_Load_DDS
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

    procedure Load_DDS_Header (File_ID    : Ada.Streams.Stream_IO.File_Type;
                               Header : out DDS_Header);
    procedure Load_DDS_Data (Data_Stream  : Ada.Streams.Stream_IO.Stream_Access;
                             Header       : DDS_Header;
                             theTexture   : out GL.Objects.Textures.Texture);

    procedure Load_Mipmaps (Data_Stream   : Ada.Streams.Stream_IO.Stream_Access;
                            Header        : DDS_Header;
                            Block_Size    : GL.Types.UInt;
                            Format        : GL.Pixels.Internal_Format);
    function Test_DW_Four_CC (DW_Four_CC : String) return boolean;

    --  ------------------------------------------------------------------------

    procedure Load_DDS_Data (Data_Stream   : Ada.Streams.Stream_IO.Stream_Access;
                             Header        : DDS_Header;
                             theTexture    : out GL.Objects.Textures.Texture) is
        use Ada.Streams.Stream_IO;
        use GL.Objects.Textures;
        use GL.Pixels;
        use GL.Types;

        Block_Size   : UInt;
        Format       : GL.Pixels.Internal_Format;
    begin
        if Header.Four_CC = "DXT1" then
            Format := Compressed_RGBA_S3TC_DXT1;
        elsif Header.Four_CC = "DXT3" then
            Format := Compressed_RGBA_S3TC_DXT3;
        elsif Header.Four_CC = "DXT5" then
            Format := Compressed_RGBA_S3TC_DXT5;
        else
            Put_Line ("Load_DSS_Data; Invalid S3TC_Type. Four_CC: "
                      & Header.Four_CC);
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

        Load_Mipmaps (Data_Stream, Header, Block_Size, Format);

    exception
        when others =>
            Put_Line ("An exception occurred in Load_DDS_Data.");
            raise;
    end Load_DDS_Data;

    --  ------------------------------------------------------------------------

    procedure Load_DDS_Header (File_ID : Ada.Streams.Stream_IO.File_Type;
                               Header  : out DDS_Header) is
        use Ada.Streams.Stream_IO;
        use GL.Types;

        Header_Stream   : Stream_Access := Stream (File_ID);
    begin
        String'Read (Header_Stream, Header.Magic);
        Set_Index (File_ID, Index (File_ID) + 8);
        UInt'Read (Header_Stream, Header.Height);
        UInt'Read (Header_Stream, Header.Width);
        UInt'Read (Header_Stream, Header.Linear_Size);
        Set_Index (File_ID, Index (File_ID) + 4);
        UInt'Read (Header_Stream, Header.Mip_Map_Count);
        Set_Index (File_ID, Index (File_ID) + 52);
        String'Read (Header_Stream, Header.Four_CC);

        if Header.Magic /= "DDS " then
            Put_Line ("Load_DSS_Header; File is not a DDS file");
            raise Header_Error;
        end if;

        if not Test_DW_Four_CC (Header.Four_CC) then
            Put_Line ("Load_DSS_Header; invalid FourCC: " & Header.Four_CC);
            raise Header_Error;
        end if;

    exception
        when others =>
            Put_Line ("An exception occurred in Load_DDS_Header.");
            raise;
    end Load_DDS_Header;

    --  ------------------------------------------------------------------------

    procedure Load_Mipmaps (Data_Stream   : Ada.Streams.Stream_IO.Stream_Access;
                            Header        : DDS_Header;
                            Block_Size    : GL.Types.UInt;
                            Format        : Gl.Pixels.Internal_Format) is
        use Ada.Streams.Stream_IO;
        use GL.Objects.Textures;
        use GL.Types;

        Width        : Int := Int (Header.Width);
        Height       : Int := Int (Header.Height);
        Mip_Size     : UInt := UInt (((Width + 3) / 4) * ((Height + 3) / 4)) * Block_Size;
        Level        : Int := 0;
        Continue     : Boolean := Width > 1 and then Height > 1;
    begin
        while Continue and then Level < Int (Header.Mip_Map_Count) loop
            Mip_Size := UInt (((Width + 3) / 4) * ((Height + 3) / 4)) * Block_Size;
            --  Load Compressed_Tex_Image_2D into the 2D texture
            declare
                Mip_Data : DDS_Data (1 .. Mip_Size);
            begin
                DDS_Data'Read (Data_Stream, Mip_Data);
                Targets.Texture_2D.Load_Compressed (Level, Format, Width, Height,
                                                    Int (Mip_Size),
                                                    Image_Source (Mip_Data'Address));
            end;

            Continue :=  Width > 1 and then Height > 1;
            if Continue then
                Level := Level + 1;
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
            raise;
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

    File_ID      : Ada.Streams.Stream_IO.File_Type;
    Data_Stream  : Ada.Streams.Stream_IO.Stream_Access;
    Header       : DDS_Header;
begin
    Open (File_ID, In_File, File_Name);
    Load_DDS_Header (File_ID, Header);
    if UInt (Ada.Streams.Stream_IO.Size (File_ID)) <= Header'Size then
        Put_Line ("Load_DDS; The file " & File_Name & " is too small.");
        raise Header_Error;
    else
        Set_Index (File_ID, Ada.Streams.Stream_IO.Count (Header.Byte_Size) + 1);
        Data_Stream := Stream (File_ID);
        Load_DDS_Data (Data_Stream, Header, theTexture);
    end if;

exception
    when anError : Ada.IO_Exceptions.Name_Error  =>
        --  File not found
        Put_Line ("Load_DDS can't find the file " & File_Name & "!");
        raise;
    when others =>
        Put_Line ("An exception occurred in Load_DDS.");
        raise;
end Load_DDS;
