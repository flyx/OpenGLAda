
with System;
with Interfaces;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with GL.Types;
with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Objects.Textures.Targets;
with GL.Objects.Textures.With_2D_Loader;
with GL.Pixels;

with IO;

--  Extracted from SOIL.adb  Direct_Load_DDS
procedure Load_DDS (File_Name  : String;
                        theTexture : out GL.Objects.Textures.Texture) is
    type t4_UBytes is array (1 .. 4) of GL.Types.UByte;
    type t44_UBytes is array (1 .. 44) of GL.Types.UByte;
    type tDDS_Data is array (GL.Types.UInt range <>) of GL.Types.UByte;

    type tDDS_Header is record
        Magic          : String (1 .. 4);
        Height         : GL.Types.UInt;
        Width          : GL.Types.UInt;
        Linear_Size    : GL.Types.UInt;
        Mip_Map_Count  : GL.Types.UInt;
        Four_CC        : String (1 .. 4);
        Byte_Size      : GL.Types.UInt := 128; --  bytes
    end record;

    type tUByte_Array is array (GL.Types.UInt range <>) of aliased GL.Types.UByte;

    -- -------------------------------------------------------------------------

    Header_Error : exception;
    Image_Error  : exception;

    -- -------------------------------------------------------------------------

    function Byte_To_Character is new
      Ada.Unchecked_Conversion (Interfaces.Unsigned_8, Character);
    function Character_To_Unsigned_8 is new
      Ada.Unchecked_Conversion (Character, Interfaces.Unsigned_8);

    procedure Load_DSS_Header (Buffer : tUByte_Array; Header : out tDDS_Header);
    procedure Load_DSS_Data (Input_Buffer : tUByte_Array;
                             Buffer_Size  : GL.Types.UInt;
                             Header       : tDDS_Header;
                             theTexture   : out GL.Objects.Textures.Texture);
    procedure Load_Mipmaps (Header                        : tDDS_Header;  DDS_Data : tDDS_Data;
                            Block_Size                    : GL.Types.UInt;
                            Initial_Width, Initial_Height : GL.Types.UInt;
                            Format                        : GL.Pixels.Internal_Format);
    function Test_DW_Four_CC (DW_Four_CC : String) return boolean;

    --  ------------------------------------------------------------------------

    function Bytes_To_String (Bytes : t4_UBytes) return String is
        use Interfaces;
        theString : String (1 .. 4);
    begin
        for index in 1 .. 4 loop
            theString (Index) := Byte_To_Character
              (Interfaces.Unsigned_8 (Bytes (Index)));
        end loop;
        return theString;
    end Bytes_To_String;

    --  ------------------------------------------------------------------------

    function Bytes_To_UInt (Bytes : t4_UBytes) return GL.Types.UInt is
        use Interfaces;
    begin
        return GL.Types.Uint (Shift_Left (Unsigned_32 (Bytes (4)), 24) +
                                Shift_Left (Unsigned_32 (Bytes (3)), 16) +
                                Shift_Left (Unsigned_32 (Bytes (2)), 8) +
                                Unsigned_32 (Bytes (1)));
    end Bytes_To_UInt;

    -- -------------------------------------------------------------------------

    procedure Load_DSS_Data (Input_Buffer  : tUByte_Array;
                             Buffer_Size   : GL.Types.UInt;
                             Header        : tDDS_Header;
                             theTexture    : out GL.Objects.Textures.Texture) is
        use Interfaces;
        use GL.Objects.Textures;
        use GL.Pixels;
        use GL.Types;

        Block_Size        : UInt;
        Components        : UInt;
        Four_CC           : String (1 .. 4);
        Format            : GL.Pixels.Internal_Format;
        aTexture          : GL.Objects.Textures.Texture;
        Data_Size         : UInt;  --  including all mipmaps
    begin
        if Header.Four_CC = "DXT1" then
            Components := 3;
        else
            Components := 4;
        end if;

        Four_CC := Header.Four_CC;
        if Four_CC = "DXT1" then
            Format := Compressed_RGBA_S3TC_DXT1;
        elsif Four_CC = "DXT3" then
            Format := Compressed_RGBA_S3TC_DXT3;
        elsif Four_CC = "DXT5" then
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

        if Header.Mip_Map_Count > 1 then
            Data_Size := 2 * Header.Linear_Size;
        else
            Data_Size := Header.Linear_Size;
        end if;

        declare
            DDS_Data  : tDDS_Data (0 .. Data_Size - 1);
        begin
            for Index in GL.Types.UInt range Header.Byte_Size + 1 .. Buffer_Size loop
                DDS_Data (Index - Header.Byte_Size) := Input_Buffer (Index);
            end loop;

            Load_Mipmaps (Header, DDS_Data, Block_Size, Header.Width,
                          Header.Height, Format);
        end;  --  declare block
    end Load_DSS_Data;

    --  ------------------------------------------------------------------------

    procedure Load_DSS_Header (Buffer : tUByte_Array; Header : out tDDS_Header) is
        use Interfaces;
        use GL.Types;

        UInt_Size         : Natural := UInt'Size;
        Header_Byte_Size  : GL.Types.UInt := 128;
    begin
        Header.Magic := Bytes_To_String (t4_UBytes (Buffer (1 .. 4)));
        Header.Height := Bytes_To_UInt (t4_UBytes (Buffer (13 .. 16)));
        Header.Width := Bytes_To_UInt (t4_UBytes (Buffer (17 .. 20)));
        Header.Linear_Size := Bytes_To_UInt (t4_UBytes (Buffer (21 .. 24)));
        Header.Mip_Map_Count := Bytes_To_UInt (t4_UBytes (Buffer (29 .. 32)));
        Header.Four_CC := Bytes_To_String (t4_UBytes (Buffer (85 .. 88)));

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
            Put_Line ("An exception occurred in Load_DSS_Header.");
            raise;
    end Load_DSS_Header;

    --  ------------------------------------------------------------------------

    procedure Load_Mipmaps (Header                        : tDDS_Header; DDS_Data : tDDS_Data;
                            Block_Size                    : GL.Types.UInt;
                            Initial_Width, Initial_Height : GL.Types.UInt;
                            Format                        : Gl.Pixels.Internal_Format) is
        use GL.Objects.Textures;
        use GL.Types;
        Width        : UInt := Initial_Width;
        Height       : UInt := Initial_Height;
        Mip_Size     : UInt := ((Width + 3) / 4) * ((Height + 3) / 4) * Block_Size;
        Offset       : UInt := 0;
        Level        : UInt := 0;
        Continue     : Boolean := Width > 1 and then Height > 1;
    begin
        while Continue and then Level < Header.Mip_Map_Count loop
            Mip_Size := ((Width + 3) / 4) * ((Height + 3) / 4) * Block_Size;
            --  Load Compressed_Tex_Image_2D into the 2D texture
            Targets.Texture_2D.Load_Compressed (Int (Level), Format,
                              Int (Width), Int (Height), Int (Mip_Size),
                              Image_Source (DDS_Data (Offset)'Address));

            Continue :=  Width > 1 and then Height > 1;
            if Continue then
                Level := Level + 1;
                Offset := Offset + Mip_Size;
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
        use IO;

        File_ID         : Byte_IO.File_Type;
        Buffer_Length   : GL.Types.UInt := 0;
        Bytes_Read      : GL.Types.UInt := 0;
        Dump            : GL.Types.UByte;
        Texture_ID      : GL.Types.UInt := 0;
        Header          : tDDS_Header;
begin
    Byte_IO.Open (File_ID, Byte_IO.In_File, File_Name);
    --  Determine file length
    while not Byte_IO.End_Of_File (File_ID) loop
        Buffer_Length := Buffer_Length + 1;
        Read_UByte (File_ID, Dump);
    end loop;

    Byte_IO.Reset (File_ID);
    declare
        Input_Buffer : tUByte_Array (1 .. Buffer_Length);
    begin
        while not Byte_IO.End_Of_File (File_ID) loop
            Bytes_Read := Bytes_Read + 1;
            Read_UByte (File_ID, Input_Buffer (Bytes_Read));
        end loop;
        Byte_IO.Close (File_ID);

        Load_DSS_Header (Input_Buffer, Header);
        if Buffer_Length < Header'Size then
            Put_Line ("Load_DDS; The buffer is too small.");
            raise Header_Error;
        else
            Load_DSS_Data (Input_Buffer, Buffer_Length, Header, theTexture);
        end if;
    end;  --  declare

exception
    when anError : Ada.IO_Exceptions.Name_Error  =>
        --  File not found
        Put_Line ("Load_DDS can't find the file " & File_Name & "!");
        raise;
    when others =>
        Put_Line ("An exception occurred in Load_DDS.");
        raise;
end Load_DDS;
