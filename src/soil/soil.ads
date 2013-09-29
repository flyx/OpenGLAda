--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Interfaces.C;

with GL.Objects.Textures;
with GL.Types;

package SOIL is
   pragma Preelaborate;

   type Image_Format is (Auto, L, LA, RGB, RGBA);
   -- defined here because of the following subtype
   for Image_Format use (Auto => 0,
                         L    => 1,
                         LA   => 2,
                         RGB  => 3,
                         RGBA => 4);
   for Image_Format'Size use Interfaces.C.int'Size;

   subtype Explicit_Image_Format is Image_Format range L .. RGBA;

   type Texture_Flags is record
      Power_Of_Two      : Boolean;
      Mipmaps           : Boolean;
      Texture_Repeats   : Boolean;
      Multiply_Alpha    : Boolean;
      Invert_Y          : Boolean;
      Compress_To_DXT   : Boolean;
      DDS_Load_Direct   : Boolean;
      NTSC_Safe_RGB     : Boolean;
      CoCg_Y            : Boolean;
      Texture_Rectangle : Boolean;
   end record;

   type Fake_HDR_Representation is (RGBE, RGBdivA, RGBdivA2);

   type Image_Save_Type is (TGA, BMP, DDS);

   type Cubemap_Part is ('E', 'W', 'U', 'D', 'N', 'S');
   pragma Convention (C, Cubemap_Part);
   for Cubemap_Part'Size use Interfaces.C.char'Size;
   type Cubemap_Layout is array (1 .. 6) of Cubemap_Part;

   SOIL_Error : exception;

   procedure Load_File_To_Texture
     (File_Name : String;
      Texture   : in out GL.Objects.Textures.Texture'Class;
      Channels  : Image_Format  := Auto;
      Flags     : Texture_Flags := (others => False));

   procedure Load_Files_To_Cubemap
     (Pos_X_File, Neg_X_File, Pos_Y_File, Neg_Y_File,
      Pos_Z_File, Neg_Z_File : String;
      Texture  : in out GL.Objects.Textures.Texture'Class;
      Channels : Image_Format  := Auto;
      Flags    : Texture_Flags := (others => False));

   procedure Load_File_To_Cubemap
     (File_Name  : String;
      Texture    : in out GL.Objects.Textures.Texture'Class;
      Face_Order : Cubemap_Layout := "EWUDNS";
      Channels   : Image_Format   := Auto;
      Flags      : Texture_Flags  := (others => False));

   procedure Load_HDR_Texture
     (File_Name      : String;
      Texture        : in out GL.Objects.Textures.Texture'Class;
      Format         : Fake_HDR_Representation;
      Rescale_To_Max : Boolean;
      Flags          : Texture_Flags := (others => False));

   procedure Save_Screenshot (File_Name  : String;
                              Image_Type : Image_Save_Type;
                              X, Y, Width, Height : GL.Types.Int);

private

   for Texture_Flags use record
      Power_Of_Two      at 0 range 0 .. 0;
      Mipmaps           at 0 range 1 .. 1;
      Texture_Repeats   at 0 range 2 .. 2;
      Multiply_Alpha    at 0 range 3 .. 3;
      Invert_Y          at 0 range 4 .. 4;
      Compress_To_DXT   at 0 range 5 .. 5;
      DDS_Load_Direct   at 0 range 6 .. 6;
      NTSC_Safe_RGB     at 0 range 7 .. 7;
      CoCg_Y            at 0 range 8 .. 8;
      Texture_Rectangle at 0 range 9 .. 9;
   end record;
   pragma Warnings (Off);
   for Texture_Flags'Size use Interfaces.C.int'Size;
   pragma Warnings (On);
   pragma Convention (C_Pass_By_Copy, Texture_Flags);

   for Fake_HDR_Representation use (RGBE     => 0,
	                            RGBdivA  => 1,
                                    RGBdivA2 => 2);
   for Fake_HDR_Representation'Size use Interfaces.C.int'Size;

   for Image_Save_Type use (TGA => 0,
                            BMP => 1,
                            DDS => 2);
   for Image_Save_Type'Size use Interfaces.C.int'Size;

   pragma Convention (C, Cubemap_Layout);
   pragma Pack (Cubemap_Layout);

   type Bool is new Boolean;
   for Bool use (False => 0, True => 1);
   for Bool'Size use Interfaces.C.int'Size;

   function Last_Error return String;
   pragma Inline (Last_Error);
end SOIL;
