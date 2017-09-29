--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

with System;

with GL.Objects.Textures;
with GL.Types;

with FT;
with FT.API; use FT.API;
with FT.Image;

private with Interfaces.C.Strings;

package FT.Faces is
   pragma Preelaborate;

   type Character_Record is private;
   type Character_Data_Vector is array (Natural range <>) of Character_Record;

   type Face_Record is private;
   type List_Record is private;

   type Generic_Record is private;
   type Size_Metrics_Record is private;
   type Size_Ptr is private;
   type Size_Record is private;

   type FT_Encoding is (None, Adobe_Custom, Adobe_Expert, Adobe_Standard,
                        Apple_Roman, Big5, GB2312, Johab, Adobe_Latin_1,
                        Old_Latin_2, SJIS, MS_Symbol, Unicode, Wansung);

   type Load_Flag is (Load_Default, Load_No_Scale, Load_No_Hinting, Load_Render,
                      Load_No_Bitmap, Load_Vertical_Layout, Load_Force_Autohint,
                      Load_Crop_Bitmap, Load_Pedantic, Load_Advance_Only,
                      Load_Ignore_Global_Advance_Width, Load_No_Recourse,
                      Load_Ignore_Transform, Load_Monochrome, Load_Linear_Design,
                      Load_SBits_Only, Load_No_Autohint, Load_Load_Colour,
                      Load_Compute_Metrics, Load_Bitmap_Metrics_Only);


   function Advance_X (Data : Character_Record) return GL.Types.Int;
   function Bitmap_Height (aFace : Face_Ptr) return GL.Types.Int;
   function Bitmap_Width (aFace : Face_Ptr) return GL.Types.Int;
   function Character_Texture (Data : Character_Record)
                               return GL.Objects.Textures.Texture;
   procedure Done_Face (aFace : Face_Ptr);
   function Face (aFace : Face_Ptr) return Face_Record;
   function Face_Height (aFace : Face_Ptr) return GL.Types.Int;
   function Face_Width (aFace : Face_Ptr) return GL.Types.Int;
   function Glyph_Slot (aFace : FT.API.Face_Ptr) return Glyph_Slot_Ptr;
   procedure Kerning (aFace       : Face_Ptr; Left_Glyph : GL.Types.UInt;
                     Right_Glyph : GL.Types.UInt; Kern_Mode : GL.Types.UInt;
                     aKerning    : access FT.Image.FT_Vector);
   function Left (Data : Character_Record) return GL.Types.Int;
   procedure Load_Character (aFace : Face_Ptr; Char_Code : GL.Types.Long;
                            Flags : Load_Flag);
   procedure New_Face (Library    : Library_Ptr; File_Path_Name : String;
                      Face_Index : GL.Types.long; aFace : in out FT.API.Face_Ptr);
   function Character_Data_To_String (Char : Character; Data : Character_Record)
                                      return String;
   function Rows (Data : Character_Record) return GL.Types.Int;
   procedure Set_Char_Data (Char_Data : in out Character_Record;
                            Width     : GL.Types.Int; Height : GL.Types.Int;
                            Left      : GL.Types.Int; Top    : GL.Types.Int;
                            Advance_X : GL.Types.Int);
   procedure Set_Pixel_Sizes (aFace        : Face_Ptr; Pixel_Width : GL.Types.UInt;
                             Pixel_Height : GL.Types.UInt);
   procedure Set_Texture (Char_Data : in out Character_Record;
                          Texture   : GL.Objects.Textures.Texture);
   function Size_Metrics (aFace : Face_Ptr) return Size_Metrics_Record;
   function Top (Data : Character_Record) return GL.Types.Int;
   function Width (Data : Character_Record) return GL.Types.Int;

   Image_Error : exception;

private
   type Char_Map_Ptr is new System.Address;
   type Driver_Ptr is new System.Address;
   type Face_Internal_Ptr is new System.Address;
   type Memory_Ptr is new System.Address;
   type Size_Ptr is new System.Address;
   type Size_Internal_Ptr is new System.Address;
   type Stream_Ptr is new System.Address;

   type Character_Record is record
      Texture   : GL.Objects.Textures.Texture;
      Width     : GL.Types.Int := 0;
      Rows      : GL.Types.Int := 0;
      Left      : GL.Types.Int := 0;
      Top       : GL.Types.Int := 0;
      Advance_X : GL.Types.Int := 0;
   end record;

   type FT_Bitmap_Size is record
      Height : GL.Types.Short;
      Width  : GL.Types.Short;
      Size   : FT.Image.FT_Pos;
      X_Ppem : FT.Image.FT_Pos;
      Y_Ppem : FT.Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Bitmap_Size);

   type FT_BBox is record
      X_Min : FT.Image.FT_Pos;
      Y_Min : FT.Image.FT_Pos;
      X_Max : FT.Image.FT_Pos;
      Y_Max : FT.Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_BBox);

   type Generic_Finalizer is access procedure (theFinalizer : System.Address);
   pragma Convention (C, Generic_Finalizer);

   type Generic_Record is record
      Data      : System.Address;
      Finalizer : Generic_Finalizer;
   end record;
   pragma Convention (C_Pass_By_Copy, Generic_Record);

   type List_Node is new System.Address;
   type List_Record is record
      head : List_Node;
      tail : List_Node;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Record);

   type Face_Record is record
      Num_Faces               : GL.Types.Long;
      --  Face_Index holds two different values.
      --  Bits 0-15 are the index of the face in the font file (starting with ~0)
      --  and are set to ~0 if there is only one face in the font file.
      Face_Index              : GL.Types.Long;
      Face_Flags              : GL.Types.Long;
      Style_Flags             : GL.Types.Long;
      Num_Glyphs              : GL.Types.Long;
      Family_Name             : Interfaces.C.Strings.chars_ptr;
      Style_Name              : Interfaces.C.Strings.chars_ptr;
      --  Num_Fixed_Sizes is the number of bitmap strikes in the face.
      --  Even if the face is scalable, there might still be bitmap strikes,
      --  which are called `sbits' in that case.

      Num_Fixed_sizes         : GL.Types.Int;
      --  Available_Sizes is an array of FT_Bitmap_Size records for all bitmap
      --  strikes in the face.  It is NULL if there is no bitmap strike.
      Available_Sizes         : access FT_Bitmap_Size;
      Num_Charmaps            : GL.Types.Int;
      Character_Map_List      : System.Address;
      C_Generic               : Generic_Record;
      --  The following member variables (down to `underline_thickness')
      --  are only relevant to scalable outlines.

      --  Bbox is the font bounding box.  Coordinates are expressed in font units
      --  The box is large enough to contain any glyph from the font.
      --  Thus, bbox.yMax can be seen as the maximum  ascender' and
      --  bbox.yMin as the `minimum descender.
      --   Bbox is only relevant for scalable   formats.

      Bbox                    : FT_BBox;
      --  Units_per_EM is the number of font units per EM square for  this face.
      --  This is typically 2048 for TrueType fonts and 1000 for Type~1 fonts.
      --  Units_per_EM is only relevant for scalable formats.

      Units_Per_EM            : GL.Types.UShort;
      --  Ascender and descender are the typographic ascender  and descender of
      --  the face expressed in font units.
      --  For font formats not having this information, they are set to
      --  bbox.yMax and bbox.yMin.
      --  Ascender is only relevant for scalable formats.

      Ascender                : GL.Types.Short;
      Descender               : GL.Types.Short;
      --  Height is the vertical distance   between two consecutive baselines,
      --  expressed in font units and is always positive.
      --  Height is only relevant for scalable formats.
      --  For the global glyph height use  ascender - descender.

      Height                  : GL.Types.Short;
      --  Max_Advance_Width and Max_Advance_Height are the maximum and advance
      --  width in font units for all glyphs in this face.
      --  They are only relevant for scalable formats.
      --  They can be used to make word wrapping computations faster.

      Max_Advance_Width       : GL.Types.Short;
      Max_Advance_Height      : GL.Types.Short;
      Underline_Position      : GL.Types.Short;
      Underline_Thickness     : GL.Types.Short;
      Glyph_Slot              : FT.API.Glyph_Slot_Ptr;
      --  Size is the current active size for this face.
      Size                    : Size_Ptr;             -- Ptr to a FT_SizeRec
      Character_Map           : Char_Map_Ptr;
      Driver                  : Driver_Ptr;
      Memory                  : Memory_Ptr;
      Stream                  : Stream_Ptr;
      Sizes_List              : List_Record;
      Autohint                : Generic_Record;
      Extensions              : System.Address := System.Null_Address;
      Internal                : Face_Internal_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Face_Record);

   type Size_Metrics_Record is record
      X_Ppem      : GL.Types.UShort;
      Y_Ppem      : GL.Types.Int;
      Y_Scale     : GL.Types.Int;
      Ascender    : FT.Image.FT_Pos;
      Descender   : FT.Image.FT_Pos;
      Height      : FT.Image.FT_Pos;
      Max_Advance : FT.Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, Size_Metrics_Record);

   type Size_Record is record
      Face       : Face_Record;
      C_Generic  : Generic_Record;
      Metrics    : Size_Metrics_Record;
      Internal   : Size_Internal_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Size_Record);

   --  FT_Encoding courtesy of OpenGLAda.src.ftgl.ftgl.ads type Charset
   --  (Felix Krause <contact@flyx.org>, 2013)
   for FT_Encoding use (None      => 0,
                        MS_Symbol => Character'Pos ('s') * 2 ** 24 +
                            Character'Pos ('y') * 2 ** 16 +
                            Character'Pos ('m') * 2 ** 8  +
                            Character'Pos ('b'),

                        Unicode   => Character'Pos ('u') * 2 ** 24 +
                            Character'Pos ('n') * 2 ** 16 +
                            Character'Pos ('i') * 2 ** 8 +
                            Character'Pos ('c'),

                        SJIS      => Character'Pos ('s') * 2 ** 24 +
                            Character'Pos ('j') * 2 ** 16 +
                            Character'Pos ('i') * 2 ** 8 +
                            Character'Pos ('s'),

                        GB2312    => Character'Pos ('g') * 2 ** 24 +
                            Character'Pos ('b') * 2 ** 16 +
                            Character'Pos (' ') * 2 ** 8 +
                            Character'Pos (' '),

                        Big5      => Character'Pos ('b') * 2 ** 24 +
                            Character'Pos ('i') * 2 ** 16 +
                            Character'Pos ('g') * 2 ** 8 +
                            Character'Pos ('5'),

                        Wansung   => Character'Pos ('w') * 2 ** 24 +
                            Character'Pos ('a') * 2 ** 16 +
                            Character'Pos ('n') * 2 ** 8 +
                            Character'Pos ('s'),

                        Johab     => Character'Pos ('j') * 2 ** 24 +
                            Character'Pos ('o') * 2 ** 16 +
                            Character'Pos ('h') * 2 ** 8 +
                            Character'Pos ('a'),

                        Adobe_Standard => Character'Pos ('A') * 2 ** 24 +
                            Character'Pos ('D') * 2 ** 16 +
                            Character'Pos ('O') * 2 ** 8 +
                            Character'Pos ('B'),

                        Adobe_Expert   => Character'Pos ('A') * 2 ** 24 +
                            Character'Pos ('D') * 2 ** 16 +
                            Character'Pos ('B') * 2 ** 8 +
                            Character'Pos ('E'),

                        Adobe_Custom   => Character'Pos ('A') * 2 ** 24 +
                            Character'Pos ('D') * 2 ** 16 +
                            Character'Pos ('B') * 2 ** 8 +
                            Character'Pos ('C'),

                        Adobe_Latin_1  => Character'Pos ('l') * 2 ** 24 +
                            Character'Pos ('a') * 2 ** 16 +
                            Character'Pos ('t') * 2 ** 8 +
                            Character'Pos ('1'),

                        Old_Latin_2    => Character'Pos ('l') * 2 ** 24 +
                            Character'Pos ('a') * 2 ** 16 +
                            Character'Pos ('t') * 2 ** 8 +
                            Character'Pos ('2'),

                        Apple_Roman    => Character'Pos ('a') * 2 ** 24 +
                            Character'Pos ('r') * 2 ** 16 +
                            Character'Pos ('m') * 2 ** 8 +
                            Character'Pos ('n'));
   for Load_Flag use
       (Load_Default                     => 16#000000#,
        Load_No_Scale                    => 16#000001#,
        Load_No_Hinting                  => 16#000002#,
        Load_Render                      => 16#000004#,
        Load_No_Bitmap                   => 16#000008#,
        Load_Vertical_Layout             => 16#000010#,
        Load_Force_Autohint              => 16#000020#,
        Load_Crop_Bitmap                 => 16#000040#,
        Load_Pedantic                    => 16#000080#,
        Load_Advance_Only                => 16#000100#,
        Load_Ignore_Global_Advance_Width => 16#000200#,
        Load_No_Recourse                 => 16#000400#,
        Load_Ignore_Transform            => 16#000800#,
        Load_Monochrome                  => 16#001000#,
        Load_Linear_Design               => 16#002000#,
        Load_SBits_Only                  => 16#0004000#,
        Load_No_Autohint                 => 16#008000#,
        Load_Load_Colour                 => 16#100000#,
        Load_Compute_Metrics             => 16#200000#,
        Load_Bitmap_Metrics_Only         => 16#400000#);

end FT.Faces;