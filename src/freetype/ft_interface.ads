
with System;

with Interfaces.C;
with Interfaces.C.Strings;

with GL.Types;

with FT_Image;
with FT_System;
with FT_Types; Use FT_Types;

package FT_Interface is

   type FT_Face is new System.Address;
   type FT_Face_Record is private;

   type FT_Encoding is (None, Adobe_Custom, Adobe_Expert, Adobe_Standard,
                        Apple_Roman, Big5, GB2312, Johab, Adobe_Latin_1,
                        Old_Latin_2, SJIS, MS_Symbol, Unicode, Wansung);

   procedure Done_Face (Face_Ptr : FT_Face);
   procedure Done_Library (Library_Ptr : FT_Library);
   function Face (Face_Ptr : FT_Face) return FT_Face_Record;
   function Get_Face_Record (Face_Ptr : FT_Face) return FT_Face_Record;
   function Get_Glyph_Slot (Face_Ptr : FT_Face) return FT_Glyph_Slot_Ptr;
   function Init_FreeType (alibrary : in out FT_Library) return FT_Error;
   function Load_Character (Face       : in out FT_Face; Char_Code : FT_ULong;
                            Load_Flags : FT_Types.Load_Flag) return FT_Error;
   function New_Face (Library    : FT_Library; File_Path_Name : String;
                      Face_Index : GL.Types.long; aFace : in out FT_Face)
                      return FT_Error;
   function Render_Glyph (Face_Ptr : FT_Face;
                          Render_Mode : FT_Render_Mode := Render_Mode_Mono)
                          return FT_Error;
   function Set_Pixel_Sizes (Face         : FT_Face; Pixel_Width : GL.Types.UInt;
                             Pixel_Height : GL.Types.UInt) return FT_Error;
private
   type FT_Bitmap_Size is record
      Height : GL.Types.Short;
      Width  : GL.Types.Short;
      Size   : FT_Image.FT_Pos;
      X_Ppem : FT_Image.FT_Pos;
      Y_Ppem : FT_Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Bitmap_Size);

   type FT_BBox is record
      X_Min : FT_Image.FT_Pos;
      Y_Min : FT_Image.FT_Pos;
      X_Max : FT_Image.FT_Pos;
      Y_Max : FT_Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_BBox);

   type FT_Face_Record is record
      Num_Faces               : GL.Types.Long;
      --  Face_Index holds two different values.
      --  Bits 0-15 are the index of the face in the font file (starting with ~0)
      --  and are set to ~0 if there is only one face in the font file.
      Face_Index              : GL.Types.Long;
      Face_Flags              : GL.Types.Long;
      Style_Flags             : GL.Types.Long;
      Num_Glyphs              : GL.Types.Long;
      Family_Name             : access FT_String;
      Style_Name              : access FT_String;
      Num_Fixed_sizes         : GL.Types.Int;
      Available_Sizes         : access FT_Bitmap_Size;
      Num_Charmaps            : GL.Types.Int;
      Character_Map_List      : System.Address;
      C_Generic               : FT_Generic;
      Bbox                    : FT_BBox;
      Units_Per_EM            : GL.Types.UShort;
      Ascender                : GL.Types.Short;
      Descender               : GL.Types.Short;
      Height                  : GL.Types.Short;
      Max_Advance_Width       : GL.Types.Short;
      Underline_Position      : GL.Types.Short;
      Underline_Thickness     : GL.Types.Short;
      Glyph_Slot              : FT_Glyph_Slot_Ptr;
      Size                    : FT_Size_Ptr;
      Character_Map           : FT_Char_Map;
      Driver                  : FT_Driver;
      Memory                  : FT_System.FT_Memory;
      Stream                  : FT_System.FT_Stream;
      Sizes_List              : FT_List_Record;
      Autohint                : FT_Generic;
      Extensions              : System.Address;
      Internal                : FT_Face_Internal;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Face_Record);

   --  FT_Encoding courtesy of OpenGLAda.src.ftgl.ftgl.ads type Charset
   --  (Felix Krause <contact@flyx.org>, 2013)
   for FT_Encoding use (None => 0,
                        MS_Symbol => Character'Pos ('s') * 2**24 +
                          Character'Pos ('y') * 2**16 +
                          Character'Pos ('m') * 2**8  +
                          Character'Pos ('b'),

                        Unicode   => Character'Pos ('u') * 2**24 +
                          Character'Pos ('n') * 2**16 +
                          Character'Pos ('i') * 2**8 +
                          Character'Pos ('c'),

                        SJIS      => Character'Pos ('s') * 2**24 +
                          Character'Pos ('j') * 2**16 +
                          Character'Pos ('i') * 2**8 +
                          Character'Pos ('s'),

                        GB2312    => Character'Pos ('g') * 2**24 +
                          Character'Pos ('b') * 2**16 +
                          Character'Pos (' ') * 2**8 +
                          Character'Pos (' '),

                        Big5      => Character'Pos ('b') * 2**24 +
                          Character'Pos ('i') * 2**16 +
                          Character'Pos ('g') * 2**8 +
                          Character'Pos ('5'),

                        Wansung   => Character'Pos ('w') * 2**24 +
                          Character'Pos ('a') * 2**16 +
                          Character'Pos ('n') * 2**8 +
                          Character'Pos ('s'),

                        Johab     => Character'Pos ('j') * 2**24 +
                          Character'Pos ('o') * 2**16 +
                          Character'Pos ('h') * 2**8 +
                          Character'Pos ('a'),

                        Adobe_Standard => Character'Pos ('A') * 2**24 +
                          Character'Pos ('D') * 2**16 +
                          Character'Pos ('O') * 2**8 +
                          Character'Pos ('B'),

                        Adobe_Expert   => Character'Pos ('A') * 2**24 +
                          Character'Pos ('D') * 2**16 +
                          Character'Pos ('B') * 2**8 +
                          Character'Pos ('E'),

                        Adobe_Custom   => Character'Pos ('A') * 2**24 +
                          Character'Pos ('D') * 2**16 +
                          Character'Pos ('B') * 2**8 +
                          Character'Pos ('C'),

                        Adobe_Latin_1  => Character'Pos ('l') * 2**24 +
                          Character'Pos ('a') * 2**16 +
                          Character'Pos ('t') * 2**8 +
                          Character'Pos ('1'),

                        Old_Latin_2    => Character'Pos ('l') * 2**24 +
                          Character'Pos ('a') * 2**16 +
                          Character'Pos ('t') * 2**8 +
                          Character'Pos ('2'),

                        Apple_Roman    => Character'Pos ('a') * 2**24 +
                          Character'Pos ('r') * 2**16 +
                          Character'Pos ('m') * 2**8 +
                          Character'Pos ('n'));
end FT_Interface;
