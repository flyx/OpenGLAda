
with System;

with GL.Types;

with FT;
with FT.Image;
with FT.Types; Use FT.Types;

package FT.Interfac is

   type Face_Ptr is new System.Address;
   type FT_Face_Record is private;

   type Library_Ptr is new System.Address;

   type Glyph_Slot_Ptr is new System.Address;

   type FT_Encoding is (None, Adobe_Custom, Adobe_Expert, Adobe_Standard,
                        Apple_Roman, Big5, GB2312, Johab, Adobe_Latin_1,
                        Old_Latin_2, SJIS, MS_Symbol, Unicode, Wansung);

   procedure Done_Face (aFace : Face_Ptr);
   procedure Done_Library (Library : Library_Ptr);
   function Face (aFace : Face_Ptr) return FT_Face_Record;
   function Face_Record (aFace : Face_Ptr) return FT_Face_Record;
   function Glyph_Slot (aFace : Face_Ptr) return Glyph_Slot_Ptr;
   function Init_FreeType (alibrary : in out Library_Ptr) return FT_Error;
   function Kerning (aFace : Face_Ptr; Left_Glyph : GL.Types.UInt;
                         Right_Glyph : GL.Types.UInt; Kern_Mode : GL.Types.UInt;
                         aKerning : access FT.Image.FT_Vector) return FT_Error;
   function Load_Character (aFace : Face_Ptr; Char_Code : FT_ULong;
                            Load_Flags : FT.Types.Load_Flag) return FT_Error;
   function New_Face (Library    : Library_Ptr; File_Path_Name : String;
                      Face_Index : GL.Types.long; aFace : in out Face_Ptr)
                      return FT_Error;
   function Render_Glyph (aFace : Face_Ptr;
                          Mode : Render_Mode := Render_Mode_Mono)
                          return FT_Error;
   function Set_Pixel_Sizes (aFace : Face_Ptr; Pixel_Width : GL.Types.UInt;
                             Pixel_Height : GL.Types.UInt) return FT_Error;
private
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
      C_Generic               : Generic_Record;
      Bbox                    : FT_BBox;
      Units_Per_EM            : GL.Types.UShort;
      Ascender                : GL.Types.Short;
      Descender               : GL.Types.Short;
      Height                  : GL.Types.Short;
      Max_Advance_Width       : GL.Types.Short;
      Underline_Position      : GL.Types.Short;
      Underline_Thickness     : GL.Types.Short;
      Glyph_Slot              : Glyph_Slot_Ptr;
      Size                    : Size_Ptr;
      Character_Map           : Char_Map_Ptr;
      Driver                  : Driver_Ptr;
      Memory                  : FT.Memory;
      Stream                  : FT.Stream;
      Sizes_List              : List_Record;
      Autohint                : Generic_Record;
      Extensions              : System.Address;
      Internal                : Face_Internal_Ptr;
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
end FT.Interfac;
