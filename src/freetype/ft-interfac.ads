
with System;

with GL.Types;

with FT;
with FT.Image;

package FT.Interfac is

   type FT_Face_Record is private;
   type List_Record is private;

   type Generic_Record is private;
   type Size_Ptr is private;

   type FT_Encoding is (None, Adobe_Custom, Adobe_Expert, Adobe_Standard,
                        Apple_Roman, Big5, GB2312, Johab, Adobe_Latin_1,
                        Old_Latin_2, SJIS, MS_Symbol, Unicode, Wansung);

   type Load_Flag is (Load_Default, Load_No_Scale, Load_No_Hinting, Load_Render,
                      Load_No_Bitmap, Load_Vertical_Layout, Load_Force_Autohint,
                      Load_Crop_Bitmap, Load_Pedantic, Load_Advance_Only,
                      Load_Ignore_Gloabl_Advance_Width, Load_No_Recourse,
                      Load_Ignore_Transform, Load_Monochrome, Load_Linear_Design,
                      Load_SBits_Only, Load_No_Autohint, Load_Load_Colour,
                      Load_Compute_Metrics, Load_Bitmap_Metrics_Only);

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
                            Flags : Load_Flag) return FT_Error;
   function New_Face (Library    : Library_Ptr; File_Path_Name : String;
                      Face_Index : GL.Types.long; aFace : in out Face_Ptr)
                      return FT_Error;
   function Render_Glyph (aFace : Face_Ptr;
                          Mode  : Render_Mode)
                          return FT_Error;
   function Set_Pixel_Sizes (aFace : Face_Ptr; Pixel_Width : GL.Types.UInt;
                             Pixel_Height : GL.Types.UInt) return FT_Error;
private

   type Char_Map_Ptr is new System.Address;
   type Driver_Ptr is new System.Address;
   type Face_Internal_Ptr is new System.Address;
   type Size_Ptr is new System.Address;

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
   for Load_Flag use
       (Load_Default => 16#000000#,
        Load_No_Scale => 16#000001#,
        Load_No_Hinting => 16#000002#,
        Load_Render => 16#000004#,
        Load_No_Bitmap => 16#000008#,
        Load_Vertical_Layout => 16#000010#,
        Load_Force_Autohint => 16#000020#,
        Load_Crop_Bitmap => 16#000040#,
        Load_Pedantic => 16#000080#,
        Load_Advance_Only => 16#000100#,
        Load_Ignore_Gloabl_Advance_Width => 16#000200#,
        Load_No_Recourse => 16#000400#,
        Load_Ignore_Transform => 16#000800#,
        Load_Monochrome => 16#001000#,
        Load_Linear_Design => 16#002000#,
        Load_SBits_Only => 16#0004000#,
        Load_No_Autohint => 16#008000#,
        Load_Load_Colour => 16#100000#,
        Load_Compute_Metrics => 16#200000#,
        Load_Bitmap_Metrics_Only => 16#400000#);

end FT.Interfac;
