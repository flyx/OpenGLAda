
with System;

with Interfaces.C.Strings;

with GL.Objects.Textures;
with GL.Types;

package FT.Image is
   pragma Preelaborate;

   type Bitmap_Record is private;
   type Outline_Record is private;
   type FT_Vector is private;
   type Glyph_Format is (Format_None, Bitmap_Format, Composite_Format,
                         Outline_Format, Plotter_Format);
   subtype FT_Pos is GL.Types.Long;

   function Buffer (Bitmap : Bitmap_Record) return GL.Objects.Textures.Image_Source;
   function Num_Grays (Bitmap : Bitmap_Record) return GL.Types.Short;
   function Rows (Bitmap : Bitmap_Record) return GL.Types.Int;
   function Palette_Mode (Bitmap : Bitmap_Record) return unsigned_char;
   function Pitch (Bitmap : Bitmap_Record) return GL.Types.Int;
   function Pixel_Mode (Bitmap : Bitmap_Record) return unsigned_char;
   function Width (Bitmap : Bitmap_Record) return GL.Types.UInt;
   function Vector_X (theVector : FT_Vector) return GL.Types.Int;
   function Vector_Y (theVector : FT_Vector) return GL.Types.Int;

private
   type Bitmap_Record is record
      Rows         : GL.Types.UInt;
      Width        : GL.Types.UInt;
      Pitch        : GL.Types.Int;
      Buffer       : access unsigned_char;
      Num_Grays    : GL.Types.Short;
      Pixel_Mode   : unsigned_char;
      Palette_Mode : unsigned_char;
      Palette      : System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, Bitmap_Record);

   type Outline_Record is record
      Num_Contours : GL.Types.short;
      Num_Points   : GL.Types.short;
      Points       : access FT_Vector;
      Tags         : Interfaces.C.Strings.chars_ptr;
      Contours     : access GL.Types.short;
      Flags        : GL.Types.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Outline_Record);

   type FT_Vector is record
      x : FT_Pos;
      y : FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Vector);

   for Glyph_Format use (Format_None => unsigned (0000000000),
                            Bitmap_Format      => unsigned (1651078259),
                            Composite_Format   => unsigned (1668246896),
                            Outline_Format     => unsigned (1869968492),
                            Plotter_Format     => unsigned (1886154612));
end FT.Image;
