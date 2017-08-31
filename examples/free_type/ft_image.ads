
with System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

with GL.Objects.Textures;
with GL.Types;

package FT_Image is

   type FT_Bitmap is private;
   type FT_Outline is private;
   type FT_Vector is private;
   type FT_Glyph_Format is (Format_None, Bitmap, Composite, Outline, Plotter);
   for FT_Glyph_Format use (Format_None => unsigned (0000000000),
                            Bitmap      => unsigned (1651078259),
                            Composite   => unsigned (1668246896),
                            Outline     => unsigned (1869968492),
                            Plotter     => unsigned (1886154612));
   subtype FT_Pos is GL.Types.Long;

   function Get_Buffer (Bitmap : FT_Bitmap) return GL.Objects.Textures.Image_Source;
   function Get_Num_Grays (Bitmap : FT_Bitmap) return GL.Types.Short;
   function Get_Rows (Bitmap : FT_Bitmap) return GL.Types.Int;
   function Get_Palette_Mode (Bitmap : FT_Bitmap) return unsigned_char;
   function Get_Pitch (Bitmap : FT_Bitmap) return GL.Types.Int;
   function Get_Pixel_Mode (Bitmap : FT_Bitmap) return unsigned_char;
   function Get_Width (Bitmap : FT_Bitmap) return GL.Types.UInt;
   function Vector_X (theVector : FT_Vector) return GL.Types.Int;
   function Vector_Y (theVector : FT_Vector) return GL.Types.Int;

private
   type FT_Bitmap is record
      Rows         : GL.Types.UInt;
      Width        : GL.Types.UInt;
      Pitch        : GL.Types.Int;
      Buffer       : access unsigned_char;
      Num_Grays    : GL.Types.Short;
      Pixel_Mode   : unsigned_char;
      Palette_Mode : unsigned_char;
      Palette      : System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Bitmap);

   type FT_Outline is record
      Num_Contours : short;
      Num_Points   : short;
      Points       : access FT_Vector;
      Tags         : Interfaces.C.Strings.chars_ptr;
      Contours     : access short;
      Flags        : int;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Outline);

   type FT_Vector is record
      x : FT_Pos;
      y : FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Vector);
end FT_Image;
