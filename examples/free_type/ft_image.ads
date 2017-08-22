
with System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

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
   subtype FT_Pos is long;

private
    type FT_Bitmap is record
      Rows         : aliased unsigned;
      Width        : aliased unsigned;
      Pitch        : aliased int;
      Buffer       : access unsigned_char;
      Num_Grays    : aliased unsigned_short;
      Pixel_mode   : aliased unsigned_char;
      Palette_mode : aliased unsigned_char;
      Palette      : System.Address;
    end record;

   pragma Convention (C_Pass_By_Copy, FT_Bitmap);
   type FT_Outline is record
      Num_Contours : aliased short;
      Num_Points   : aliased short;
      Points     : access FT_Vector;
      Tags       : Interfaces.C.Strings.chars_ptr;
      Contours   : access short;
      Flags      : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Outline);

    type FT_Vector is record
      x : aliased FT_Pos;
      y : aliased FT_Pos;
    end record;
   pragma Convention (C_Pass_By_Copy, FT_Vector);
end FT_Image;
