
with Interfaces.C; use Interfaces.C;

package FT_Image is

   type FT_Vector is private;
   type FT_Glyph_Format is (Format_None, Bitmap, Composite, Outline, Plotter);
   for FT_Glyph_Format use (Format_None => unsigned (0000000000),
                            Bitmap      => unsigned (1651078259),
                            Composite   => unsigned (1668246896),
                            Outline     => unsigned (1869968492),
                            Plotter     => unsigned (1886154612));
   subtype FT_Pos is long;

private
    type FT_Vector is record
      x : aliased FT_Pos;
      y : aliased FT_Pos;
    end record;
   pragma Convention (C_Pass_By_Copy, FT_Vector);
end FT_Image;
