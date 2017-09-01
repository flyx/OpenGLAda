
with System;

with FT.Glyphs;
with FT.Image;
with FT.Types;

package FT.Glyphs.API is

   procedure FT_Done_Glyph (Glyph : FT.Glyphs.FT_Glyph);
   pragma Import (C, FT_Done_Glyph, "FT_Done_Glyph");

  function FT_Glyph_To_Bitmap
     (theGlyph   : System.Address; Mode : FT.Types.Render_Mode;
      Origin      : access FT.Image.FT_Vector;
      Destroy     : FT.Types.FT_Bool) return FT.Types.FT_Error;
   pragma Import (C, FT_Glyph_To_Bitmap, "FT_Glyph_To_Bitmap");

   function FT_Get_Glyph (Slot_Ptr : FT.Interfac.Glyph_Slot_Ptr;
                          aGlyph   : in out System.Address)
                          return FT.Types.FT_Error;
   pragma Import (C, FT_Get_Glyph, "FT_Get_Glyph");

end FT.Glyphs.API;
