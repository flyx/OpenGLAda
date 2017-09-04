
with System;

with FT.Glyphs;
with FT.Image;

package FT.API.Glyphs is

   procedure FT_Done_Glyph (Glyph : FT.Glyphs.Glyph_Ptr);
   pragma Import (C, FT_Done_Glyph, "FT_Done_Glyph");

  function FT_Glyph_To_Bitmap
     (theGlyph   : System.Address; Mode : FT.API.Render_Mode;
      Origin      : access FT.Image.FT_Vector;
      Destroy     : FT.FT_Bool) return FT.FT_Error;
   pragma Import (C, FT_Glyph_To_Bitmap, "FT_Glyph_To_Bitmap");

   function FT_Get_Glyph (Slot_Ptr : FT.API.Glyph_Slot_Ptr;
                          aGlyph   : in out System.Address)
                          return FT.FT_Error;
   pragma Import (C, FT_Get_Glyph, "FT_Get_Glyph");

   function FT_Render_Glyph (Slot : Glyph_Slot_Ptr; Mode : Render_Mode)
                             return FT_Error;
   pragma Import (C, FT_Render_Glyph, "FT_Render_Glyph");

end FT.API.Glyphs;
