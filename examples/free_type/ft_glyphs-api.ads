
with System;

with FT_Glyph;
with FT_Image;
with FT_Types;

package FT_Glyph.API is

   procedure FT_Done_Glyph (Glyph : FT_Glyph.);
   pragma Import (C, FT_Done_Glyph, "FT_Done_Glyph");

  function FT_Glyph_To_Bitmap
     (the_glyph   : System.Address;
      render_mode : FT_Types.FT_Render_Mode;
      origin      : access FT_Image.FT_Vector;
      destroy     : FT_Types.FT_Bool) return FT_Types.FT_Error;
   pragma Import (C, FT_Glyph_To_Bitmap, "FT_Glyph_To_Bitmap");

   function FT_Get_Glyph (Slot_Ptr : FT_Types.FT_Glyph_Slot;
                          aGlyph   : in out System.Address)
                          return FT_Types.FT_Error;
   pragma Import (C, FT_Get_Glyph, "FT_Get_Glyph");

end FT_Glyph.API;
