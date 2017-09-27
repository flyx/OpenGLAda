
with System;

with FT.Errors;
with FT.Glyphs;
with FT.Image;

package FT.API.Glyphs is
   pragma Preelaborate;

   procedure FT_Done_Glyph (Glyph : FT.Glyphs.Glyph_Ptr);
   pragma Import (C, FT_Done_Glyph, "FT_Done_Glyph");

   --  tell the compiler that we are aware that Bool is 8-bit and will need to
   --  be a char on the C side.
   pragma Warnings (Off, "8-bit Ada Boolean");
   function FT_Glyph_To_Bitmap
     (theGlyph   : System.Address; Mode : FT.API.Render_Mode;
      Origin      : access FT.Image.FT_Vector;
      Destroy     : Bool) return Errors.Error_Code;
   pragma Import (C, FT_Glyph_To_Bitmap, "FT_Glyph_To_Bitmap");
   pragma Warnings (On, "8-bit Ada Boolean");

   function FT_Get_Glyph (Slot_Ptr : FT.API.Glyph_Slot_Ptr;
                          aGlyph   : in out System.Address)
                          return Errors.Error_Code;
   pragma Import (C, FT_Get_Glyph, "FT_Get_Glyph");

   function FT_Render_Glyph (Slot : Glyph_Slot_Ptr; Mode : Render_Mode)
                             return Errors.Error_Code;
   pragma Import (C, FT_Render_Glyph, "FT_Render_Glyph");

end FT.API.Glyphs;
