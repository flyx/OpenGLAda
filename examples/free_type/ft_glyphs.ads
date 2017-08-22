
with System;
with Interfaces.C;

with FT_Image;
with FT_Types;

package FT_Glyph is

   type FT_Glyph_Record is private;
   type FT_Glyph is new System.Address;
   subtype FT_Pos is Interfaces.C.long;

   procedure FT_Done_Glyph (Glyph : FT_Glyph);

   function FT_Glyph_To_Bitmap
       (theGlyph    : System.Address; Render_Mode : FT_Types.FT_Render_Mode;
        Origin      : access FT_Image.FT_Vector;
        Destroy     : FT_Types.FT_Bool) return FT_Types.FT_Error;
   function Get_Glyph (Slot_Ptr  : FT_Types.FT_Glyph_Slot;
                       Glyph_Ptr : in out System.Address) return FT_Types.FT_Error;
private
   type FT_Glyph_Record is record
      Library : FT_Types.FT_Library;
      Clazz   : System.Address;
      Format  : aliased FT_Image.FT_Glyph_Format;
      Advance : aliased FT_Image.FT_Vector;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Glyph_Record);

end FT_Glyph;
