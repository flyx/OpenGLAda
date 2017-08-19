
with System;

with FT_Types;

package FT_Glyph.API is

   function FT_Get_Glyph (Slot_Ptr : FT_Types.FT_Glyph_Slot;
                          aGlyph   : in out System.Address)
                          return FT_Types.FT_Error;
   pragma Import (C, FT_Get_Glyph, "FT_Get_Glyph");

end FT_Glyph.API;
