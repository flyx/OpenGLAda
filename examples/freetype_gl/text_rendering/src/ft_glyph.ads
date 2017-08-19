
with System;
with Interfaces.C;

with FT_Types;

package FT_Glyph is

   subtype FT_Pos is Interfaces.C.long;

   function Get_Glyph (Slot_Ptr : FT_Types.FT_Glyph_Slot; aGlyph : in out System.Address)
                          return FT_Types.FT_Error;

end FT_Glyph;
