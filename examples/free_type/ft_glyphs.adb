
with FT_Glyph.API;

package body FT_Glyph is

   function Get_Glyph (Slot_Ptr : FT_Types.FT_Glyph_Slot;
                       Glyph_Ptr : in out System.Address)
                          return FT_Types.FT_Error is
   begin
      return FT_Glyph.API.FT_Get_Glyph (Slot_Ptr, Glyph_Ptr);
   end Get_Glyph;

end FT_Glyph;
