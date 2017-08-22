
with System.Address_To_Access_Conversions;

with FT_Glyphs.API;
with FT_Interface;

package body FT_Glyphs is
   package Glyph_Slot_Access is new System.Address_To_Access_Conversions (FT_Interface.FT_Glyph_Slot_Record);

   procedure Done_Glyph (Glyph_Ptr : FT_Glyph) is
   begin
      FT_Glyphs.API.FT_Done_Glyph (Glyph_Ptr);
   end Done_Glyph;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Width (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Single is
      use Glyph_Slot_Access;
      Glyph : FT_Interface.FT_Glyph_Slot_Record :=
                To_Pointer (System.Address (Slot_Ptr)).all;
      Width : GL.Types.Single := Glyph.;
   begin
      return Width;
   end Get_Bitmap_Width;

   --  -------------------------------------------------------------------------

   function Get_Glyph (Slot_Ptr : FT_Types.FT_Glyph_Slot;
                       Glyph_Ptr : in out System.Address)
                          return FT_Types.FT_Error is
   begin
      return FT_Glyphs.API.FT_Get_Glyph (Slot_Ptr, Glyph_Ptr);
   end Get_Glyph;

   --  -------------------------------------------------------------------------

   function Glyph_To_Bitmap
       (theGlyph    : System.Address; Render_Mode : FT_Types.FT_Render_Mode;
        Origin      : access FT_Image.FT_Vector;
        Destroy     : FT_Types.FT_Bool) return FT_Types.FT_Error is
   begin
      return FT_Glyphs.API.FT_Glyph_To_Bitmap (theGlyph, Render_Mode,
                                               Origin, Destroy);
   end Glyph_To_Bitmap;

end FT_Glyphs;
