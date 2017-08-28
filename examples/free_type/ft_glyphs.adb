
with System.Address_To_Access_Conversions;

with GL.Objects.Textures;

with FT_Glyphs.API;
with FT_Image;
with FT_Interface;

package body FT_Glyphs is
   package Glyph_Bitmap_Access is new System.Address_To_Access_Conversions (FT_Image.FT_Bitmap);
   package Glyph_Slot_Access is new System.Address_To_Access_Conversions (FT_Interface.FT_Glyph_Slot_Record);

   procedure Done_Glyph (Glyph_Ptr : FT_Glyph) is
   begin
      FT_Glyphs.API.FT_Done_Glyph (Glyph_Ptr);
   end Done_Glyph;

   --  -------------------------------------------------------------------------

   function Get_Bitmap (Slot_Ptr : FT_Types.FT_Glyph_Slot)
                              return FT_Image.FT_Bitmap is
      use Glyph_Slot_Access;
      Glyph : FT_Interface.FT_Glyph_Slot_Record :=
                To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Bitmap;
   end Get_Bitmap;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Image (Slot_Ptr : FT_Types.FT_Glyph_Slot)
                              return GL.Objects.Textures.Image_Source is
      use Glyph_Slot_Access;
      Glyph : FT_Interface.FT_Glyph_Slot_Record :=
                To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return FT_Image.Get_Buffer (Glyph.Bitmap);
   end Get_Bitmap_Image;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Left (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Int is
      use Glyph_Slot_Access;
      Glyph : FT_Interface.FT_Glyph_Slot_Record :=
                To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return GL.Types.Int (Glyph.Bitmap_Left);
   end Get_Bitmap_Left;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Width (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Single is
      use Glyph_Slot_Access;
      Glyph : FT_Interface.FT_Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
      Bitmap : FT_Image.FT_Bitmap := Glyph.Bitmap;
   begin
      return GL.Types.Single (FT_Image.Get_Width (Bitmap));
   end Get_Bitmap_Width;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Height (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Single is
      use Glyph_Slot_Access;
      Glyph : FT_Interface.FT_Glyph_Slot_Record :=
                To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return GL.Types.Single (Glyph.Metrics.height);
   end Get_Bitmap_Height;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Rows (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Int is
      use Glyph_Slot_Access;
      Glyph : FT_Interface.FT_Glyph_Slot_Record :=
                To_Pointer (System.Address (Slot_Ptr)).all;
      Bitmap : FT_Image.FT_Bitmap := Glyph.Bitmap;
   begin
      return GL.Types.Int (FT_Image.Get_Rows (Bitmap));
   end Get_Bitmap_Rows;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Top (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Int is
      use Glyph_Slot_Access;
      Glyph : FT_Interface.FT_Glyph_Slot_Record :=
                To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return GL.Types.Int (Glyph.Bitmap_Top);
   end Get_Bitmap_Top;

   --  -------------------------------------------------------------------------

   function Get_Glyph (Slot_Ptr : FT_Types.FT_Glyph_Slot;
                       Glyph_Ptr : in out System.Address)
                          return FT_Types.FT_Error is
   begin
      return FT_Glyphs.API.FT_Get_Glyph (Slot_Ptr, Glyph_Ptr);
   end Get_Glyph;

   --  -------------------------------------------------------------------------

   function Get_Glyph_Advance (Slot_Ptr : FT_Types.FT_Glyph_Slot) return FT_Image.FT_Vector is
      use Glyph_Slot_Access;
      Glyph : FT_Interface.FT_Glyph_Slot_Record :=
                To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Advance;
   end Get_Glyph_Advance;

   --  -------------------------------------------------------------------------

   function Get_Glyph_Format (Slot_Ptr : FT_Types.FT_Glyph_Slot) return FT_Image.FT_Glyph_Format is
      use Glyph_Slot_Access;
      Glyph : FT_Interface.FT_Glyph_Slot_Record :=
                To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Format;
   end Get_Glyph_Format;

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
