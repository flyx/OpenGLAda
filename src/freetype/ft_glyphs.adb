
with System.Address_To_Access_Conversions;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures;

with FT_Glyphs.API;
with FT_Image;
with FT_Interface;

package body FT_Glyphs is
   package Glyph_Access is new System.Address_To_Access_Conversions (FT_Glyph_Record);
   package Glyph_Slot_Access is new System.Address_To_Access_Conversions (FT_Glyph_Slot_Record);

   procedure Done_Glyph (Glyph_Ptr : FT_Glyph) is
   begin
      FT_Glyphs.API.FT_Done_Glyph (Glyph_Ptr);
   end Done_Glyph;

   --  -------------------------------------------------------------------------
   --  FT_Glyph_Slot (SA) => FT_Glyph_Slot_Record => FT_Bitmap (record)
   --  FT_Bitmap => Buffer (access unsigned_char)

   function Get_Bitmap (Glyph_Slot : FT.Types.FT_Glyph_Slot_Ptr) return FT_Image.FT_Bitmap is
      use GL.Types;
      use Glyph_Slot_Access;
      aGlyph_Ptr    : System.Address;
      Glyph_Pointer : Object_Pointer := To_Pointer (System.Address (Glyph_Slot));
      theGlyph      : FT_Glyph_Slot_Record := Glyph_Pointer.all;
   begin
      --  Get_Glyph calls the FT_Get_Glyph C function.
      if FT_Glyphs.Get_Glyph (Glyph_Slot, aGlyph_Ptr) /= 0 then
         Put_Line ("FT_Interface.Bitmap raised an Exception");
         raise FT.Types.FT_Exception;
      end if;
      return theGlyph.Bitmap;
   end Get_Bitmap;

   --  -------------------------------------------------------------------------
   function Get_Bitmap_Image (Slot_Ptr : FT.Types.FT_Glyph_Slot_Ptr)
                              return GL.Objects.Textures.Image_Source is
      use Glyph_Slot_Access;
      Glyph : FT_Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return FT_Image.Get_Buffer (Get_Bitmap (Slot_Ptr));
   end Get_Bitmap_Image;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Left (Slot_Ptr : FT.Types.FT_Glyph_Slot_Ptr) return GL.Types.Int is
      use Glyph_Slot_Access;
      Glyph : FT_Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return GL.Types.Int (Glyph.Bitmap_Left);
   end Get_Bitmap_Left;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Width (Slot_Ptr : FT.Types.FT_Glyph_Slot_Ptr) return GL.Types.Single is
      Bitmap : FT_Image.FT_Bitmap := Get_Bitmap (Slot_Ptr);
   begin
      return GL.Types.Single (FT_Image.Get_Width (Bitmap));
   end Get_Bitmap_Width;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Height (Slot_Ptr : FT.Types.FT_Glyph_Slot_Ptr) return GL.Types.Single is
      Bitmap : FT_Image.FT_Bitmap := Get_Bitmap (Slot_Ptr);
   begin
      return GL.Types.Single (FT_Image.Get_Rows (Bitmap));
   end Get_Bitmap_Height;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Rows (Slot_Ptr : FT.Types.FT_Glyph_Slot_Ptr) return GL.Types.Int is
      Bitmap : FT_Image.FT_Bitmap := Get_Bitmap (Slot_Ptr);
   begin
      return GL.Types.Int (FT_Image.Get_Rows (Bitmap));
   end Get_Bitmap_Rows;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Top (Slot_Ptr : FT.Types.FT_Glyph_Slot_Ptr) return GL.Types.Int is
      use Glyph_Slot_Access;
      Glyph : FT_Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return GL.Types.Int (Glyph.Bitmap_Top);
   end Get_Bitmap_Top;

   --  -------------------------------------------------------------------------

   function Get_Glyph (Slot_Ptr : FT.Types.FT_Glyph_Slot_Ptr;
                       Glyph_Ptr : in out System.Address)
                       return FT.Types.FT_Error is
   begin
      return FT_Glyphs.API.FT_Get_Glyph (Slot_Ptr, Glyph_Ptr);
   end Get_Glyph;

   --  -------------------------------------------------------------------------

   function Get_Glyph_Advance (Slot_Ptr : FT.Types.FT_Glyph_Slot_Ptr) return FT_Image.FT_Vector is
      use Glyph_Slot_Access;
      Glyph : FT_Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Advance;
   end Get_Glyph_Advance;

   --  -------------------------------------------------------------------------

   function Get_Glyph_Format (Slot_Ptr : FT.Types.FT_Glyph_Slot_Ptr) return FT_Image.FT_Glyph_Format is
      use Glyph_Slot_Access;
      Glyph : FT_Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Format;
   end Get_Glyph_Format;

   --  -------------------------------------------------------------------------

   function Get_Glyph_Record (Face_Ptr : FT_Interface.FT_Face) return FT_Glyph_Record is
      use FT_Interface;
      use Glyph_Access;
      Glyph_Pointer : Object_Pointer :=
        To_Pointer (System.Address (Glyph_Slot (Face_Ptr)));
   begin
      return Glyph_Pointer.all;
   end Get_Glyph_Record;

   --  -------------------------------------------------------------------------

   function Glyph_To_Bitmap
     (theGlyph    : System.Address; Render_Mode : FT.Types.FT_Render_Mode;
      Origin      : access FT_Image.FT_Vector;
      Destroy     : FT.Types.FT_Bool) return FT.Types.FT_Error is
   begin
      return FT_Glyphs.API.FT_Glyph_To_Bitmap (theGlyph, Render_Mode,
                                               Origin, Destroy);
   end Glyph_To_Bitmap;

end FT_Glyphs;
