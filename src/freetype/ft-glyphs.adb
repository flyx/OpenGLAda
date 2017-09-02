
with System.Address_To_Access_Conversions;

with Ada.Text_IO; use Ada.Text_IO;

with FT.API.Glyphs;

package body FT.Glyphs is
   package Glyph_Slot_Access is new
       System.Address_To_Access_Conversions (Glyph_Slot_Record);

   procedure Done_Glyph (Glyph : Glyph_Ptr) is
   begin
      FT.API.Glyphs.FT_Done_Glyph (Glyph);
   end Done_Glyph;

   --  -------------------------------------------------------------------------
   --  Glyph_Slot (SA) => Glyph_Slot_Record => Bitmap_Record
   --  Bitmap_Record => Buffer (access unsigned_char)

   function Bitmap (Glyph_Slot : FT.API.Glyph_Slot_Ptr)
                        return FT.Image.Bitmap_Record is
      use GL.Types;
      use Glyph_Slot_Access;
      aGlyph_Ptr    : System.Address;
      Glyph_Pointer : constant Object_Pointer :=
                        To_Pointer (System.Address (Glyph_Slot));
      theGlyph      : constant Glyph_Slot_Record := Glyph_Pointer.all;
   begin
      --  Glyph calls the FT_Glyph C function.
      if Glyph (Glyph_Slot, aGlyph_Ptr) /= 0 then
         Put_Line ("FT_Interfac.Bitmap raised an Exception");
         raise FT.FT_Exception;
      end if;
      return theGlyph.Bitmap;
   end Bitmap;

   --  -------------------------------------------------------------------------
   function Bitmap_Image (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                              return GL.Objects.Textures.Image_Source is
   begin
      return FT.Image.Buffer (Bitmap (Slot_Ptr));
   end Bitmap_Image;

   --  -------------------------------------------------------------------------

   function Bitmap_Left (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                             return GL.Types.Int is
      use Glyph_Slot_Access;
      Glyph : constant Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Bitmap_Left;
   end Bitmap_Left;

   --  -------------------------------------------------------------------------

   function Bitmap_Width (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                              return GL.Types.Single is
      theBitmap : constant FT.Image.Bitmap_Record := Bitmap (Slot_Ptr);
   begin
      return GL.Types.Single (FT.Image.Width (theBitmap));
   end Bitmap_Width;

   --  -------------------------------------------------------------------------

   function Bitmap_Height (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                               return GL.Types.Single is
      theBitmap : constant FT.Image.Bitmap_Record := Bitmap (Slot_Ptr);
   begin
      return GL.Types.Single (FT.Image.Rows (theBitmap));
   end Bitmap_Height;

   --  -------------------------------------------------------------------------

   function Bitmap_Rows (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                             return GL.Types.Int is
      theBitmap : constant FT.Image.Bitmap_Record := Bitmap (Slot_Ptr);
   begin
      return FT.Image.Rows (theBitmap);
   end Bitmap_Rows;

   --  -------------------------------------------------------------------------

   function Bitmap_Top (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                            return GL.Types.Int is
      use Glyph_Slot_Access;
      Glyph : constant Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Bitmap_Top;
   end Bitmap_Top;

   --  -------------------------------------------------------------------------

   function Glyph (Slot_Ptr : FT.API.Glyph_Slot_Ptr;
                       Glyph_Ptr : in out System.Address)
                       return FT.FT_Error is
   begin
      return FT.API.Glyphs.FT_Get_Glyph (Slot_Ptr, Glyph_Ptr);
   end Glyph;

   --  -------------------------------------------------------------------------

   function Glyph_Advance (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                               return FT.Image.FT_Vector is
      use Glyph_Slot_Access;
      Glyph : constant Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Advance;
   end Glyph_Advance;

   --  -------------------------------------------------------------------------

   function Glyph_Format (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                              return FT.Image.Glyph_Format is
      use Glyph_Slot_Access;
      Glyph : constant Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Format;
   end Glyph_Format;

   --  -------------------------------------------------------------------------

   function Glyph_To_Bitmap
     (theGlyph    : System.Address; Mode : FT.API.Render_Mode;
      Origin      : access FT.Image.FT_Vector;
      Destroy     : FT.FT_Bool) return FT.FT_Error is
   begin
      return FT.API.Glyphs.FT_Glyph_To_Bitmap (theGlyph, Mode,
                                               Origin, Destroy);
   end Glyph_To_Bitmap;

end FT.Glyphs;
