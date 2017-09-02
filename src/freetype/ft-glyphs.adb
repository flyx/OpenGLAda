
with System.Address_To_Access_Conversions;

with Ada.Text_IO; use Ada.Text_IO;

with FT.API.Glyphs;

package body FT.Glyphs is
   package Glyph_Access is new
       System.Address_To_Access_Conversions (Glyph_Record);
   package Glyph_Slot_Access is new
       System.Address_To_Access_Conversions (Glyph_Slot_Record);

   procedure Done_Glyph (Glyph : Glyph_Ptr) is
   begin
      FT.API.Glyphs.FT_Done_Glyph (Glyph);
   end Done_Glyph;

   --  -------------------------------------------------------------------------
   --  Glyph_Slot (SA) => Glyph_Slot_Record => Bitmap_Record
   --  Bitmap_Record => Buffer (access unsigned_char)

   function Get_Bitmap (Glyph_Slot : FT.API.Glyph_Slot_Ptr)
                        return FT.Image.Bitmap_Record is
      use GL.Types;
      use Glyph_Slot_Access;
      aGlyph_Ptr    : System.Address;
      Glyph_Pointer : constant Object_Pointer :=
                        To_Pointer (System.Address (Glyph_Slot));
      theGlyph      : constant Glyph_Slot_Record := Glyph_Pointer.all;
   begin
      --  Get_Glyph calls the FT_Get_Glyph C function.
      if Get_Glyph (Glyph_Slot, aGlyph_Ptr) /= 0 then
         Put_Line ("FT_Interfac.Bitmap raised an Exception");
         raise FT.FT_Exception;
      end if;
      return theGlyph.Bitmap;
   end Get_Bitmap;

   --  -------------------------------------------------------------------------
   function Get_Bitmap_Image (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                              return GL.Objects.Textures.Image_Source is
   begin
      return FT.Image.Buffer (Get_Bitmap (Slot_Ptr));
   end Get_Bitmap_Image;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Left (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                             return GL.Types.Int is
      use Glyph_Slot_Access;
      Glyph : constant Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Bitmap_Left;
   end Get_Bitmap_Left;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Width (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                              return GL.Types.Single is
      Bitmap : constant FT.Image.Bitmap_Record := Get_Bitmap (Slot_Ptr);
   begin
      return GL.Types.Single (FT.Image.Width (Bitmap));
   end Get_Bitmap_Width;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Height (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                               return GL.Types.Single is
      Bitmap : constant FT.Image.Bitmap_Record := Get_Bitmap (Slot_Ptr);
   begin
      return GL.Types.Single (FT.Image.Rows (Bitmap));
   end Get_Bitmap_Height;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Rows (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                             return GL.Types.Int is
      Bitmap : constant FT.Image.Bitmap_Record := Get_Bitmap (Slot_Ptr);
   begin
      return FT.Image.Rows (Bitmap);
   end Get_Bitmap_Rows;

   --  -------------------------------------------------------------------------

   function Get_Bitmap_Top (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                            return GL.Types.Int is
      use Glyph_Slot_Access;
      Glyph : constant Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Bitmap_Top;
   end Get_Bitmap_Top;

   --  -------------------------------------------------------------------------

   function Get_Glyph (Slot_Ptr : FT.API.Glyph_Slot_Ptr;
                       Glyph_Ptr : in out System.Address)
                       return FT.FT_Error is
   begin
      return FT.API.Glyphs.FT_Get_Glyph (Slot_Ptr, Glyph_Ptr);
   end Get_Glyph;

   --  -------------------------------------------------------------------------

   function Get_Glyph_Advance (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                               return FT.Image.FT_Vector is
      use Glyph_Slot_Access;
      Glyph : constant Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Advance;
   end Get_Glyph_Advance;

   --  -------------------------------------------------------------------------

   function Get_Glyph_Format (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                              return FT.Image.Glyph_Format is
      use Glyph_Slot_Access;
      Glyph : constant Glyph_Slot_Record :=
        To_Pointer (System.Address (Slot_Ptr)).all;
   begin
      return Glyph.Format;
   end Get_Glyph_Format;

   --  -------------------------------------------------------------------------

   function Get_Glyph_Record (aFace : FT.API.Face_Ptr)
                              return Glyph_Record is
      use FT.Interfac;
      use Glyph_Access;
      Glyph_Pointer : constant Object_Pointer :=
        To_Pointer (System.Address (Glyph_Slot (aFace)));
   begin
      return Glyph_Pointer.all;
   end Get_Glyph_Record;

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
