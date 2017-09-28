--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with System.Address_To_Access_Conversions;

with FT.API.Glyphs;

package body FT.Glyphs is
   use type Errors.Error_Code;

   package Glyph_Slot_Access is new
       System.Address_To_Access_Conversions (Glyph_Slot_Record);
   package Glyph_Access is new
       System.Address_To_Access_Conversions (Glyph_Record);

   procedure Check_Face_Ptr (Face_Ptr : FT.API.Face_Ptr);
   procedure Check_Glyph_Ptr (thePtr : Glyph_Ptr);
   procedure Check_Glyph_Slot_Ptr (thePtr : FT.API.Glyph_Slot_Ptr);

   procedure Glyph (Slot_Ptr     : FT.API.Glyph_Slot_Ptr;
                    theGlyph_Ptr : in out Glyph_Ptr);

   --  -------------------------------------------------------------------------

   procedure Done_Glyph (Glyph : Glyph_Ptr) is
   begin
      Check_Glyph_Ptr (Glyph);
      FT.API.Glyphs.FT_Done_Glyph (Glyph);
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Done_Glyph raised an Exception";
   end Done_Glyph;

   --  -------------------------------------------------------------------------
   --  Glyph_Slot (SA) => Glyph_Slot_Record => Bitmap_Record
   --  Bitmap_Record => Buffer (access unsigned_char)

   procedure Bitmap (Face_Ptr : FT.API.Face_Ptr;
                    theBitmap : out FT.Image.Bitmap_Record) is
      use GL.Types;
      use Glyph_Slot_Access;
      Glyph_Slot    : FT.API.Glyph_Slot_Ptr;
      aGlyph_Ptr    : Glyph_Ptr;
      Glyph_Pointer :  Object_Pointer;
      theGlyph      :  Glyph_Slot_Record;
   begin
      Check_Face_Ptr (Face_Ptr);
      Glyph_Slot := FT.Faces.Glyph_Slot (Face_Ptr);
      Check_Glyph_Slot_Ptr (Glyph_Slot);
      Glyph_Pointer:= To_Pointer (System.Address (Glyph_Slot));
      theGlyph := Glyph_Pointer.all;
      Glyph (Glyph_Slot, aGlyph_Ptr);
      --  Glyph calls the FT_Glyph C function.
      theBitmap := theGlyph.Bitmap;
   exception
      when others =>
         raise FreeType_Exception with "FT.Glyphs.Bitmap raised an Exception";
   end Bitmap;

   --  -------------------------------------------------------------------------

   procedure Bitmap_Image (Face_Ptr : FT.API.Face_Ptr;
                          theImage : out GL.Objects.Textures.Image_Source) is
      use GL.Types;
      theBitmap  : FT.Image.Bitmap_Record;
   begin
      Check_Face_Ptr (Face_Ptr);
      Bitmap (Face_Ptr, theBitmap);
      theImage := FT.Image.Buffer (theBitmap);
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Bitmap_Image raised an Exception";
   end Bitmap_Image;

   --  -------------------------------------------------------------------------

   function Bitmap_Left (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Int is
      use Glyph_Slot_Access;
      Slot_Ptr   : FT.API.Glyph_Slot_Ptr;
      Glyph_Slot : Glyph_Slot_Record;
   begin
      Check_Face_Ptr (Face_Ptr);
      Slot_Ptr := FT.Faces.Glyph_Slot (Face_Ptr);
      Check_Glyph_Slot_Ptr (Slot_Ptr);
      Glyph_Slot := To_Pointer (System.Address (Slot_Ptr)).all;
      return Glyph_Slot.Bitmap_Left;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Bitmap_Left raised an Exception";
   end Bitmap_Left;

   --  -------------------------------------------------------------------------

   function Bitmap_Height (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Single is
      use GL.Types;
      theBitmap  : FT.Image.Bitmap_Record;
   begin
      Check_Face_Ptr (Face_Ptr);
      Bitmap (Face_Ptr, theBitmap);
      return GL.Types.Single (FT.Image.Rows (theBitmap));
   end Bitmap_Height;

   --  -------------------------------------------------------------------------

   function Bitmap_Rows (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Int is
      use GL.Types;
      theBitmap  : FT.Image.Bitmap_Record;
   begin
      Check_Face_Ptr (Face_Ptr);
      Bitmap (Face_Ptr, theBitmap);
      return FT.Image.Rows (theBitmap);
   end Bitmap_Rows;

   --  -------------------------------------------------------------------------

   function Bitmap_Top (Face_Ptr : FT.API.Face_Ptr)
                            return GL.Types.Int is
      use Glyph_Slot_Access;
      Slot_Ptr  : FT.API.Glyph_Slot_Ptr;
      Glyph     : Glyph_Slot_Record;
   begin
      Check_Face_Ptr (Face_Ptr);
      Slot_Ptr := FT.Faces.Glyph_Slot (Face_Ptr);
      Check_Glyph_Slot_Ptr (Slot_Ptr);
      Glyph := To_Pointer (System.Address (Slot_Ptr)).all;
      return Glyph.Bitmap_Top;
   exception
      when others =>
              raise FreeType_Exception with
                "FT.Glyphs.Bitmap_Top raised an Exception";
   end Bitmap_Top;

   --  -------------------------------------------------------------------------

   function Bitmap_Width (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Single is
      use GL.Types;
      theBitmap  : FT.Image.Bitmap_Record;
   begin
      Check_Face_Ptr (Face_Ptr);
      Bitmap (Face_Ptr, theBitmap);
      return GL.Types.Single (FT.Image.Width (theBitmap));
   end Bitmap_Width;

   --  -------------------------------------------------------------------------

   procedure Check_Face_Ptr (Face_Ptr : FT.API.Face_Ptr) is
      use System;
   begin
      if System.Address (Face_Ptr) = System.Null_Address then
         raise FreeType_Exception with
           "No face is loaded, Face_Ptr is null.";
      end if;
   end Check_Face_Ptr;

   --  -------------------------------------------------------------------------

   procedure Check_Glyph_Ptr (thePtr : Glyph_Ptr) is
      use System;
   begin
      if System.Address (thePtr) = System.Null_Address then
         raise FreeType_Exception with
           "No glyph is loaded, Glyph_Ptr is null.";
      end if;
   end Check_Glyph_Ptr;

   --  -------------------------------------------------------------------------

   procedure Check_Glyph_Slot_Ptr (thePtr : FT.API.Glyph_Slot_Ptr) is
      use System;
   begin
      if System.Address (thePtr) = System.Null_Address then
         raise FreeType_Exception with
           "No glyph is loaded, Glyph_Slot_Ptr is null.";
      end if;
   end Check_Glyph_Slot_Ptr;

   --  -------------------------------------------------------------------------

   procedure Glyph (Face_Ptr : FT.API.Face_Ptr; theGlyph : out Glyph_Record) is
   use GL.Types;
      use Glyph_Access;
      aGlyph_Slot : FT.API.Glyph_Slot_Ptr ;
      aGlyph_Ptr : Glyph_Ptr;
   begin
      Check_Face_Ptr (Face_Ptr);
      aGlyph_Slot := FT.Faces.Glyph_Slot (Face_Ptr);
      Check_Glyph_Slot_Ptr (aGlyph_Slot);
      Glyph (aGlyph_Slot, aGlyph_Ptr);
      Check_Glyph_Ptr (aGlyph_Ptr);
      theGlyph := Glyph (aGlyph_Ptr);
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph raised an Exception";
   end Glyph;

   --  -------------------------------------------------------------------------

   function Glyph (aGlyph_Ptr : Glyph_Ptr) return Glyph_Record is
      use Glyph_Access;
      Glyph_Acc : access Glyph_Record;
   begin
      Check_Glyph_Ptr (aGlyph_Ptr);
      Glyph_Acc := To_Pointer (System.Address (aGlyph_Ptr));
      return Glyph_Acc.all;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph aGlyph_Ptr raised an Exception";
   end Glyph;

   --  -------------------------------------------------------------------------

   procedure Glyph (Slot_Ptr    : FT.API.Glyph_Slot_Ptr;
                   theGlyph_Ptr : in out Glyph_Ptr) is
      use GL.Types;
      Code : Errors.Error_Code;
   begin
      Check_Glyph_Slot_Ptr (Slot_Ptr);
      Code := FT.API.Glyphs.FT_Get_Glyph (Slot_Ptr, System.Address (theGlyph_Ptr));
      if Code /= Errors.Ok then
         raise FreeType_Exception with
           "FT.Glyphs.Glyph error :" & Errors.Description (Code);
      end if;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph raised an Exception";
   end Glyph;

   --  -------------------------------------------------------------------------

   function Glyph_Advance (Face_Ptr : FT.API.Face_Ptr)
                               return FT.Image.FT_Vector is
      use Glyph_Slot_Access;
      Slot_Ptr : FT.API.Glyph_Slot_Ptr;
      Glyph    : Glyph_Slot_Record;
   begin
      Check_Face_Ptr (Face_Ptr);
      Slot_Ptr := FT.Faces.Glyph_Slot (Face_Ptr);
      Check_Glyph_Slot_Ptr (Slot_Ptr);
      Glyph := To_Pointer (System.Address (Slot_Ptr)).all;
      return Glyph.Advance;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph_Advance raised an Exception";
   end Glyph_Advance;

   --  -------------------------------------------------------------------------

   function Glyph_Format (Face_Ptr : FT.API.Face_Ptr)
                              return FT.Image.Glyph_Format is
      use Glyph_Slot_Access;
      Slot_Ptr : FT.API.Glyph_Slot_Ptr;
      Glyph    : Glyph_Slot_Record;
   begin
      Check_Face_Ptr (Face_Ptr);
      Slot_Ptr := FT.Faces.Glyph_Slot (Face_Ptr);
      Check_Glyph_Slot_Ptr (Slot_Ptr);
      Glyph := To_Pointer (System.Address (Slot_Ptr)).all;
      return Glyph.Format;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph_Format raised an Exception";
   end Glyph_Format;

   --  -------------------------------------------------------------------------

   function Glyph_To_Bitmap
     (theGlyph    : System.Address; Mode : FT.API.Render_Mode;
      Origin      : access FT.Image.FT_Vector;
      Destroy     : Bool) return Errors.Error_Code is
   begin
      return FT.API.Glyphs.FT_Glyph_To_Bitmap (theGlyph, Mode, Origin, Destroy);
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph_To_Bitmap raised an Exception";
   end Glyph_To_Bitmap;

   --  -------------------------------------------------------------------------

   function Render_Glyph (aFace : FT.API.Face_Ptr; Mode : FT.API.Render_Mode)
                          return Errors.Error_Code is
      Slot : FT.API.Glyph_Slot_Ptr;
   begin
      Check_Face_Ptr (aFace);
      Slot := FT.Faces.Glyph_Slot (aFace);
      return  FT.API.Glyphs.FT_Render_Glyph (Slot, Mode);
   end Render_Glyph;

   --  -------------------------------------------------------------------------

end FT.Glyphs;
