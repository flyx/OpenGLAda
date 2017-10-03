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

with Errors;
with FT.API.Glyphs;
with FT.Image;

package body FT.Faces.Glyphs is
   use type Errors.Error_Code;

   procedure Check_Glyph_Ptr (thePtr : Glyph_Ptr);
   procedure Check_Glyph_Access (thePtr : access Glyph_Record);
   procedure Glyph_Slot (Object : FT.Faces.Face_Reference;
                         theGlyph_Slot : out Glyph_Slot_Record);
   function Glyph_Access (aSlot_Ptr    : Glyph_Slot_Ptr)
                          return access Glyph_Record;

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

   procedure Bitmap (Object : Face_Reference;
                     theBitmap : out Bitmap_Record) is
      theGlyph      : Glyph_Slot_Record;
   begin
      Glyph_Slot (Object, theGlyph);
      theBitmap := theGlyph.Bitmap;
   exception
      when others =>
         raise FreeType_Exception with "FT.Glyphs.Bitmap raised an Exception";
   end Bitmap;

   --  -------------------------------------------------------------------------

   procedure Bitmap_Image (Object : Face_Reference;
                          theImage : out GL.Objects.Textures.Image_Source) is
      theBitmap  : Bitmap_Record;
   begin
      Check_Face_Ptr (Object);
      Bitmap (Object, theBitmap);
      theImage := FT.Image.Buffer (theBitmap);
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Bitmap_Image raised an Exception";
   end Bitmap_Image;

   --  -------------------------------------------------------------------------

   function Bitmap_Left (Object : Face_Reference) return GL.Types.Int is
      theGlyph_Slot : Glyph_Slot_Record;
   begin
      Glyph_Slot (Object, theGlyph_Slot);
      return theGlyph_Slot.Bitmap_Left;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Bitmap_Left raised an Exception";
   end Bitmap_Left;

   --  -------------------------------------------------------------------------

   function Bitmap_Height (Object : Face_Reference) return GL.Types.Single is
      theBitmap  : Bitmap_Record;
   begin
      Check_Face_Ptr (Object);
      Bitmap (Object, theBitmap);
      return GL.Types.Single (FT.Image.Rows (theBitmap));
   end Bitmap_Height;

   --  -------------------------------------------------------------------------

   function Bitmap_Rows (Object : Face_Reference) return GL.Types.Int is
      theBitmap  : Bitmap_Record;
   begin
      Check_Face_Ptr (Object);
      Bitmap (Object, theBitmap);
      return FT.Image.Rows (theBitmap);
   end Bitmap_Rows;

   --  -------------------------------------------------------------------------

   function Bitmap_Top (Object : Face_Reference)
                            return GL.Types.Int is
      theGlyph_Slot : Glyph_Slot_Record;
   begin
      Glyph_Slot (Object, theGlyph_Slot);
      return theGlyph_Slot.Bitmap_Top;
   exception
      when others =>
              raise FreeType_Exception with
                "FT.Glyphs.Bitmap_Top raised an Exception";
   end Bitmap_Top;

   --  -------------------------------------------------------------------------

   function Bitmap_Width (Object : Face_Reference) return GL.Types.Single is
      theBitmap  : Bitmap_Record;
   begin
      Check_Face_Ptr (Object);
      Bitmap (Object, theBitmap);
      return GL.Types.Single (FT.Image.Width (theBitmap));
   end Bitmap_Width;

   --  -------------------------------------------------------------------------

   procedure Check_Glyph_Ptr (thePtr : Glyph_Ptr) is
   begin
      if thePtr = Null then
         raise FreeType_Exception with
           "FT.Glyphs.Check_Glyph_Ptr - No glyph is loaded, Glyph_Ptr is null.";
      end if;
   end Check_Glyph_Ptr;

   --  -------------------------------------------------------------------------

   procedure Check_Glyph_Access (thePtr : access Glyph_Record) is
   begin
      if thePtr = Null then
         raise FreeType_Exception with
           "FT.Glyphs.Check_Glyph_Access - No glyph is loaded, Glyph_Ptr is null.";
      end if;
   end Check_Glyph_Access;

   --  -------------------------------------------------------------------------

   function Glyph_Access (aSlot_Ptr    : Glyph_Slot_Ptr)
                          return access Glyph_Record is
      theGlyph_Ptr : Glyph_Ptr;
      Code         : Errors.Error_Code;
   begin
      Check_Glyph_Slot_Ptr (aSlot_Ptr);
      Code := API.Glyphs.FT_Get_Glyph (aSlot_Ptr, theGlyph_Ptr);
      if Code /= Errors.Ok then
         raise FreeType_Exception with
           "FT.Glyphs.Glyph_Access error :" & Errors.Description (Code);
      end if;
      return theGlyph_Ptr;

   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph raised an Exception";
   end Glyph_Access;

   --  -------------------------------------------------------------------------

   procedure Glyph_Slot (Object : Face_Reference;
                         theGlyph_Slot : out Glyph_Slot_Record) is
      aSlot_Ptr    : access Glyph_Slot_Record;
   begin
      Check_Face_Ptr (Object);
      aSlot_Ptr := Slot_Ptr (Object);
      theGlyph_Slot := aSlot_Ptr.all;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph_Slot raised an Exception";
   end Glyph_Slot;

   --  -------------------------------------------------------------------------

   procedure Glyph (Object : Face_Reference; theGlyph : out Glyph_Record) is
      aSlot_Ptr  : Glyph_Slot_Ptr;
      aGlyph_Ptr : access Glyph_Record;
   begin
      Check_Face_Ptr (Object);
      aSlot_Ptr := Slot_Ptr (Object);
      Check_Glyph_Slot_Ptr (aSlot_Ptr);
      aGlyph_Ptr := Glyph_Access (aSlot_Ptr);
      Check_Glyph_Access (aGlyph_Ptr);
      theGlyph := Glyph (aGlyph_Ptr);
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph raised an Exception";
   end Glyph;

   --  -------------------------------------------------------------------------

   function Glyph (aGlyph_Ptr : access Glyph_Record) return Glyph_Record is
   begin
      Check_Glyph_Access (aGlyph_Ptr);
      return aGlyph_Ptr.all;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph aGlyph_Ptr raised an Exception";
   end Glyph;

   --  -------------------------------------------------------------------------

   function Glyph_Advance (Object : Face_Reference) return Vector is
      aGlyph_Slot : Glyph_Slot_Record;
   begin
      Glyph_Slot (Object, aGlyph_Slot);
      return aGlyph_Slot.Advance;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph_Advance raised an Exception";
   end Glyph_Advance;

   --  -------------------------------------------------------------------------

   function Format (Object : Face_Reference) return Glyph_Format is
      aGlyph_Slot : Glyph_Slot_Record;
   begin
      Glyph_Slot (Object, aGlyph_Slot);
      return aGlyph_Slot.Format;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph_Format raised an Exception";
   end Format;

   --  -------------------------------------------------------------------------

   procedure Glyph_To_Bitmap
     (theGlyph    : Glyph_Ptr; Mode : FT.Faces.Render_Mode;
      Origin      : access Vector; Destroy     : Boolean) is
      use Errors;
      Code : constant Errors.Error_Code :=
               API.Glyphs.FT_Glyph_To_Bitmap (theGlyph, Mode, Origin, FT.API.Bool (Destroy));
   begin
      if Code /= Errors.Ok then
         raise FT.FreeType_Exception with "FT.Glyphs.Glyph_To_Bitmap error: " &
             Errors.Description (Code);
      end if;
   end Glyph_To_Bitmap;

   --  -------------------------------------------------------------------------

   procedure Render_Glyph (Object : Face_Reference; Mode : FT.Faces.Render_Mode) is
      use Errors;
      Slot : access Glyph_Slot_Record;
      Code : Errors.Error_Code;
   begin
      Check_Face_Ptr (Object);
      Slot := Slot_Ptr (Object);
      Code := FT.API.Glyphs.FT_Render_Glyph (Slot, Mode);
      if Code /= Errors.Ok then
         raise FT.FreeType_Exception with "FT.Glyphs.Render_Glyph error: " &
             Errors.Description (Code);
      end if;
   end Render_Glyph;

   --  -------------------------------------------------------------------------

end FT.Faces.Glyphs;
