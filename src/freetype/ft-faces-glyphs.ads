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

with System;

with GL.Objects.Textures;
with GL.Types;

package FT.Faces.Glyphs is
   pragma Preelaborate;

   type Glyph_Record is private;
   type Glyph_Ptr is private;

   procedure Done_Glyph (Glyph : Glyph_Ptr);

   procedure Bitmap (Object : Face_Reference;
                     theBitmap : out Bitmap_Record);
   function Bitmap_Height (Object : Face_Reference) return GL.Types.Single;
   procedure Bitmap_Image (Object : Face_Reference;
                          theImage : out GL.Objects.Textures.Image_Source);
   function Bitmap_Left (Object : Face_Reference) return GL.Types.Int;
   function Bitmap_Rows (Object : Face_Reference) return GL.Types.Int;
   function Bitmap_Top (Object : Face_Reference)
                        return GL.Types.Int;
   function Bitmap_Width (Object : Face_Reference) return GL.Types.Single;
   function Glyph_Advance (Object : Face_Reference) return Vector;
   function Format (Object : Face_Reference) return Glyph_Format;
   procedure Glyph_To_Bitmap
     (theGlyph    : Glyph_Ptr; Mode : FT.Faces.Render_Mode;
      Origin      : access Vector; Destroy     : Boolean);
   procedure Render_Glyph (Object : Face_Reference; Mode : FT.Faces.Render_Mode);
private
   procedure Glyph (Object : Face_Reference; theGlyph : out Glyph_Record);
   function Glyph (aGlyph_Ptr : access Glyph_Record) return Glyph_Record;

   type Glyph_Ptr is access Glyph_Record;
   pragma Convention (C, Glyph_Ptr);

   type Outline_Glyph_Record;
   type Outline_Glyph_Ptr is access Outline_Glyph_Record;
   pragma Convention (C, Outline_Glyph_Ptr);

   type Glyph_Record is record
      Library : FT.Library_Ptr;
      Clazz   : System.Address;
      Format  : Glyph_Format;
      Advance : Vector;
   end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Record);

   --  A Glyph can be typecast to an Outline_Glyph if
   --  glyph->format == GLYPH_FORMAT_OUTLINE.
   --  This provides easy access the outline's content.
   --  As the outline is extracted from a glyph slot, its coordinates are
   --  expressed normally in 26.6 pixels, unless the flag
   --  LOAD_NO_SCALE was used in FT_Load_Glyph() or FT_Load_Char().
   type Outline_Glyph_Record is record
      Root    : Glyph_Record;
      Outline : Outline_Record;
   end record;
   pragma Convention (C_Pass_By_Copy, Outline_Glyph_Record);
end FT.Faces.Glyphs;
