--------------------------------------------------------------------------------
-- Copyright (c) 2017, Felix Krause <contact@flyx.org>
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

with Ada.Text_IO; use Ada.Text_IO;

with FT.Glyphs;

package body FT.Utilities is
   use Interfaces.C;

   procedure Print_Bitmap_Metadata (Bitmap : FT.Bitmap_Record) is
   begin
      New_Line;
      Put_Line ("Bitmap data:");
      Put_Line ("Rows: " & Interfaces.C.unsigned'Image (Bitmap.Rows));
      Put_Line ("Width: " & Interfaces.C.unsigned'Image (Bitmap.Width));
      Put_Line ("Pitch: " & Interfaces.C.int'Image (Bitmap.Pitch));
      Put_Line ("Num_Grays: " & Interfaces.C.short'Image (Bitmap.Num_Grays));
      Put_Line ("Pixel_mode: " & unsigned_char'Image (Bitmap.Pixel_Mode));
      Put_Line ("Palette_mode: " & unsigned_char'Image (Bitmap.Palette_Mode));
      New_Line;
   end Print_Bitmap_Metadata;

   --  -------------------------------------------------------------------------

   procedure Print_Character_Metadata (aFace : FT.Faces.Face_Reference;
                                       aChar : Character) is
      Slot : constant FT.Glyph_Slot_Reference := aFace.Glyph_Slot;
      Bitmap : constant FT.Bitmap_Record := Glyphs.Bitmap (Slot);
   begin
      Put_Line ("Character " & aChar & " Data");
      Put_Line ("Width: " & Interfaces.C.unsigned'Image (Bitmap.Width));
      Put_Line ("Rows: " & Interfaces.C.unsigned'Image (Bitmap.Rows));
      Put_Line ("Left: " & Interfaces.C.int'Image (Glyphs.Bitmap_Left (Slot)));
      Put_Line ("Top: " & Interfaces.C.int'Image (Glyphs.Bitmap_Top (Slot)));
      Put_Line ("Advance X: " & Position'Image (Glyphs.Advance (Slot).X) & " bits");
      Put_Line ("Glyph format: " & FT.Glyph_Format'Image (Glyphs.Format (Slot)));
      New_Line;
   end Print_Character_Metadata;

   --  ------------------------------------------------------------------------

end FT.Utilities;
