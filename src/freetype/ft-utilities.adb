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

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with FT.Faces.Glyphs;
with FT.Image;

package body FT.Utilities is
   use Interfaces.C;

   procedure Print_Bitmap_Metadata (Bitmap : FT.Bitmap_Record) is
      use GL.Types;
      use FT.Image;
   begin
      New_Line;
      Put_Line ("Bitmap data:");
      Put_Line ("Rows: " & GL.Types.int'Image (Rows (Bitmap)));
      Put_Line ("Width: " & uint'Image (Width (Bitmap)));
      Put_Line ("Pitch: " & GL.Types.int'Image (Pitch (Bitmap)));
      Put_Line ("Num_Grays: " & GL.Types.short'Image (Num_Grays (Bitmap)));
      Put_Line ("Pixel_mode: " & unsigned_char'Image (Pixel_Mode (Bitmap)));
      Put_Line ("Palette_mode: " & unsigned_char'Image (Palette_Mode (Bitmap)));
      New_Line;
   end Print_Bitmap_Metadata;

   --  -------------------------------------------------------------------------

   procedure Print_Character_Metadata (aFace : FT.Faces.Face_Reference;
                                       aChar : Character) is
      use GL.Types;
      Advance_X : constant GL.Types.Int := GL.Types.Int (Faces.Glyphs.Glyph_Advance (aFace).X);
      Size : constant FT.Bitmap_Size := aFace.Size;
   begin
      Put_Line ("Character " & aChar & " Data");
      Put_Line ("Width: " & GL.Types.Short'Image (Size.Width));
      Put_Line ("Height: " & GL.Types.Short'Image (Size.Height));
      Put_Line ("Size: " & FT.Position'Image (Size.Size));
      Put_Line ("Advance: " & GL.Types.Int'Image (Advance_X) & " bits");
      Put_Line ("Glyph format: " & FT.Glyph_Format'Image (FT.Faces.Glyphs.Format (aFace)));
      New_Line;
   end Print_Character_Metadata;

   --  ------------------------------------------------------------------------

   procedure Print_Character_Metadata (Data : FT.Faces.Character_Record) is
      Size : constant FT.Faces.Character_Size_Type := FT.Faces.Character_Size (Data);
   begin
      Put_Line ("Width: " & GL.Types.Int'Image (Size.Width));
      Put_Line ("Rows: " & GL.Types.Int'Image (Size.Rows));
      Put_Line ("Left: " & GL.Types.Int'Image (Size.Left));
      Put_Line ("Top: " & GL.Types.Int'Image (Size.Top));
      Put_Line ("Advance X: " & GL.Types.Int'Image (Size.Advance_X) & " bits");
      New_Line;
   end Print_Character_Metadata;

   --  ------------------------------------------------------------------------

end FT.Utilities;
