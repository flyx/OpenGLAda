
with System;
with System.Address_Image;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with FT.Glyphs;
with FT.Interfac;

package body FT.Utilities is

   procedure Print_Bitmap_Metadata (Bitmap : FT.Image.Bitmap_Record) is
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

   procedure Print_Character_Metadata (aFace : FT.API.Face_Ptr; aChar : Character) is
      use GL.Types;
      use FT.Glyphs;
      Slot_Ptr  : constant FT.API.Glyph_Slot_Ptr := FT.Interfac.Glyph_Slot (aFace);
      Advance_X : constant GL.Types.Int := FT.Image.Vector_X (Glyph_Advance (Slot_Ptr));
   begin
      Put_Line ("Character " & aChar & " Data");
      Put_Line ("Width: " & Single'Image (Bitmap_Width (Slot_Ptr)));
      Put_Line ("Rows: " & GL.Types.Int'Image (Bitmap_Rows (Slot_Ptr)));
      Put_Line ("Left: " & GL.Types.Int'Image (Bitmap_Left (Slot_Ptr)));
      Put_Line ("Top: " & GL.Types.Int'Image (Bitmap_Top (Slot_Ptr)));
      Put_Line ("Advance X: " & GL.Types.Int'Image (Advance_X) & " bits");
      Put_Line ("Glyph format: " & FT.Image.Glyph_Format'Image (Glyph_Format (Slot_Ptr)));
      Put_Line ("Bitmap address: " & System.Address_Image
                (System.Address (Bitmap_Image (Slot_Ptr))));
      New_Line;
   end Print_Character_Metadata;

   --  ------------------------------------------------------------------------

end FT.Utilities;
