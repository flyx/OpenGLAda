
with System;
with System.Address_Image;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with FT_Glyphs;
with FT_Image;
with FT_Types;

package body FT_Utilities is

   procedure Print_Character_Data (Face_Ptr : FT_Interface.FT_Face; aChar : Character) is
      use GL.Types;
      use FT_Glyphs;
      Slot_Ptr  : FT_Types.FT_Glyph_Slot_Ptr := FT_Interface.Get_Glyph_Slot (Face_Ptr);
      Advance_X : GL.Types.Int := FT_Image.Vector_X (Get_Glyph_Advance (Slot_Ptr));
   begin
      Put_Line ("Character " & aChar & " Data");
      Put_Line ("Width: " & Single'Image (Get_Bitmap_Width (Slot_Ptr)));
      Put_Line ("Rows: " & Int'Image (Get_Bitmap_Rows (Slot_Ptr)));
      Put_Line ("Left: " & Int'Image (Get_Bitmap_Left (Slot_Ptr)));
      Put_Line ("Top: " & Int'Image (Get_Bitmap_Top (Slot_Ptr)));
      Put_Line ("Advance X: " & Int'Image (Advance_X) & " bits");
      Put_Line ("Glyph format: " & FT_Image.FT_Glyph_Format'Image (Get_Glyph_Format (Slot_Ptr)));
      Put_Line ("Bitmap address: " & System.Address_Image
                (System.Address (Get_Bitmap_Image (Slot_Ptr))));
      New_Line;
   end Print_Character_Data;

   --  ------------------------------------------------------------------------

end FT_Utilities;
