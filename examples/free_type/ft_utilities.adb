
with System;
with System.Address_Image;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with FT_Glyphs;
with FT_Image;

package body FT_Utilities is

   procedure Print_Character_Data (Face_Ptr : FT_Interface.FT_Face; aChar : Character) is
      use GL.Types;
      use FT_Glyphs;
      aFace     : FT_Interface.FT_Face_Record := FT_Interface.Face (Face_Ptr);
      Advance_X : GL.Types.Int := FT_Image.Vector_X (Get_Glyph_Advance (aFace.Glyph_Slot));
   begin
      Put_Line ("Character " & aChar & " Data");
      Put_Line ("Width: " & Single'Image (Get_Bitmap_Width (aFace.Glyph_Slot)));
      Put_Line ("Rows: " & Int'Image (Get_Bitmap_Rows (aFace.Glyph_Slot)));
      Put_Line ("Left: " & Int'Image (Get_Bitmap_Left (aFace.Glyph_Slot)));
      Put_Line ("Top: " & Int'Image (Get_Bitmap_Top (aFace.Glyph_Slot)));
      Put_Line ("Advance X: " & Int'Image (Advance_X) & " bits");
      Put_Line ("Bitmap address: " & System.Address_Image
                (System.Address (Get_Bitmap_Image (aFace.Glyph_Slot))));
      New_Line;
   end Print_Character_Data;

   --  ------------------------------------------------------------------------

end FT_Utilities;
