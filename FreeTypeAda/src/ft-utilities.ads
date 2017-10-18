--  part of FreeTypeAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with FT.Faces;

package FT.Utilities is
   procedure Print_Bitmap_Metadata (Bitmap : FT.Bitmap_Record);
   procedure Print_Character_Metadata (aFace : FT.Faces.Face_Reference;
                                       aChar : Character);
end FT.Utilities;
