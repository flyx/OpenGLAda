
with FT.Image;
with FT.Interfac;

package FT.Utilities is

   procedure Print_Bitmap_Metadata (Bitmap : FT.Image.FT_Bitmap);
   procedure Print_Character_Metadata (Face_Ptr : FT.Interfac.FT_Face;
                                   aChar : Character);
end FT.Utilities;
