
with FT.Image;
with FT.Interfac;

package FT.Utilities is

   procedure Print_Bitmap_Metadata (Bitmap : FT.Image.FT_Bitmap);
   procedure Print_Character_Metadata (aFace : FT.Interfac.Face_Ptr;
                                   aChar : Character);
end FT.Utilities;
