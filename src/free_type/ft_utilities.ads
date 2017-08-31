
with FT_Image;
with FT_Interface;

package FT_Utilities is

   procedure Print_Bitmap (Bitmap : FT_Image.FT_Bitmap);
   procedure Print_Character_Data (Face_Ptr : FT_Interface.FT_Face;
                                   aChar : Character);
end FT_Utilities;
