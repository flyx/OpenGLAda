
with FT.API;
with FT.Image;
with FT.Interfac;

package FT.Utilities is

   procedure Print_Bitmap_Metadata (Bitmap : FT.Image.Bitmap_Record);
   procedure Print_Character_Metadata (aFace : FT.API.Face_Ptr;
                                       aChar : Character);
   procedure Print_Character_Metadata (Data : FT.Interfac.Character_Record);
   procedure Setup_Character_Textures
       (Face_Ptr       : FT.API.Face_Ptr;
        Character_Data : in out FT.Interfac.Character_Data_Vector);
end FT.Utilities;
