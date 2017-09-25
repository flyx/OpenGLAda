

with GL.Types;

with FT.API;
with FT.Image;
with FT.Interfac;

package FT.Utilities is

   procedure Load_Texture (Face_Ptr  : FT.API.Face_Ptr;
                           Char_Data : in out FT.Interfac.Character_Record;
                           Width, Height : GL.Types.Size;
                           X_Offset, Y_Offset : GL.Types.Int);
   procedure Print_Bitmap_Metadata (Bitmap : FT.Image.Bitmap_Record);
   procedure Print_Character_Metadata (aFace : FT.API.Face_Ptr;
                                       aChar : Character);
   procedure Print_Character_Metadata (Data : FT.Interfac.Character_Record);
end FT.Utilities;
