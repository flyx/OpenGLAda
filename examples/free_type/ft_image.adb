
with FT_Types;

package body FT_Image is

   function Get_Buffer (Bitmap : FT_Bitmap) return GL.Objects.Textures.Image_Source is
     Addr   : System.Address := FT_Types.Unsigned_Char_To_Address.To_Address (Bitmap.Buffer);
   begin
      return GL.Objects.Textures.Image_Source (Addr);
   end Get_Buffer;

   --  -------------------------------------------------------------------------

   function Get_Rows (Bitmap : FT_Bitmap) return GL.Types.Int is
   begin
      return GL.Types.Int (Bitmap.Rows);
   end Get_Rows;

   --  -------------------------------------------------------------------------

   function Get_Width (Bitmap : FT_Bitmap) return GL.Types.UInt is
   begin
      return GL.Types.UInt (Bitmap.Width);
   end Get_Width;

   --  -------------------------------------------------------------------------

   function Vector_X (theVector : FT_Vector) return GL.Types.Int is
   begin
      return GL.Types.Int (theVector.x);
   end Vector_X;

   --  -------------------------------------------------------------------------

   function Vector_Y (theVector : FT_Vector) return GL.Types.Int is
   begin
      return GL.Types.Int (theVector.y);
   end Vector_Y;

   --  -------------------------------------------------------------------------

end FT_Image;
