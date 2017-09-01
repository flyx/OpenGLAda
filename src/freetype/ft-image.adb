
with System.Address_To_Access_Conversions;

package body FT.Image is

   package Unsigned_Char_To_Address is new
       System.Address_To_Access_Conversions (Interfaces.C.unsigned_char);

   --  -------------------------------------------------------------------------

   --  FT_Glyph_Slot (SA) => FT_Glyph_Slot_Record => FT_Bitmap (record)
   --  FT_Bitmap => Buffer (access unsigned_char)

   function Get_Buffer (Bitmap : FT_Bitmap) return GL.Objects.Textures.Image_Source is
     Addr   : constant System.Address := Unsigned_Char_To_Address.To_Address (Bitmap.Buffer);
   begin
      return GL.Objects.Textures.Image_Source (Addr);
   end Get_Buffer;

   --  -------------------------------------------------------------------------

   function Get_Num_Grays (Bitmap : FT_Bitmap) return GL.Types.Short is
   begin
      return Bitmap.Num_Grays;
   end Get_Num_Grays;

   --  -------------------------------------------------------------------------

   function Get_Palette_Mode (Bitmap : FT_Bitmap) return unsigned_char is
   begin
      return Bitmap.Palette_Mode;
   end Get_Palette_Mode;

   --  -------------------------------------------------------------------------

   function Get_Pitch (Bitmap : FT_Bitmap) return GL.Types.Int is
   begin
      return Bitmap.Pitch;
   end Get_Pitch;

   --  -------------------------------------------------------------------------

   function Get_Pixel_Mode (Bitmap : FT_Bitmap) return unsigned_char is
   begin
      return Bitmap.Pixel_Mode;
   end Get_Pixel_Mode;

   --  -------------------------------------------------------------------------

   function Get_Rows (Bitmap : FT_Bitmap) return GL.Types.Int is
   begin
      return GL.Types.Int (Bitmap.Rows);
   end Get_Rows;

   --  -------------------------------------------------------------------------

   function Get_Width (Bitmap : FT_Bitmap) return GL.Types.UInt is
   begin
      return Bitmap.Width;
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

end FT.Image;
