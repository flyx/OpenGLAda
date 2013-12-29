--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with GL.API;
with GL.Enums.Getter;

package body GL.Pixels is

   procedure Set_Pack_Swap_Bytes   (Value : Boolean) is
   begin
      API.Pixel_Store (Enums.Pack_Swap_Bytes, Low_Level.Bool (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Swap_Bytes;

   procedure Set_Pack_LSB_First    (Value : Boolean) is
   begin
      API.Pixel_Store (Enums.Pack_LSB_First, Low_Level.Bool (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_LSB_First;

   procedure Set_Pack_Row_Length   (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Row_Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Row_Length;

   procedure Set_Pack_Image_Height (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Image_Height, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Image_Height;

   procedure Set_Pack_Skip_Pixels  (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Skip_Pixels, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Skip_Pixels;

   procedure Set_Pack_Skip_Rows    (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Skip_Rows, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Skip_Rows;

   procedure Set_Pack_Skip_Images  (Value : Size) is
   begin
      API.Pixel_Store (Enums.Pack_Skip_Images, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Skip_Images;

   procedure Set_Pack_Alignment    (Value : Alignment) is
   begin
      API.Pixel_Store (Enums.Pack_Alignment, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Pack_Alignment;

   function Pack_Swap_Bytes   return Boolean is
      Ret : aliased Low_Level.Bool;
   begin
      API.Get_Boolean (Enums.Getter.Pack_Swap_Bytes, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Pack_Swap_Bytes;

   function Pack_LSB_First    return Boolean is
      Ret : aliased Low_Level.Bool;
   begin
      API.Get_Boolean (Enums.Getter.Pack_Lsb_First, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Pack_LSB_First;

   function Pack_Row_Length   return Size is
      Ret : aliased Types.Int;
   begin
      API.Get_Integer (Enums.Getter.Pack_Row_Length, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Row_Length;

   function Pack_Image_Height return Size is
      Ret : aliased Types.Int;
   begin
      API.Get_Integer (Enums.Getter.Pack_Image_Height, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Image_Height;

   function Pack_Skip_Pixels  return Size is
      Ret : aliased Types.Int;
   begin
      API.Get_Integer (Enums.Getter.Pack_Skip_Pixels, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Skip_Pixels;

   function Pack_Skip_Rows    return Size is
      Ret : aliased Types.Int;
   begin
      API.Get_Integer (Enums.Getter.Pack_Skip_Rows, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Skip_Rows;

   function Pack_Skip_Images  return Size is
      Ret : aliased Types.Int;
   begin
      API.Get_Integer (Enums.Getter.Pack_Skip_Images, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Pack_Skip_Images;

   function Pack_Alignment    return Alignment is
      Ret : aliased Alignment;
   begin
      API.Get_Alignment (Enums.Getter.Pack_Alignment, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Pack_Alignment;

   procedure Set_Unpack_Swap_Bytes   (Value : Boolean) is
   begin
      API.Pixel_Store (Enums.Unpack_Swap_Bytes, Low_Level.Bool (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Swap_Bytes;

   procedure Set_Unpack_LSB_First    (Value : Boolean) is
   begin
      API.Pixel_Store (Enums.Unpack_LSB_First, Low_Level.Bool (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_LSB_First;

   procedure Set_Unpack_Row_Length   (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Row_Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Row_Length;

   procedure Set_Unpack_Image_Height (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Image_Height, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Image_Height;

   procedure Set_Unpack_Skip_Pixels  (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Skip_Pixels, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Skip_Pixels;

   procedure Set_Unpack_Skip_Rows    (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Skip_Rows, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Skip_Rows;

   procedure Set_Unpack_Skip_Images  (Value : Size) is
   begin
      API.Pixel_Store (Enums.Unpack_Skip_Images, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Skip_Images;

   procedure Set_Unpack_Alignment    (Value : Alignment) is
   begin
      API.Pixel_Store (Enums.Unpack_Alignment, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Unpack_Alignment;

   function Unpack_Swap_Bytes   return Boolean is
      Ret : aliased Low_Level.Bool;
   begin
      API.Get_Boolean (Enums.Getter.Unpack_Swap_Bytes, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Unpack_Swap_Bytes;

   function Unpack_LSB_First    return Boolean is
      Ret : aliased Low_Level.Bool;
   begin
      API.Get_Boolean (Enums.Getter.Unpack_Lsb_First, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Unpack_LSB_First;

   function Unpack_Row_Length   return Size is
      Ret : aliased Types.Int;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Row_Length, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Row_Length;

   function Unpack_Image_Height return Size is
      Ret : aliased Types.Int;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Image_Height, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Image_Height;

   function Unpack_Skip_Pixels  return Size is
      Ret : aliased Types.Int;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Skip_Pixels, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Skip_Pixels;

   function Unpack_Skip_Rows    return Size is
      Ret : aliased Types.Int;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Skip_Rows, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Skip_Rows;

   function Unpack_Skip_Images  return Size is
      Ret : aliased Types.Int;
   begin
      API.Get_Integer (Enums.Getter.Unpack_Skip_Images, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Size (Ret);
   end Unpack_Skip_Images;

   function Unpack_Alignment    return Alignment is
      Ret : aliased Alignment;
   begin
      API.Get_Alignment (Enums.Getter.Unpack_Alignment, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Unpack_Alignment;

end GL.Pixels;
