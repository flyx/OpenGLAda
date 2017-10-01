--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

with System.Address_To_Access_Conversions;

package body FT.Image is

   package Unsigned_Char_To_Address is new
       System.Address_To_Access_Conversions (Interfaces.C.unsigned_char);

   --  -------------------------------------------------------------------------

   --  Glyph_Slot (SA) => Glyph_Slot_Record => Bitmap (record)
   --  Bitmap => Buffer (access unsigned_char)

   function Buffer (Bitmap : Bitmap_Record) return GL.Objects.Textures.Image_Source is
      Addr   : constant System.Address := Unsigned_Char_To_Address.To_Address (Bitmap.Buffer);
   begin
      return GL.Objects.Textures.Image_Source (Addr);
   end Buffer;

   --  -------------------------------------------------------------------------

   function Num_Grays (Bitmap : Bitmap_Record) return GL.Types.Short is
   begin
      return Bitmap.Num_Grays;
   end Num_Grays;

   --  -------------------------------------------------------------------------

   function Palette_Mode (Bitmap : Bitmap_Record)
                          return Interfaces.C.unsigned_char is
   begin
      return Bitmap.Palette_Mode;
   end Palette_Mode;

   --  -------------------------------------------------------------------------

   function Pitch (Bitmap : Bitmap_Record) return GL.Types.Int is
   begin
      return Bitmap.Pitch;
   end Pitch;

   --  -------------------------------------------------------------------------

   function Pixel_Mode (Bitmap : Bitmap_Record)
                        return Interfaces.C.unsigned_char is
   begin
      return Bitmap.Pixel_Mode;
   end Pixel_Mode;

   --  -------------------------------------------------------------------------

   function Rows (Bitmap : Bitmap_Record) return GL.Types.Int is
   begin
      return GL.Types.Int (Bitmap.Rows);
   end Rows;

   --  -------------------------------------------------------------------------

   function Width (Bitmap : Bitmap_Record) return GL.Types.UInt is
   begin
      return Bitmap.Width;
   end Width;

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
