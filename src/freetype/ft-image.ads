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

with System;

with Interfaces.C.Strings;

with GL.Objects.Textures;
with GL.Types;

package FT.Image is
   pragma Preelaborate;

   type Bitmap_Record is private;
   type Outline_Record is private;
   type FT_Vector is private;
   type Glyph_Format is (Format_None, Bitmap_Format, Composite_Format,
                         Outline_Format, Plotter_Format);
   subtype FT_Pos is GL.Types.Long;

   function Buffer (Bitmap : Bitmap_Record) return GL.Objects.Textures.Image_Source;
   function Num_Grays (Bitmap : Bitmap_Record) return GL.Types.Short;
   function Rows (Bitmap : Bitmap_Record) return GL.Types.Int;
   function Palette_Mode (Bitmap : Bitmap_Record)
                          return Interfaces.C.unsigned_char;
   function Pitch (Bitmap : Bitmap_Record) return GL.Types.Int;
   function Pixel_Mode (Bitmap : Bitmap_Record)
                        return Interfaces.C.unsigned_char;
   function Width (Bitmap : Bitmap_Record) return GL.Types.UInt;
   function Vector_X (theVector : FT_Vector) return GL.Types.Int;
   function Vector_Y (theVector : FT_Vector) return GL.Types.Int;

private
   type Bitmap_Record is record
      Rows         : GL.Types.UInt;
      Width        : GL.Types.UInt;
      Pitch        : GL.Types.Int;
      Buffer       : access Interfaces.C.unsigned_char;
      Num_Grays    : GL.Types.Short;
      Pixel_Mode   : Interfaces.C.unsigned_char;
      Palette_Mode : Interfaces.C.unsigned_char;
      Palette      : System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, Bitmap_Record);

   type Outline_Record is record
      Num_Contours : GL.Types.short;
      Num_Points   : GL.Types.short;
      Points       : access FT_Vector;
      Tags         : Interfaces.C.Strings.chars_ptr;
      Contours     : access GL.Types.short;
      Flags        : GL.Types.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Outline_Record);

   type FT_Vector is record
      x : FT_Pos;
      y : FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Vector);

   for Glyph_Format use (Format_None        => 0000000000,
                         Bitmap_Format      => 1651078259,
                         Composite_Format   => 1668246896,
                         Outline_Format     => 1869968492,
                         Plotter_Format     => 1886154612);
   for Glyph_Format'Size use Interfaces.C.unsigned'Size;
end FT.Image;
