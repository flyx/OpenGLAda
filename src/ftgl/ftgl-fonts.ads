--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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

with Ada.Finalization;
with System;

with GL.Types;

with FTGL.Errors;

package FTGL.Fonts is

   type Charset_List is array (Positive range <>) of aliased Charset;

   subtype Depth is GL.Types.Single;

   -- reference counted
   type Font is abstract new Ada.Finalization.Controlled with private;

   overriding
   procedure Initialize (Object : in out Font);

   overriding
   procedure Adjust (Object : in out Font);

   overriding
   procedure Finalize (Object : in out Font);

   procedure Load (Object : in out Font; Path : String) is abstract;
   function Loaded (Object : Font) return Boolean;

   procedure Set_Char_Map (Object : in out Font; Value : Charset);
   function Get_Char_Map_List (Object : Font) return Charset_List;

   procedure Set_Font_Face_Size (Object : in out Font;
                                 Size : GL.Types.UInt;
                                 Resolution : GL.Types.UInt := 72);
   function Get_Font_Face_Size (Object : Font) return GL.Types.UInt;

   procedure Set_Font_Depth (Object : in out Font; Value : GL.Types.Single);

   procedure Set_Font_Outset (Object : in out Font;
                              Front, Back : GL.Types.Single);

   procedure Set_Display_List (Object : in out Font;
                               Use_Display_List : Boolean);

   function Ascender (Object : Font) return GL.Types.Single;
   function Descender (Object : Font) return GL.Types.Single;
   function Line_Height (Object : Font) return GL.Types.Single;
   function Bounds (Object : Font; Text : String) return Bounding_Box;
   function Advance_Width (Object : Font; Text : String) return GL.Types.Single;

   procedure Render (Object : Font;
                     Value  : String;
                     Mode : Render_Mode);

   function Error (Object : Font) return Errors.FT_Error;


   type Bitmap_Font is new Font with private;

   overriding procedure Load (Object : in out Bitmap_Font; Path : String);

   type Pixmap_Font is new Font with private;

   overriding procedure Load (Object : in out Pixmap_Font; Path : String);

   type Polygon_Font is new Font with private;

   overriding procedure Load (Object : in out Polygon_Font; Path : String);

   type Outline_Font is new Font with private;

   overriding procedure Load (Object : in out Outline_Font; Path : String);

   type Extrude_Font is new Font with private;

   overriding procedure Load (Object : in out Extrude_Font; Path : String);

   type Texture_Font is new Font with private;

   overriding procedure Load (Object : in out Texture_Font; Path : String);

   type Buffer_Font is new Font with private;

   overriding procedure Load (Object : in out Buffer_Font; Path : String);

private
   pragma Convention (C, Charset_List);
   for Charset_List'Component_Size use 32;

   type Font_Data is record
      Reference_Count : Positive;
      Pointer         : System.Address;
   end record;

   type Font_Data_Access is access all Font_Data;

   type Font is abstract new Ada.Finalization.Controlled with record
      Data : Font_Data_Access;
   end record;

   type Bitmap_Font is new Font with null record;
   type Pixmap_Font is new Font with null record;
   type Polygon_Font is new Font with null record;
   type Outline_Font is new Font with null record;
   type Extrude_Font is new Font with null record;
   type Texture_Font is new Font with null record;
   type Buffer_Font is new Font with null record;
end FTGL.Fonts;
