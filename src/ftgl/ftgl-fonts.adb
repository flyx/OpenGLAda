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

with Ada.Unchecked_Deallocation;

with FTGL.API;

package body FTGL.Fonts is

   overriding procedure Initialize (Object : in out Font) is
   begin
      Object.Data := null;
   end Initialize;

   overriding procedure Adjust (Object : in out Font) is
   begin
      if Object.Data /= null then
         Object.Data.Reference_Count := Object.Data.Reference_Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Font) is
      procedure Free is new Ada.Unchecked_Deallocation (Font_Data,
                                                        Font_Data_Access);
   begin
      if Object.Data /= null then
         if Object.Data.Reference_Count = 1 then
            FTGL.API.Destroy_Font (Object.Data.Pointer);
            Free (Object.Data);
         else
            Object.Data.Reference_Count := Object.Data.Reference_Count - 1;
         end if;
      end if;
   end Finalize;

   generic
      type Font_Type is new Font with private;
      with function Creator (Path : API.C.char_array) return System.Address;
   procedure Loader (Object : in out Font_Type; Path : String);

   procedure Loader (Object : in out Font_Type; Path : String) is
      use type System.Address;
      C_Path : constant API.C.char_array := API.C.To_C (Path);
      Raw    : constant System.Address := Creator (C_Path);
   begin
      if Raw = System.Null_Address then
         raise FTGL_Error with "Could not load font " & Path;
      end if;

      if Object.Data /= null then
         FTGL.API.Destroy_Font (Object.Data.Pointer);
         Object.Data.Pointer := Raw;
      else
         Object.Data := new Font_Data'(Reference_Count => 1, Pointer => Raw);
      end if;
   end Loader;

   procedure Bitmap_Loader is new Loader (Bitmap_Font, API.Create_Bitmap_Font);
   procedure Load (Object : in out Bitmap_Font; Path : String)
                   renames Bitmap_Loader;

   procedure Pixmap_Loader is new Loader (Pixmap_Font, API.Create_Pixmap_Font);
   procedure Load (Object : in out Pixmap_Font; Path : String)
                   renames Pixmap_Loader;

   procedure Polygon_Loader is
     new Loader (Polygon_Font, API.Create_Polygon_Font);
   procedure Load (Object : in out Polygon_Font; Path : String)
                   renames Polygon_Loader;

   procedure Outline_Loader is
     new Loader (Outline_Font, API.Create_Outline_Font);
   procedure Load (Object : in out Outline_Font; Path : String)
                   renames Outline_Loader;

   procedure Extrude_Loader is
     new Loader (Extrude_Font, API.Create_Extrude_Font);
   procedure Load (Object : in out Extrude_Font; Path : String)
                   renames Extrude_Loader;

   procedure Texture_Loader is
     new Loader (Texture_Font, API.Create_Texture_Font);
   procedure Load (Object : in out Texture_Font; Path : String)
                   renames Texture_Loader;

   procedure Buffer_Loader is new Loader (Buffer_Font, API.Create_Buffer_Font);
   procedure Load (Object : in out Buffer_Font; Path : String)
                   renames Buffer_Loader;

   function Loaded (Object : Font) return Boolean is
   begin
     return Object.Data /= null;
   end Loaded;

   procedure Set_Char_Map (Object : in out Font; Value : Charset) is
      use type API.C.int;
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      if API.Set_Font_Char_Map (Object.Data.Pointer, Value) /= 1 then
         raise FTGL_Error with "Cannot set char map!";
      end if;
   end Set_Char_Map;

   function Get_Char_Map_List (Object : Font) return Charset_List is
      Size : API.C.unsigned;
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      Size := API.Get_Font_Char_Map_Count (Object.Data.Pointer);
      return API.Charset_Pointers.Value
        (API.Get_Font_Char_Map_List (Object.Data.Pointer),
         Interfaces.C.ptrdiff_t (Size));
   end Get_Char_Map_List;

   procedure Set_Font_Face_Size (Object     : in out Font;
                                 Size       : GL.Types.UInt;
                                 Resolution : GL.Types.UInt := 72) is
      use type API.C.int;
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      if API.Set_Font_Face_Size (Object.Data.Pointer, Size, Resolution) /= 1
      then
         raise FTGL_Error with "Cannot set font size";
      end if;
   end Set_Font_Face_Size;

   function Get_Font_Face_Size (Object : Font) return GL.Types.UInt is
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      return API.Get_Font_Face_Size (Object.Data.Pointer);
   end Get_Font_Face_Size;

   procedure Set_Font_Depth (Object : in out Font; Value : GL.Types.Single) is
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      API.Set_Font_Depth (Object.Data.Pointer, Value);
   end Set_Font_Depth;

   procedure Set_Font_Outset (Object : in out Font;
                              Front, Back : GL.Types.Single) is
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      API.Set_Font_Outset (Object.Data.Pointer, Front, Back);
   end Set_Font_Outset;

   procedure Set_Display_List (Object : in out Font;
                               Use_Display_List : Boolean) is
      Value : API.C.int;
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      if Use_Display_List then
         Value := 1;
      else
         Value := 0;
      end if;
      API.Set_Font_Display_List (Object.Data.Pointer, Value);
   end Set_Display_List;

   function Ascender (Object : Font) return GL.Types.Single is
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      return API.Get_Font_Ascender (Object.Data.Pointer);
   end Ascender;

   function Descender (Object : Font) return GL.Types.Single is
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      return API.Get_Font_Descender (Object.Data.Pointer);
   end Descender;

   function Line_Height (Object : Font) return GL.Types.Single is
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      return API.Get_Font_Line_Height (Object.Data.Pointer);
   end Line_Height;

   function Bounds (Object : Font; Text : String) return Bounding_Box is
      Result : Bounding_Box;
      C_Text : constant API.C.char_array := API.C.To_C (Text, False);
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      API.Get_Font_BBox (Object.Data.Pointer, C_Text, C_Text'Length, Result);
      return Result;
   end Bounds;

   function Advance_Width (Object : Font; Text : String) return GL.Types.Single
   is
      C_Text : constant API.C.char_array := API.C.To_C (Text);
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;
      return API.Get_Font_Advance (Object.Data.Pointer, C_Text);
   end Advance_Width;

   procedure Render (Object : Font; Value : String; Mode : Render_Mode) is
      C_Value : constant API.C.char_array := API.C.To_C (Value);
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;

      API.Render_Font (Object.Data.Pointer, C_Value, Mode);
   end Render;

   function Error (Object : Font) return Errors.FT_Error is
   begin
      if Object.Data = null then
         raise FTGL_Error with "Font uninitialized!";
      end if;

      return API.Get_Font_Error (Object.Data.Pointer);
   end Error;
end FTGL.Fonts;
