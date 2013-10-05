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

with System;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with FTGL.Errors;
with FTGL.Fonts;

private package FTGL.API is

   package C renames Interfaces.C;

   package Charset_Pointers is new C.Pointers (Positive, Charset,
                                               Fonts.Charset_List, None);

   -----------------------------------------------------------------------------
   -- Glyphs
   -----------------------------------------------------------------------------

   type Render_Callback is access procedure
     (Glyph, Data : System.Address; X, Y : GL.Types.Double; Mode : Render_Mode;
      New_X, New_Y : out GL.Types.Double);
   pragma Convention (C, Render_Callback);

   type Destroy_Callback is access procedure
     (Glyph, Data : System.Address);
   pragma Convention (C, Destroy_Callback);

   function Create_Custom_Glyph (Base, Data : System.Address;
                                 Render_CB  : Render_Callback;
                                 Destroy_CB : Destroy_Callback)
                                 return System.Address;
   pragma Import (Convention => C, Entity => Create_Custom_Glyph,
                  External_Name => "ftglCreateCustomGlyph");

   procedure Destroy_Glyph (Glyph : System.Address);
   pragma Import (Convention => C, Entity => Destroy_Glyph,
                  External_Name => "ftglDestroyGlyph");

   procedure Render_Glyph (Glyph        : System.Address;
                           Pen_X, Pen_Y : GL.Types.Double;
                           Mode         : Render_Mode;
                           New_X, New_Y : out GL.Types.Double);
   pragma Import (Convention => C, Entity => Render_Glyph,
                  External_Name => "ftglRenderGlyph");

   function Get_Glyph_Advance (Glyph : System.Address) return GL.Types.Single;
   pragma Import (Convention => C, Entity => Get_Glyph_Advance,
                 External_Name => "ftglGetGlyphAdvance");

   procedure Get_Glyph_BBox (Glyph : System.Address; Bounds : out Bounding_Box);
   pragma Import (Convention => C, Entity => Get_Glyph_BBox,
                  External_Name => "ftglGetGlyphBBox");

   function Get_Glyph_Error (Glyph : System.Address) return Errors.FT_Error;
   pragma Import (Convention => C, Entity => Get_Glyph_Error,
                  External_Name => "ftglGetGlyphError");

   -----------------------------------------------------------------------------
   -- Layouts
   -----------------------------------------------------------------------------

   procedure Destroy_Layout (Layout : System.Address);
   pragma Import (Convention => C, Entity => Destroy_Layout,
                  External_Name => "ftglDestroyLayout");

   procedure Get_Layout_BBox (Layout : System.Address;
                              Value  : C.char_array;
                              Bounds : Bounding_Box);
   pragma Import (Convention => C, Entity => Get_Layout_BBox,
                  External_Name => "ftglGetLayoutBBox");

   procedure Render_Layout (Layout : System.Address;
                            Value  : C.char_array;
                            Mode   : Render_Mode);
   pragma Import (Convention => C, Entity => Render_Layout,
                  External_Name => "ftglRenderLayout");

   function Get_Layout_Error (Layout : System.Address) return Errors.FT_Error;
   pragma Import (Convention => C, Entity => Get_Layout_Error,
                  External_Name => "ftglGetLayoutError");

   -----------------------------------------------------------------------------
   -- Fonts
   -----------------------------------------------------------------------------

   -- ftglCreateCustomFont omitted because we don't want to provide a complete
   -- FreeType wrapper, and without one, the function is pointless.

   function Create_Pixmap_Font (File : C.char_array) return System.Address;
   pragma Import (Convention => C, Entity => Create_Pixmap_Font,
                  External_Name => "ftglCreatePixmapFont");

   function Create_Buffer_Font (File : C.char_array) return System.Address;
   pragma Import (Convention => C, Entity => Create_Buffer_Font,
                  External_Name => "ftglCreateBufferFont");

   function Create_Bitmap_Font (File : C.char_array) return System.Address;
   pragma Import (Convention => C, Entity => Create_Bitmap_Font,
                  External_Name => "ftglCreateBitmapFont");

   function Create_Extrude_Font (File : C.char_array) return System.Address;
   pragma Import (Convention => C, Entity => Create_Extrude_Font,
                  External_Name => "ftglCreateExtrudeFont");

   function Create_Polygon_Font (File : C.char_array) return System.Address;
   pragma Import (Convention => C, Entity => Create_Polygon_Font,
                  External_Name => "ftglCreatePolygonFont");

   function Create_Outline_Font (File : C.char_array) return System.Address;
   pragma Import (Convention => C, Entity => Create_Outline_Font,
                  External_Name => "ftglCreateOutlineFont");

   function Create_Texture_Font (File : C.char_array) return System.Address;
   pragma Import (Convention => C, Entity => Create_Texture_Font,
                  External_Name => "ftglCreateTextureFont");

   procedure Destroy_Font (Font : System.Address);
   pragma Import (Convention => C, Entity => Destroy_Font,
                  External_Name => "ftglDestroyFont");

   function Attach_File (Font : System.Address; Path : C.char_array)
                         return C.int;
   pragma Import (Convention => C, Entity => Attach_File,
                  External_Name => "ftglAttachFile");

   function Attach_Data (Font : System.Address; Data : System.Address;
                         Size : C.size_t) return C.int;
   pragma Import (Convention => C, Entity => Attach_Data,
                  External_Name => "ftglAttachData");

   function Set_Font_Char_Map (Font : System.Address; Encoding : Charset)
                               return C.int;
   pragma Import (Convention => C, Entity => Set_Font_Char_Map,
                  External_Name => "ftglSetFontCharMap");

   function Get_Font_Char_Map_Count (Font : System.Address) return C.unsigned;
   pragma Import (Convention => C, Entity => Get_Font_Char_Map_Count,
                  External_Name => "ftglGetFontCharMapCount");

   function Get_Font_Char_Map_List (Font : System.Address)
                                    return Charset_Pointers.Pointer;
   pragma Import (Convention => C, Entity => Get_Font_Char_Map_List,
                  External_Name => "ftglGetFontCharMapList");

   function Set_Font_Face_Size (Font : System.Address;
                                Size, Res : GL.Types.UInt) return C.int;
   pragma Import (Convention => C, Entity => Set_Font_Face_Size,
                  External_Name => "ftglSetFontFaceSize");

   function Get_Font_Face_Size (Font : System.Address) return GL.Types.UInt;
   pragma Import (Convention => C, Entity => Get_Font_Face_Size,
                  External_Name => "ftglGetFontFaceSize");

   procedure Set_Font_Depth (Font : System.Address; Depth : Fonts.Depth);
   pragma Import (Convention => C, Entity => Set_Font_Depth,
                  External_Name => "ftglSetFontDepth");

   procedure Set_Font_Outset (Font : System.Address;
                              Front, Back : GL.Types.Single);
   pragma Import (Convention => C, Entity => Set_Font_Outset,
                  External_Name => "ftglSetFontOutset");

   procedure Set_Font_Display_List (Font : System.Address; Use_List : C.int);
   pragma Import (Convention => C, Entity => Set_Font_Display_List,
                  External_Name => "ftglSetFontDisplayList");

   function Get_Font_Ascender (Font : System.Address) return GL.Types.Single;
   pragma Import (Convention => C, Entity => Get_Font_Ascender,
                  External_Name => "ftglGetFontAscender");

   function Get_Font_Descender (Font : System.Address) return GL.Types.Single;
   pragma Import (Convention => C, Entity => Get_Font_Descender,
                  External_Name => "ftglGetFontDescender");

   function Get_Font_Line_Height (Font : System.Address) return GL.Types.Single;
   pragma Import (Convention => C, Entity => Get_Font_Line_Height,
                  External_Name => "ftglGetFontLineHeight");

   procedure Get_Font_BBox (Font  : System.Address;
                            Value : C.char_array; Len : C.int;
                            Bounds : out Bounding_Box);
   pragma Import (Convention => C, Entity => Get_Font_BBox,
                  External_Name => "ftglGetFontBBox");

   function Get_Font_Advance (Font : System.Address;
                              Value : C.char_array) return GL.Types.Single;
   pragma Import (Convention => C, Entity => Get_Font_Advance,
                  External_Name => "ftglGetFontAdvance");

   procedure Render_Font (Font : System.Address;
                          Value : C.char_array; Mode : Render_Mode);
   pragma Import (Convention => C, Entity => Render_Font,
                  External_Name => "ftglRenderFont");

   function Get_Font_Error (Font : System.Address) return Errors.FT_Error;
   pragma Import (Convention => C, Entity => Get_Font_Error,
                  External_Name => "ftglGetFontError");
end FTGL.API;
