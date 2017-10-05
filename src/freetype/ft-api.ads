--------------------------------------------------------------------------------
-- Copyright (c) 2017, Felix Krause <contact@flyx.org>
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

with Interfaces.C.Strings;

with FT.Faces;

with FT.Errors;

private package FT.API is
   pragma Preelaborate;

   type Bool is new Boolean;

   function FT_Reference_Library (Ptr : Library_Ptr) return Errors.Error_Code;
   pragma Import (C, FT_Reference_Library, "FT_Reference_Library");

   function FT_Done_Library (Library : Library_Ptr) return Errors.Error_Code;
   pragma Import (C, FT_Done_Library, "FT_Done_Library");

   function FT_Get_Kerning
     (aFace : Face_Ptr; Left_Glyph : UInt; Right_Glyph :  UInt;
      Kern_Mode : UInt; aKerning : access Vector)
      return Errors.Error_Code;
   pragma Import (C, FT_Get_Kerning, "FT_Get_Kerning");

   function FT_Init_FreeType (aLibrary : in out FT.Library_Ptr)
                              return Errors.Error_Code;
   pragma Import (C, FT_Init_FreeType, "FT_Init_FreeType");

   function FT_Load_Char (aFace : Face_Ptr; Char_Code : ULong;
                          Load_Flags : Faces.Load_Flag)
                          return Errors.Error_Code;
   pragma Import (C, FT_Load_Char, "FT_Load_Char");

   function FT_New_Face (Library        : Library_Ptr;
                         File_Path_Name : Interfaces.C.Strings.chars_ptr;
                         Face_Index     : Faces.Face_Index_Type;
                         aFace          : in out Face_Ptr)
                         return Errors.Error_Code;
   pragma Import (C, FT_New_Face, "FT_New_Face");

   function FT_Reference_Face (Face : Face_Ptr)
                               return Errors.Error_Code;
   pragma Import (C, FT_Reference_Face, "FT_Reference_Face");

   function FT_Done_Face (aFace : Face_Ptr) return Errors.Error_Code;
   pragma Import (C, FT_Done_Face, "FT_Done_Face");

   function FT_Set_Pixel_Sizes (aFace : Face_Ptr;
                                Pixel_Width : UInt;
                                Pixel_Height : UInt)
                                return Errors.Error_Code;
   pragma Import (C, FT_Set_Pixel_Sizes, "FT_Set_Pixel_Sizes");
private
   for Bool use (False => 0, True => 1);
   for Bool'Size use Interfaces.C.unsigned_char'Size;
end FT.API;
