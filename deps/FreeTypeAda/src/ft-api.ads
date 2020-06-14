--  part of FreeTypeAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

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

   function FT_Init_FreeType (aLibrary : access FT.Library_Ptr)
                              return Errors.Error_Code;
   pragma Import (C, FT_Init_FreeType, "FT_Init_FreeType");

   function FT_Get_Char_Index (Face : Face_Ptr; Char_Code : ULong)
                               return Faces.Char_Index_Type;
   pragma Import (C, FT_Get_Char_Index, "FT_Get_Char_Index");

   function FT_Load_Glyph (Face : Face_Ptr; Glyph_Index : Faces.Char_Index_Type;
                           Flags : Faces.Load_Flag) return Errors.Error_Code;
   pragma Import (C, FT_Load_Glyph, "FT_Load_Glyph");

   function FT_Load_Char (aFace : Face_Ptr; Char_Code : ULong;
                          Load_Flags : Faces.Load_Flag)
                          return Errors.Error_Code;
   pragma Import (C, FT_Load_Char, "FT_Load_Char");

   function FT_New_Face (Library        : Library_Ptr;
                         File_Path_Name : Interfaces.C.Strings.chars_ptr;
                         Face_Index     : Faces.Face_Index_Type;
                         aFace          : access Face_Ptr)
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
