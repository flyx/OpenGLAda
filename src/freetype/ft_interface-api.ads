
with System;

with Interfaces.C.Strings;

package FT_Interface.API is

   function FT_Done_Face (face : FT_Face) return FT_Error;
   pragma Import (C, FT_Done_Face, "FT_Done_Face");

   function FT_Done_Library (Library : FT_Library) return FT_Error;
   pragma Import (C, FT_Done_Library, "FT_Done_Library");

   function FT_Get_Kerning
     (aFace : FT_Face; Left_Glyph : GL.Types.UInt; Right_Glyph :  GL.Types.UInt;
      Kern_Mode : GL.Types.UInt; aKerning : access FT_Image.FT_Vector) return FT_Error;
   pragma Import (C, FT_Get_Kerning, "FT_Get_Kerning");

  function FT_Init_FreeType (aLibrary : System.Address) return FT_Error;
   pragma Import (C, FT_Init_FreeType, "FT_Init_FreeType");

   function FT_Load_Char (Face : FT_Face; Char_Code : FT_ULong;
      Load_Flags : GL.Types.Int) return FT_Error;
   pragma Import (C, FT_Load_Char, "FT_Load_Char");

   function FT_New_Face (Library : FT_Library;
               File_Path_Name : Interfaces.C.Strings.chars_ptr;
               Face_Index     : GL.Types.long; aFace : System.Address)
               return FT_Error;
   pragma Import (C, FT_New_Face, "FT_New_Face");

   function FT_Render_Glyph (Slot : FT_Glyph_Slot_Ptr; Render_Mode : FT_Render_Mode)
                             return FT_Error;
   pragma Import (C, FT_Render_Glyph, "FT_Render_Glyph");

   function FT_Set_Pixel_Sizes (Face : FT_Face; Pixel_Width : GL.Types.UInt;
                                Pixel_Height : GL.Types.UInt) return FT_Error;
   pragma Import (C, FT_Set_Pixel_Sizes, "FT_Set_Pixel_Sizes");

end FT_Interface.API;
