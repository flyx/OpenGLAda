
with System;

with Interfaces.C.Strings;

package FT.Interfac.API is

   function FT_Done_Face (aFace : Face_Ptr) return FT_Error;
   pragma Import (C, FT_Done_Face, "FT_Done_Face");

   function FT_Done_Library (Library : Library_Ptr) return FT_Error;
   pragma Import (C, FT_Done_Library, "FT_Done_Library");

   function FT_Get_Kerning
     (aFace : Face_Ptr; Left_Glyph : GL.Types.UInt; Right_Glyph :  GL.Types.UInt;
      Kern_Mode : GL.Types.UInt; aKerning : access FT.Image.FT_Vector) return FT_Error;
   pragma Import (C, FT_Get_Kerning, "FT_Get_Kerning");

  function FT_Init_FreeType (aLibrary : in out System.Address) return FT_Error;
   pragma Import (C, FT_Init_FreeType, "FT_Init_FreeType");

   function FT_Load_Char (aFace : Face_Ptr; Char_Code : FT_ULong;
      Load_Flags : GL.Types.Int) return FT_Error;
   pragma Import (C, FT_Load_Char, "FT_Load_Char");

   function FT_New_Face (Library : Library_Ptr;
               File_Path_Name : Interfaces.C.Strings.chars_ptr;
               Face_Index     : GL.Types.long; aFace : in out System.Address)
               return FT_Error;
   pragma Import (C, FT_New_Face, "FT_New_Face");

   function FT_Render_Glyph (Slot : Glyph_Slot_Ptr; Mode : Render_Mode)
                             return FT_Error;
   pragma Import (C, FT_Render_Glyph, "FT_Render_Glyph");

   function FT_Set_Pixel_Sizes (aFace : Face_Ptr; Pixel_Width : GL.Types.UInt;
                                Pixel_Height : GL.Types.UInt) return FT_Error;
   pragma Import (C, FT_Set_Pixel_Sizes, "FT_Set_Pixel_Sizes");

end FT.Interfac.API;
