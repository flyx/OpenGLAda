
with System;

with Interfaces.C;
with Interfaces.C.Strings;

with GL.Types;

with FT_Config;
--  with FT_Glyphs;
with FT_Image;
with FT_System;
with FT_Types; Use FT_Types;

package FT_Interface is

   type FT_Face is new System.Address;

   type FT_Bitmap_Size is record
      Height : GL.Types.Short;
      Width  : GL.Types.Short;
      Size   : FT_Image.FT_Pos;
      X_Ppem : FT_Image.FT_Pos;
      Y_Ppem : FT_Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Bitmap_Size);

   type FT_BBox is record
      X_Min : FT_Image.FT_Pos;
      Y_Min : FT_Image.FT_Pos;
      X_Max : FT_Image.FT_Pos;
      Y_Max : FT_Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_BBox);

   type FT_Face_Record is record
      Num_Faces               : GL.Types.Long;
      Face_Index              : GL.Types.Long;
      Face_Flags              : GL.Types.Long;
      Style_Flags             : GL.Types.Long;
      Num_Glyphs              : GL.Types.Long;
      Family_Name             : access FT_String;
      Style_Name              : access FT_String;
      Num_Fixed_sizes         : GL.Types.Int;
      Available_Sizes         : access FT_Bitmap_Size;
      Num_Charmaps            : GL.Types.Int;
      Character_Map_List      : System.Address;
      C_Generic               : FT_Generic;
      Bbox                    : FT_BBox;
      Units_Per_EM            : GL.Types.UShort;
      Ascender                : GL.Types.Short;
      Descender               : GL.Types.Short;
      Height                  : GL.Types.Short;
      Max_Advance_Width       : GL.Types.Short;
      Underline_Position      : GL.Types.Short;
      Underline_Thickness     :  GL.Types.Short;
      Glyph_Slot              : FT_Glyph_Slot;
      Size                    : FT_Size;
      Character_Map           : FT_Char_Map;
      Driver                  : FT_Driver;
      Memory                  : FT_System.FT_Memory;
      Stream                  : FT_System.FT_Stream;
      Sizes_List              : FT_List_Record;
      Autohint                : FT_Generic;
      Extensions              : System.Address;
      Internal                : FT_Face_Internal;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Face_Record);

   procedure Done_Face (Face_Ptr : FT_Face);
   procedure Done_Library (Library_Ptr : FT_Library);
   function Face (Face_Ptr : FT_Face) return FT_Face_Record;
   function Get_Glyph_Slot (Face_Ptr : FT_Face) return FT_Glyph_Slot;
   function Init_FreeType (alibrary : in out FT_Library) return FT_Error;
   function Load_Character (Face       : in out FT_Face; Char_Code : FT_ULong;
                            Load_Flags : FT_Types.Load_Flag) return FT_Error;
   function New_Face (Library    : FT_Library; File_Path_Name : String;
                      Face_Index : GL.Types.long; aFace : in out FT_Face)
                      return FT_Error;
   function Render_Glyph (Face_Ptr : FT_Face;
                          Render_Mode : FT_Render_Mode := Render_Mode_Mono)
                          return FT_Error;
   function Set_Pixel_Sizes (Face         : FT_Face; Pixel_Width : GL.Types.UInt;
                             Pixel_Height : GL.Types.UInt) return FT_Error;
end FT_Interface;
