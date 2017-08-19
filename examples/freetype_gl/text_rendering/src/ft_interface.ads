
with System;

with Interfaces.C.Strings;

with FT_Config;
with FT_Glyph;
with FT_Image;
with FT_System;
with FT_Types; Use FT_Types;

package FT_Interface is

   type FT_Face is new System.Address;
   type FT_Face_Record is private;
   type FT_Library is new System.Address;

   function Face (Face_Ptr : FT_Face) return FT_Face_Record;
   function Init_FreeType (alibrary : in out FT_Library) return FT_Error;
   function Load_Character (Face       : in out FT_Face; Char_Code : FT_ULong;
                            Load_Flags : FT_Types.Load_Flag) return FT_Error;
   function New_Face (Library       : FT_Library; File_Path_Name : String;
                      Face_Index    : FT_Long; aFace : in out FT_Face) return FT_Error;
   function Set_Pixel_Sizes (Face         : in out FT_Face; Pixel_Width : FT_UInt;
                             Pixel_Height : FT_UInt) return FT_Error;

private
   use FT_Image;

   type FT_BBox is record
      xMin : aliased FT_Pos;
      yMin : aliased FT_Pos;
      xMax : aliased FT_Pos;
      yMax : aliased FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_BBox);

   type FT_Bitmap_Size is record
      height : aliased FT_Short;
      width  : aliased FT_Short;
      size   : aliased FT_Pos;
      x_ppem : aliased FT_Pos;
      y_ppem : aliased FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Bitmap_Size);

   type FT_Face_Record is record
      Num_Faces               : aliased FT_Long;
      Face_Index              : aliased FT_Long;
      Face_Flags              : aliased FT_Long;
      Style_Flags             : aliased FT_Long;
      Num_Glyphs              : aliased FT_Long;
      Family_Name             : access FT_String;
      Style_Name              : access FT_String;
      Num_Fixed_sizes         : aliased FT_Int;
      Available_Sizes         : access FT_Bitmap_Size;
      Num_Charmaps            : aliased FT_Int;
      Character_Map_List      : System.Address;
      C_Generic               : aliased FT_Generic;
      Bbox                    : aliased FT_BBox;
      Units_Per_EM            : aliased FT_UShort;
      Ascender                : aliased FT_Short;
      Descender               : aliased FT_Short;
      Height                  : aliased FT_Short;
      Max_Advance_Width       : aliased FT_Short;
      Max_Advance_Height      : aliased FT_Short;
      Underline_Position      : aliased FT_Short;
      Underline_Thickness     : aliased FT_Short;
      Glyph                   : FT_Glyph_Slot;
      Size                    : FT_Size;
      Character_Map           : FT_Char_Map;
      Driver                  : FT_Driver;
      Memory                  : FT_System.FT_Memory;
      Stream                  : FT_System.FT_Stream;
      Sizes_List              : aliased FT_List_Record;
      Autohint                : aliased FT_Generic;
      Extensions              : System.Address;
      Internal                : FT_Face_Internal;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Face_Record);

end FT_Interface;
