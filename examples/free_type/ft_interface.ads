
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
   type FT_Glyph_Slot_Record is private;

   function Face (Face_Ptr : FT_Face) return FT_Face_Record;
   function Get_Kerning (aFace : FT_Face; Left_Glyph : FT_UInt; Right_Glyph : FT_UInt;
      Kern_Mode : FT_UInt; aKerning : access FT_Image.FT_Vector) return FT_Error;
   function Init_FreeType (alibrary : in out FT_Library) return FT_Error;
   function Load_Character (Face       : in out FT_Face; Char_Code : FT_ULong;
                            Load_Flags : FT_Types.Load_Flag) return FT_Error;
   function New_Face (Library       : FT_Library; File_Path_Name : String;
                      Face_Index    : FT_Long; aFace : in out FT_Face) return FT_Error;
   function Glyph (Face_Ptr : FT_Face) return FT_Glyph.FT_Glyph_Record;
   function Glyph (Face_Ptr : FT_Face) return FT_Glyph_Slot_Record;
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

    type FT_Glyph_Metrics is record
      width : aliased FT_Image.FT_Pos;
      height : aliased FT_Image.FT_Pos;
      horiBearingX : aliased FT_Image.FT_Pos;
      horiBearingY : aliased FT_Image.FT_Pos;
      horiAdvance : aliased FT_Image.FT_Pos;
      vertBearingX : aliased FT_Image.FT_Pos;
      vertBearingY : aliased FT_Image.FT_Pos;
      vertAdvance : aliased FT_Image.FT_Pos;
    end record;
   pragma Convention (C_Pass_By_Copy, FT_Glyph_Metrics);

   type FT_Glyph_Slot_Record is record
      Library           : FT_Library;
      Face              : FT_Face;
      Next              : FT_Glyph_Slot;
      Reserved          : aliased FT_Types.FT_UInt;
      C_Generic         : aliased FT_Types.FT_Generic;
      etrics            : aliased FT_Glyph_Metrics;
      LinearHoriAdvance : aliased FT_Types.FT_Fixed;
      LinearVertAdvance : aliased FT_Types.FT_Fixed;
      Advance           : aliased FT_Image.FT_Vector;
      Format            : aliased FT_Image.FT_Glyph_Format;
      Bitmap            : aliased FT_Image.FT_Bitmap;
      Bitmap_Left       : aliased FT_Types.FT_Int;
      Bitmap_Top        : aliased FT_Types.FT_Int;
      Outline           : aliased FT_Image.FT_Outline;
      Num_Subglyphs     : aliased FT_Types.FT_UInt;
      Subglyphs         : FT_Subglyph;
      Control_Data      : System.Address;
      Control_Len       : aliased Interfaces.C.long;
      Lsb_Delta         : aliased FT_Image.FT_Pos;
      Rsb_Delta         : aliased FT_Image.FT_Pos;
      Other             : System.Address;
      Internal          : FT_Slot_Internal;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Glyph_Slot_Record);

end FT_Interface;
