
with System;

with Interfaces.C;
with Interfaces.C.Strings;

with FT_Config;
with FT_Glyphs;
with FT_Image;
with FT_System;
with FT_Types; Use FT_Types;

package FT_Interface is

   type FT_Face is new System.Address;

   type FT_Bitmap_Size is record
      Height : aliased FT_Short;
      Width  : aliased FT_Short;
      Size   : aliased FT_Image.FT_Pos;
      X_Ppem : aliased FT_Image.FT_Pos;
      Y_Ppem : aliased FT_Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Bitmap_Size);

   type FT_BBox is record
      xMin : aliased FT_Image.FT_Pos;
      yMin : aliased FT_Image.FT_Pos;
      xMax : aliased FT_Image.FT_Pos;
      yMax : aliased FT_Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_BBox);

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
      width        : aliased FT_Image.FT_Pos;
      height       : aliased FT_Image.FT_Pos;
      horiBearingX : aliased FT_Image.FT_Pos;
      horiBearingY : aliased FT_Image.FT_Pos;
      horiAdvance  : aliased FT_Image.FT_Pos;
      vertBearingX : aliased FT_Image.FT_Pos;
      vertBearingY : aliased FT_Image.FT_Pos;
      vertAdvance  : aliased FT_Image.FT_Pos;
    end record;
   pragma Convention (C_Pass_By_Copy, FT_Glyph_Metrics);

   type FT_Glyph_Slot_Record is record
      Library : FT_Library;
      Face              : FT_Face;
      Next              : FT_Glyph_Slot;
      Reserved          : aliased FT_Types.FT_UInt;
      C_Generic         : aliased FT_Types.FT_Generic;
      Metrics           : aliased FT_Glyph_Metrics;
      LinearHoriAdvance : aliased FT_Types.FT_Fixed;
      LinearVertAdvance : aliased FT_Types.FT_Fixed;
      Advance           : aliased FT_Image.FT_Vector;
      Format            : aliased FT_Image.FT_Glyph_Format;
      Bitmap            : aliased FT_Image.FT_Bitmap;
      Bitmap_left       : aliased FT_Types.FT_Int;
      Bitmap_top        : aliased FT_Types.FT_Int;
      Outline           : aliased FT_Image.FT_Outline;
      Num_subglyphs     : aliased FT_Types.FT_UInt;
      Subglyphs         : FT_SubGlyph;
      Control_data      : System.Address;
      Control_len       : aliased Interfaces.C.long;
      Lsb_Delta         : aliased FT_Image.FT_Pos;
      Rsb_Delta         : aliased FT_Image.FT_Pos;
      Other             : System.Address;
      Internal : FT_Slot_Internal;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Glyph_Slot_Record);

   procedure Done_Face (Face_Ptr : FT_Face);
   procedure Done_Library (Library_Ptr : FT_Library);
   function Face (Face_Ptr : FT_Face) return FT_Face_Record;
   function Get_Bitmap (Glyph_Slot : FT_Glyph_Slot) return FT_Image.FT_Bitmap;
   function Init_FreeType (alibrary : in out FT_Library) return FT_Error;
   function Load_Character (Face       : in out FT_Face; Char_Code : FT_ULong;
                            Load_Flags : FT_Types.Load_Flag) return FT_Error;
   function New_Face (Library       : FT_Library; File_Path_Name : String;
                      Face_Index    : FT_Long; aFace : in out FT_Face) return FT_Error;
   function Get_Glyph_Record (Face_Ptr : FT_Face) return FT_Glyphs.FT_Glyph_Record;
   function Get_Glyph_Slot (Face_Ptr : FT_Face) return FT_Glyph_Slot;
   function Set_Pixel_Sizes (Face         : in out FT_Face; Pixel_Width : FT_UInt;
                             Pixel_Height : FT_UInt) return FT_Error;

private

end FT_Interface;
