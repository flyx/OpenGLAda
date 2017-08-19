
with System.Address_To_Access_Conversions;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with FT_Interface.API; use FT_Interface.API;

package body FT_Interface is

   function Face (Face_Ptr : FT_Face) return FT_Face_Record is
      use System.Address_To_Access_Conversions;
      Face_Pointer : Object_Pointer := To_Pointer (Face_Ptr);
   begin
      return Face_Ptr.all;
   end Face;

  function Init_FreeType (aLibrary : in out FT_Library) return FT_Types.FT_Error is
  begin
      return FT_Init_FreeType (System.Address (aLibrary));
  end Init_FreeType;

   function Load_Character (Face : in out FT_Face; Char_Code : FT_ULong;
      Load_Flags : FT_Types.Load_Flag) return FT_Error is
   begin
      return FT_Load_Char (Face, Char_Code, Load_Flags'Enum_Rep);
   end Load_Character;

  function New_Face (Library : FT_Library;
               File_Path_Name : String;
               Face_Index     : FT_Long; aFace : in out FT_Face) return FT_Error is
      Path : Interfaces.C.Strings.chars_ptr :=
               Interfaces.C.Strings.New_String (File_Path_Name);
  begin
      return  FT_New_Face (Library, Path, Face_Index, System.Address (aFace));
  end New_Face;

   function Set_Pixel_Sizes (Face : in out FT_Face; Pixel_Width : FT_UInt;
                             Pixel_Height : FT_UInt) return FT_Error is
   begin
       return FT_Set_Pixel_Sizes (Face, Pixel_Width, Pixel_Height);
   end;

end FT_Interface;
