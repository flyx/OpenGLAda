
with System.Address_To_Access_Conversions;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with FT_Interface.API; use FT_Interface.API;
with FT_Glyphs;

package body FT_Interface is
   package Face_Access is new System.Address_To_Access_Conversions (FT_Face_Record);
   package Glyph_Access is new System.Address_To_Access_Conversions (FT_Glyphs.FT_Glyph_Record);
   package Slot_Access is new System.Address_To_Access_Conversions (FT_Interface.FT_Glyph_Slot_Record);

   --  -------------------------------------------------------------------------

   procedure Done_Face (Face_Ptr : FT_Face) is
      use Interfaces.C;
   begin
      if FT_Done_Face (Face_Ptr) /= 0 then
         Put_Line ("FT_Done_Face failed");
         raise FT_Types.FT_Exception;
      end if;
   end Done_Face;

   --  -------------------------------------------------------------------------

   procedure Done_Library (Library_Ptr : FT_Library) is
      use Interfaces.C;
   begin
      if FT_Done_Library (Library_Ptr) /= 0 then
         Put_Line ("FT_Done_Library failed");
         raise FT_Types.FT_Exception;
      end if;
   end Done_Library;

   --  -------------------------------------------------------------------------

   function Get_Bitmap (Glyph_Slot : FT_Glyph_Slot) return FT_Image.FT_Bitmap is
      use Interfaces.C;
      use Slot_Access;
      aGlyph_Ptr    : System.Address;
      Glyph_Pointer : Object_Pointer := To_Pointer (System.Address (Glyph_Slot));
      theGlyph      : FT_Interface.FT_Glyph_Slot_Record := Glyph_Pointer.all;
   begin
      --  Get_Glyph calls the FT_Get_Glyph C function.
      if FT_Glyphs.Get_Glyph (Glyph_Slot, aGlyph_Ptr) /= 0 then
         Put_Line ("FT_Interface.Bitmap raised an Exception");
         raise FT_Exception;
      end if;
      return theGlyph.Bitmap;
   end Get_Bitmap;

   --  -------------------------------------------------------------------------

   function Face (Face_Ptr : FT_Face) return FT_Face_Record is
      use Face_Access;
      --  type Object_Pointer is access all Object;
      Face_Pointer : Object_Pointer := To_Pointer (System.Address (Face_Ptr));
   begin
      return Face_Pointer.all;
   end Face;

   --  -------------------------------------------------------------------------

   function Get_Kerning (aFace : FT_Face; Left_Glyph : FT_UInt; Right_Glyph : FT_UInt;
                         Kern_Mode : FT_UInt; aKerning : access FT_Image.FT_Vector) return FT_Error is
   begin
      return  FT_Get_Kerning (aFace, Left_Glyph, Right_Glyph, Kern_Mode, aKerning);
   end Get_Kerning;

   --  -------------------------------------------------------------------------

   function Get_Glyph_Record (Face_Ptr : FT_Face) return FT_Glyphs.FT_Glyph_Record is
      use Glyph_Access;
      theFace : FT_Face_Record := Face (Face_Ptr);
      Glyph_Pointer : Object_Pointer :=
        To_Pointer (System.Address (theFace.Glyph));
   begin
      return Glyph_Pointer.all;
   end Get_Glyph_Record;

   --  -------------------------------------------------------------------------

   function Get_Glyph_Slot (Face_Ptr : FT_Face) return FT_Glyph_Slot is
      theFace : FT_Face_Record := Face (Face_Ptr);
   begin
      return theFace.Glyph;
   end Get_Glyph_Slot;

   --  -------------------------------------------------------------------------

   function Init_FreeType (aLibrary : in out FT_Library) return FT_Types.FT_Error is
   begin
      return FT_Init_FreeType (System.Address (aLibrary));
   end Init_FreeType;

   --  -------------------------------------------------------------------------

   function Load_Character (Face : in out FT_Face; Char_Code : FT_ULong;
                            Load_Flags : FT_Types.Load_Flag) return FT_Error is
   begin
      return FT_Load_Char (Face, Char_Code, Load_Flags'Enum_Rep);
   end Load_Character;

   --  -------------------------------------------------------------------------

   function New_Face (Library : FT_Library;
                      File_Path_Name : String;
                      Face_Index     : FT_Long; aFace : in out FT_Face) return FT_Error is
      Path : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (File_Path_Name);
   begin
      return  FT_New_Face (Library, Path, Face_Index, System.Address (aFace));
   end New_Face;

   --  -------------------------------------------------------------------------

   function Set_Pixel_Sizes (Face : in out FT_Face; Pixel_Width : FT_UInt;
                             Pixel_Height : FT_UInt) return FT_Error is
   begin
      return FT_Set_Pixel_Sizes (Face, Pixel_Width, Pixel_Height);
   end;

   --  -------------------------------------------------------------------------

end FT_Interface;
