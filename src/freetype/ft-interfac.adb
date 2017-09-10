
with Interfaces.C.Strings;

with System.Address_To_Access_Conversions;

with Ada.Text_IO; use Ada.Text_IO;

with FT.API.Interfac; use FT.API.Interfac;

package body FT.Interfac is
   package Face_Access is new System.Address_To_Access_Conversions (Face_Record);

   --  -------------------------------------------------------------------------

   procedure Done_Face (aFace : Face_Ptr) is
      use GL.Types;
   begin
      if FT_Done_Face (aFace) /= 0 then
         Put_Line ("FT_Done_Face failed");
         raise FT.FT_Exception;
      end if;
   end Done_Face;

   --  -------------------------------------------------------------------------

   procedure Done_Library (Library : Library_Ptr) is
      use GL.Types;
   begin
      if FT_Done_Library (Library) /= 0 then
         Put_Line ("FT_Done_Library failed");
         raise FT.FT_Exception;
      end if;
   end Done_Library;

   --  -------------------------------------------------------------------------

   function Face (aFace : Face_Ptr) return Face_Record is
      use Face_Access;
      --  type Object_Pointer is access all Object;
      Face_Pointer : constant Object_Pointer := To_Pointer (System.Address (aFace));
   begin
      return Face_Pointer.all;
   end Face;

   --  -------------------------------------------------------------------------

   function Glyph_Slot (aFace : FT.API.Face_Ptr) return Glyph_Slot_Ptr is
   use System;
     theFace : constant Face_Record := Face (aFace);
   begin
      if System.Address (theFace.Glyph_Slot) = System.Null_Address then
         Put_Line ("No Glyph is loaded.");
         raise FT.FT_Exception;
      end if;
      return theFace.Glyph_Slot;
   end Glyph_Slot;

   --  -------------------------------------------------------------------------

   function Init_FreeType (aLibrary : in out Library_Ptr) return FT.FT_Error is
   begin
      return FT_Init_FreeType (System.Address (aLibrary));
   end Init_FreeType;

   --  -------------------------------------------------------------------------

   function Kerning (aFace : Face_Ptr; Left_Glyph : GL.Types.UInt;
                         Right_Glyph : GL.Types.UInt; Kern_Mode : GL.Types.UInt;
                         aKerning : access FT.Image.FT_Vector) return FT_Error is
   begin
      return  FT_Get_Kerning (aFace, Left_Glyph, Right_Glyph, Kern_Mode, aKerning);
   end Kerning;

   --  -------------------------------------------------------------------------

   function Load_Character (aFace : Face_Ptr; Char_Code : GL.Types.Long;
                            Flags : Load_Flag) return FT_Error is
   begin
      return FT_Load_Char (aFace, FT_ULong (Char_Code), Flags'Enum_Rep);
   end Load_Character;

   --  -------------------------------------------------------------------------

   function New_Face (Library : Library_Ptr; File_Path_Name : String;
                      Face_Index : GL.Types.long; aFace : in out Face_Ptr)
                      return FT_Error is
      Path : constant Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (File_Path_Name);
   begin
      return  FT_New_Face (Library, Path, Face_Index, System.Address (aFace));
   end New_Face;

   --  -------------------------------------------------------------------------

  function Set_Pixel_Sizes (aFace : Face_Ptr; Pixel_Width : GL.Types.UInt;
                            Pixel_Height : GL.Types.UInt) return FT_Error is
  begin
      return FT_Set_Pixel_Sizes (aFace, Pixel_Width, Pixel_Height);
  end;

   --  -------------------------------------------------------------------------

end FT.Interfac;
