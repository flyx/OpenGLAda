
with Interfaces.C.Strings;

with System.Address_To_Access_Conversions;

with Ada.Text_IO; use Ada.Text_IO;

with FT.API.Interfac; use FT.API.Interfac;

package body FT.Interfac is
   package Face_Access is new System.Address_To_Access_Conversions (Face_Record);

   --  -------------------------------------------------------------------------

   procedure Done_Face (aFace : FT.API.Face_Ptr) is
      use GL.Types;
   begin
      if FT_Done_Face (aFace) /= 0 then
         Put_Line ("FT_Done_Face failed");
         raise FT.FT_Exception;
      end if;
   end Done_Face;

   --  -------------------------------------------------------------------------

   procedure Done_Library (Library : FT.API.Library_Ptr) is
      use GL.Types;
   begin
      if FT_Done_Library (Library) /= 0 then
         Put_Line ("FT_Done_Library failed");
         raise FT.FT_Exception;
      end if;
   end Done_Library;

   --  -------------------------------------------------------------------------

   function Face (aFace : FT.API.Face_Ptr) return Face_Record is
      use Face_Access;
      --  type Object_Pointer is access all Object;
      Face_Pointer : constant Object_Pointer := To_Pointer (System.Address (aFace));
   begin
      return Face_Pointer.all;
   end Face;

   --  -------------------------------------------------------------------------

   function Kerning (aFace : FT.API.Face_Ptr; Left_Glyph : GL.Types.UInt;
                         Right_Glyph : GL.Types.UInt; Kern_Mode : GL.Types.UInt;
                         aKerning : access FT.Image.FT_Vector) return FT_Error is
   begin
      return  FT_Get_Kerning (aFace, Left_Glyph, Right_Glyph, Kern_Mode, aKerning);
   end Kerning;

   --  -------------------------------------------------------------------------

    function Glyph_Slot (aFace : FT.API.Face_Ptr) return FT.API.Glyph_Slot_Ptr is
      theFace : constant Face_Record := Face (aFace);
   begin
      return theFace.Glyph_Slot;
   end Glyph_Slot;

   --  -------------------------------------------------------------------------

   function Init_FreeType (aLibrary : in out FT.API.Library_Ptr) return FT.FT_Error is
   begin
      return FT_Init_FreeType (System.Address (aLibrary));
   end Init_FreeType;

   --  -------------------------------------------------------------------------

   function Load_Character (aFace : FT.API.Face_Ptr; Char_Code : FT_ULong;
                            Flags : Load_Flag) return FT_Error is
   begin
      return FT_Load_Char (aFace, Char_Code, Flags'Enum_Rep);
   end Load_Character;

   --  -------------------------------------------------------------------------

   function New_Face (Library : FT.API.Library_Ptr;
                      File_Path_Name : String;
                      Face_Index     : GL.Types.long;
                      aFace : in out FT.API.Face_Ptr)
                      return FT_Error is
      Path : constant Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (File_Path_Name);
   begin
      return  FT_New_Face (Library, Path, Face_Index, System.Address (aFace));
   end New_Face;

   --  -------------------------------------------------------------------------

   function Render_Glyph (aFace : FT.API.Face_Ptr;
                          Mode : FT.API.Render_Mode)
                          return FT_Error is
      Slot : constant FT.API.Glyph_Slot_Ptr := Glyph_Slot (aFace);
   begin
      return  FT_Render_Glyph (Slot, Mode);
   end Render_Glyph;

   --  -------------------------------------------------------------------------

  function Set_Pixel_Sizes (aFace : FT.API.Face_Ptr; Pixel_Width : GL.Types.UInt;
                            Pixel_Height : GL.Types.UInt) return FT_Error is
  begin
      return FT_Set_Pixel_Sizes (aFace, Pixel_Width, Pixel_Height);
  end;

   --  -------------------------------------------------------------------------

end FT.Interfac;
