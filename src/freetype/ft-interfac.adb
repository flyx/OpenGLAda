
with Interfaces.C.Strings;

with System.Address_To_Access_Conversions;

with Ada.Text_IO; use Ada.Text_IO;

with FT.Interfac.API; use FT.Interfac.API;

package body FT.Interfac is
   package Face_Access is new System.Address_To_Access_Conversions (FT_Face_Record);

   --  -------------------------------------------------------------------------

   procedure Done_Face (Face_Ptr : FT_Face) is
      use GL.Types;
   begin
      if FT_Done_Face (Face_Ptr) /= 0 then
         Put_Line ("FT_Done_Face failed");
         raise FT.Types.FT_Exception;
      end if;
   end Done_Face;

   --  -------------------------------------------------------------------------

   procedure Done_Library (Library : Library_Ptr) is
      use GL.Types;
   begin
      if FT_Done_Library (Library) /= 0 then
         Put_Line ("FT_Done_Library failed");
         raise FT.Types.FT_Exception;
      end if;
   end Done_Library;

   --  -------------------------------------------------------------------------

   function Face (Face_Ptr : FT_Face) return FT_Face_Record is
      use Face_Access;
      --  type Object_Pointer is access all Object;
      Face_Pointer : constant Object_Pointer := To_Pointer (System.Address (Face_Ptr));
   begin
      return Face_Pointer.all;
   end Face;

   --  -------------------------------------------------------------------------

   function Kerning (aFace : FT_Face; Left_Glyph : GL.Types.UInt;
                         Right_Glyph : GL.Types.UInt; Kern_Mode : GL.Types.UInt;
                         aKerning : access FT.Image.FT_Vector) return FT_Error is
   begin
      return  FT_Get_Kerning (aFace, Left_Glyph, Right_Glyph, Kern_Mode, aKerning);
   end Kerning;

   --  -------------------------------------------------------------------------

   function Face_Record (Face_Ptr : FT_Face) return FT_Face_Record is
   begin
      return Face (Face_Ptr);
   end Face_Record;

   --  -------------------------------------------------------------------------

   function Glyph_Slot (Face_Ptr : FT_Face) return Glyph_Slot_Ptr is
      theFace : constant FT_Face_Record := Face (Face_Ptr);
   begin
      return theFace.Glyph_Slot;
   end Glyph_Slot;

   --  -------------------------------------------------------------------------

   function Init_FreeType (aLibrary : in out Library_Ptr) return FT.Types.FT_Error is
   begin
      return FT_Init_FreeType (System.Address (aLibrary));
   end Init_FreeType;

   --  -------------------------------------------------------------------------

   function Load_Character (Face : FT_Face; Char_Code : FT_ULong;
                            Load_Flags : FT.Types.Load_Flag) return FT_Error is
   begin
      return FT_Load_Char (Face, Char_Code, Load_Flags'Enum_Rep);
   end Load_Character;

   --  -------------------------------------------------------------------------

   function New_Face (Library : Library_Ptr;
                      File_Path_Name : String;
                      Face_Index     : GL.Types.long; aFace : in out FT_Face)
                      return FT_Error is
      Path : constant Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (File_Path_Name);
   begin
      return  FT_New_Face (Library, Path, Face_Index, System.Address (aFace));
   end New_Face;

   --  -------------------------------------------------------------------------

   function Render_Glyph (Face_Ptr : FT_Face;
                          Mode : Render_Mode := Render_Mode_Mono)
                          return FT_Error is
      Slot : constant Glyph_Slot_Ptr := Glyph_Slot (Face_Ptr);
   begin
      return  FT_Render_Glyph (Slot, Mode);
   end Render_Glyph;

   --  -------------------------------------------------------------------------

  function Set_Pixel_Sizes (Face : FT_Face; Pixel_Width : GL.Types.UInt;
                            Pixel_Height : GL.Types.UInt) return FT_Error is
  begin
      return FT_Set_Pixel_Sizes (Face, Pixel_Width, Pixel_Height);
  end;

   --  -------------------------------------------------------------------------

end FT.Interfac;
