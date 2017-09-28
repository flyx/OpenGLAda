
with Interfaces.C.Strings;

with System.Address_To_Access_Conversions;

with Ada.Text_IO; use Ada.Text_IO;

with FT.API.FreeType; use FT.API.FreeType;

package body FT.FreeType is
   package Face_Access is new System.Address_To_Access_Conversions (Face_Record);
   package Size_Access is new System.Address_To_Access_Conversions (Size_Record);

   --  -------------------------------------------------------------------------

   function Advance_X (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Advance_X;
   end Advance_X;

   --  ------------------------------------------------------------------------

   function Bitmap_Height (aFace : Face_Ptr) return GL.Types.Int is
      theFace  : constant Face_Record := Face (aFace);
      Sizes    : FT_Bitmap_Size;
   begin
      if theFace.Available_Sizes = null then
         raise FreeType_Exception with
             "Bitmap_Height failed, there are no sizes available for this face.";
      end if;
      Sizes:= theFace.Available_Sizes.all;
      return GL.Types.Int (Sizes.Height);
   end Bitmap_Height;

   --  -------------------------------------------------------------------------

   function Bitmap_Width (aFace : Face_Ptr) return GL.Types.Int is
      theFace : constant Face_Record := Face (aFace);
      Sizes   : FT_Bitmap_Size;
   begin
      if theFace.Available_Sizes = null then
         raise FreeType_Exception with
             "Bitmap_Height failed, there are no sizes available for this face.";
      end if;
      Sizes:= theFace.Available_Sizes.all;
      return GL.Types.Int (Sizes.Width);
   end Bitmap_Width;

   --  -------------------------------------------------------------------------

   function Character_Texture (Data : Character_Record)
                               return GL.Objects.Textures.Texture is
   begin
      return Data.Texture;
   end Character_Texture;

   --  ------------------------------------------------------------------------

   procedure Done_Face (aFace : Face_Ptr) is
      use GL.Types;
   begin
      if FT_Done_Face (aFace) /= 0 then
         raise FreeType_Exception with "FT.FreeType.Done_Face failed";
      end if;
   end Done_Face;

   --  -------------------------------------------------------------------------

   procedure Done_Library (Library : Library_Ptr) is
      use GL.Types;
   begin
      if FT_Done_Library (Library) /= 0 then
         raise FreeType_Exception with "FT.FreeType.Done_Library failed";
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

   function Face_Size (aFace : Face_Ptr) return Size_Record is
      use Size_Access;
      theFace      : constant Face_Record := Face (aFace);
      Size_Pointer : constant Object_Pointer :=
                       To_Pointer (System.Address (theFace.Size));
   begin
      if Size_Pointer = null then
         raise FreeType_Exception with
             "FT.FreeType.Face_Size failed, theFace.Size is null.";
      end if;
      return Size_Pointer.all;
   exception
         when others =>
         raise FreeType_Exception with "FT.FreeType.Face_Size raised an exception.";
   end Face_Size;

   --  -------------------------------------------------------------------------

   function Face_Height (aFace : Face_Ptr) return GL.Types.Int is
   use GL.Types;
   begin
      return GL.Types.Int (Face_Size (aFace).Metrics.Ascender -
                               Face_Size (aFace).Metrics.Descender);
   exception
         when others =>
         raise FreeType_Exception with "FT.FreeType.Face_Height raised an exception.";
   end Face_Height;

   --  -------------------------------------------------------------------------

   function Face_Width (aFace : Face_Ptr) return GL.Types.Int is
   begin
      return GL.Types.Int (Face_Size (aFace).Metrics.X_Ppem);
   exception
         when others =>
         raise FreeType_Exception with "FT.FreeType.Face_Width raised an exception.";
   end Face_Width;

   --  -------------------------------------------------------------------------

   function Glyph_Slot (aFace : FT.API.Face_Ptr) return Glyph_Slot_Ptr is
   use System;
     theFace : constant Face_Record := Face (aFace);
   begin
      if System.Address (theFace.Glyph_Slot) = System.Null_Address then
         raise FreeType_Exception with
             "FT.FreeType.Glyph_Slot error: No Glyph is loaded.";
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

   function Left (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Left;
   end Left;

   --  ------------------------------------------------------------------------

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

   procedure Print_Character_Data (Char : Character;
                                   Data : Character_Record) is
      use GL.Types;
   begin
      Put_Line ("Character" & Char & " Data");
      Put_Line ("Width: " & GL.Types.Int'Image (Data.Width));
      Put_Line ("Rows: " & GL.Types.Int'Image (Data.Rows));
      Put_Line ("Left: " & GL.Types.Int'Image (Data.Left));
      Put_Line ("Top: " & GL.Types.Int'Image (Data.Top));
      Put_Line ("Advance X: " & GL.Types.Int'Image (Data.Advance_X) & " bits");
      New_Line;
   end Print_Character_Data;

   --  ------------------------------------------------------------------------

   function Rows (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Rows;
   end Rows;

   --  ------------------------------------------------------------------------

   procedure Set_Char_Data (Char_Data : in out Character_Record;
                            Width     : GL.Types.Int; Height : GL.Types.Int;
                            Left      : GL.Types.Int; Top    : GL.Types.Int;
                            Advance_X : GL.Types.Int) is
   begin
      Char_Data.Width := Width;
      Char_Data.Rows := Height;
      Char_Data.Left := Left;
      Char_Data.Top := Top;
      Char_Data.Advance_X := Advance_X;
   end Set_Char_Data;

   --  -------------------------------------------------------------------------

  function Set_Pixel_Sizes (aFace : Face_Ptr; Pixel_Width : GL.Types.UInt;
                            Pixel_Height : GL.Types.UInt) return FT_Error is
  begin
      return FT_Set_Pixel_Sizes (aFace, Pixel_Width, Pixel_Height);
  end;

   --  -------------------------------------------------------------------------

   procedure Set_Texture (Char_Data : in out Character_Record;
                          Texture   : GL.Objects.Textures.Texture) is
   begin
      Char_Data.Texture := Texture;
   end Set_Texture;

   --  -------------------------------------------------------------------------

   function Size_Metrics (aFace : Face_Ptr) return Size_Metrics_Record is
     Size : constant Size_Record := Face_Size (aFace);
   begin
      return Size.Metrics;
   end Size_Metrics;

   --  -------------------------------------------------------------------------

   function Top (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Top;
   end Top;

   --  ------------------------------------------------------------------------

   function Width (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Width;
   end Width;

   --  ------------------------------------------------------------------------

end FT.FreeType;
