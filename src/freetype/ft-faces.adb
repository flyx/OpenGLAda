--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with System.Address_To_Access_Conversions;

with FT.API.Faces; use FT.API.Faces;

package body FT.Faces is
   use type Errors.Error_Code;

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
      if FT_Done_Face (aFace) /= Errors.Ok then
         raise FreeType_Exception with "FT_Done_Face failed";
      end if;
   end Done_Face;

   --  -------------------------------------------------------------------------

   procedure Done_Library (Library : Library_Ptr) is
      use GL.Types;
   begin
      if FT_Done_Library (Library) /= Errors.Ok then
         raise FreeType_Exception with "FT_Done_Library failed";
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
           "Face_Size failed, theFace.Size is null.";
      end if;
      return Size_Pointer.all;
   exception
         when others =>
            raise FreeType_Exception with "Face_Size raised an exception.";
   end Face_Size;

   --  -------------------------------------------------------------------------

   function Face_Height (aFace : Face_Ptr) return GL.Types.Int is
   use GL.Types;
   begin
      return GL.Types.Int (Face_Size (aFace).Metrics.Ascender -
                               Face_Size (aFace).Metrics.Descender);
   exception
         when others =>
            raise FreeType_Exception with "Face_Height raised an exception.";
   end Face_Height;

   --  -------------------------------------------------------------------------

   function Face_Width (aFace : Face_Ptr) return GL.Types.Int is
   begin
      return GL.Types.Int (Face_Size (aFace).Metrics.X_Ppem);
   exception
         when others =>
            raise FreeType_Exception with "Face_Width raised an exception.";
   end Face_Width;

   --  -------------------------------------------------------------------------

   function Glyph_Slot (aFace : FT.API.Face_Ptr) return Glyph_Slot_Ptr is
   use System;
     theFace : constant Face_Record := Face (aFace);
   begin
      if System.Address (theFace.Glyph_Slot) = System.Null_Address then
         raise FreeType_Exception with "No Glyph is loaded.";
      end if;
      return theFace.Glyph_Slot;
   end Glyph_Slot;

   --  -------------------------------------------------------------------------

   function Kerning (aFace : Face_Ptr; Left_Glyph : GL.Types.UInt;
                         Right_Glyph : GL.Types.UInt; Kern_Mode : GL.Types.UInt;
                     aKerning : access FT.Image.FT_Vector)
                     return Errors.Error_Code is
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
                            Flags : Load_Flag) return Errors.Error_Code is
   begin
      return FT_Load_Char (aFace, ULong (Char_Code), Flags'Enum_Rep);
   end Load_Character;

   --  -------------------------------------------------------------------------

   function New_Face (Library : Library_Ptr; File_Path_Name : String;
                      Face_Index : GL.Types.long; aFace : in out Face_Ptr)
                      return Errors.Error_Code is
      Path : constant Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (File_Path_Name);
   begin
      return  FT_New_Face (Library, Path, Face_Index, System.Address (aFace));
   end New_Face;

   --  -------------------------------------------------------------------------

   function Character_Data_To_String (Char : Character;
                                      Data : Character_Record) return String is
      use GL.Types;
   begin
      return "Character" & Char & " Data" & Character'Val (10) &
             "Width: " & GL.Types.Int'Image (Data.Width) & Character'Val (10) &
             "Rows: " & GL.Types.Int'Image (Data.Rows) & Character'Val (10) &
             "Left: " & GL.Types.Int'Image (Data.Left) & Character'Val (10) &
             "Top: " & GL.Types.Int'Image (Data.Top) & Character'Val (10) &
             "Advance X: " & GL.Types.Int'Image (Data.Advance_X) & " bits";
   end Character_Data_To_String;

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
                            Pixel_Height : GL.Types.UInt)
                            return Errors.Error_Code is
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

end FT.Faces;
