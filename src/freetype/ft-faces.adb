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

with Errors;
with FT.API; use FT.API;

package body FT.Faces is

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
           "FT.Faces.Bitmap_Height failed, there are no sizes available for this face.";
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
           "FT.Faces.Bitmap_Height failed, there are no sizes available for this face.";
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

   procedure Check_Face_Ptr (Face_Ptr : FT.Faces.Face_Ptr) is
   begin
      if Face_Ptr = Null then
         raise FreeType_Exception with
           "FT.Faces.Check_Face_Ptr - No face is loaded, Face_Ptr is null.";
      end if;
   end Check_Face_Ptr;

   --  -------------------------------------------------------------------------

   procedure Check_Glyph_Slot_Ptr (thePtr : Glyph_Slot_Ptr) is
   begin
      if thePtr = Null then
         raise FreeType_Exception with
           "FT.Faces.Check_Glyph_Slot_Ptr - No glyph is loaded, Glyph_Slot_Ptr is null.";
      end if;
   end Check_Glyph_Slot_Ptr;

   --  -------------------------------------------------------------------------

   procedure Done_Face (aFace : Face_Ptr) is
      use GL.Types;
      use Errors;
   begin
      if FT_Done_Face (aFace) /= Errors.Ok then
         raise FreeType_Exception with "FT.Faces.FT_Done_Face failed";
      end if;
   end Done_Face;

   --  -------------------------------------------------------------------------

   function Face (aFace : Face_Ptr) return Face_Record is
   begin
      return aFace.all;
   end Face;

   --  -------------------------------------------------------------------------

   function Face_Size (aFace : Face_Ptr) return Size_Record is
      theFace      : constant Face_Record := Face (aFace);
   begin
      if theFace.Size = null then
         raise FreeType_Exception with
           "FT.Faces.Face_Size failed, theFace.Size is null.";
      end if;
      return theFace.Size.all;
   exception
         when others =>
            raise FreeType_Exception with "FT.Faces.Face_Size raised an exception.";
   end Face_Size;

   --  -------------------------------------------------------------------------

   function Face_Height (aFace : Face_Ptr) return GL.Types.Int is
   use GL.Types;
   begin
      return GL.Types.Int (Face_Size (aFace).Metrics.Ascender -
                               Face_Size (aFace).Metrics.Descender);
   exception
         when others =>
            raise FreeType_Exception with "FT.Faces.Face_Height raised an exception.";
   end Face_Height;

   --  -------------------------------------------------------------------------

   function Face_Width (aFace : Face_Ptr) return GL.Types.Int is
   begin
      return GL.Types.Int (Face_Size (aFace).Metrics.X_Ppem);
   exception
         when others =>
            raise FreeType_Exception with "FT.Faces.Face_Width raised an exception.";
   end Face_Width;

   --  -------------------------------------------------------------------------

   function Slot_Ptr (aFace : Face_Ptr) return access FT.Glyphs.Glyph_Slot_Record is
     theFace : constant Face_Record := Face (aFace);
   begin
      if theFace.Glyph_Slot = Null then
         raise FreeType_Exception with "FT.Faces.Slot_Ptr - No Glyph is loaded.";
      end if;
      return theFace.Glyph_Slot;
   end Slot_Ptr;

   --  -------------------------------------------------------------------------

   procedure Kerning (aFace : Face_Ptr; Left_Glyph : GL.Types.UInt;
                         Right_Glyph : GL.Types.UInt; Kern_Mode : GL.Types.UInt;
                     aKerning : access FT.Image.FT_Vector) is
      use Errors;
      Code : constant Errors.Error_Code :=
               FT_Get_Kerning (aFace, Left_Glyph, Right_Glyph, Kern_Mode, aKerning);
   begin
      if Code /= Errors.Ok then
         raise FT.FreeType_Exception with "FT.Faces.Kerning error: " &
             Errors.Description (Code);
      end if;
   end Kerning;

   --  -------------------------------------------------------------------------

   function Left (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Left;
   end Left;

   --  ------------------------------------------------------------------------

   procedure Load_Character (aFace : Face_Ptr; Char_Code : GL.Types.Long;
                            Flags : Load_Flag) is
      use Errors;
      Code : constant Errors.Error_Code :=
                FT_Load_Char (aFace, ULong (Char_Code), Flags'Enum_Rep);
   begin
      if Code /= Errors.Ok then
         raise FT.FreeType_Exception with "FT.Faces.Load_Character error: " &
             Errors.Description (Code);
      end if;
   end Load_Character;

   --  -------------------------------------------------------------------------

   procedure New_Face (Library : Library_Ptr; File_Path_Name : String;
                      Face_Index : GL.Types.long; aFace : in out Face_Ptr) is
      use Errors;
      Path : constant Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (File_Path_Name);
      Code : constant Errors.Error_Code :=
               FT_New_Face (Library, Path, Face_Index, aFace);
   begin
      if Code /= Errors.Ok then
         if Code = Errors.Cannot_Open_Resource then
            raise FT.FreeType_Exception with "The file " &
                File_Path_Name & " cannot be found.";
         else
            raise FT.FreeType_Exception with "FT.Faces.Load_Character error: " &
                Errors.Description (Code);
         end if;
      end if;
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

  procedure Set_Pixel_Sizes (aFace : Face_Ptr; Pixel_Width : GL.Types.UInt;
                            Pixel_Height : GL.Types.UInt) is
      use Errors;
      Code : constant Errors.Error_Code :=
               FT_Set_Pixel_Sizes (aFace, Pixel_Width, Pixel_Height);
  begin
      if Code /= Errors.Ok then
         raise FT.FreeType_Exception with "FT.Faces.Load_Character error: " &
             Errors.Description (Code);
      end if;
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
