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
   use type Errors.Error_Code;

   procedure Adjust (Object : in out Face_Reference) is
   begin
      if Object.Data /= null then
         if API.FT_Reference_Face (Object.Data) /= Errors.Ok then
            null;
         end if;
      end if;
   end Adjust;

   function Initialized (Object : Face_Reference) return Boolean is
   begin
      return Object.Data /= null;
   end Initialized;

   procedure Finalize (Object : in out Face_Reference) is
      Ptr : constant Face_Ptr := Object.Data;
   begin
      Object.Data := null;
      if Ptr /= null then
         if API.FT_Done_Face (Ptr) /= Errors.Ok then
            null;
         end if;
      end if;
   end Finalize;

   --  ------------------------------------------------------------------------

   function Size (Object : Face_Reference) return Bitmap_Size is
   begin
      if Object.Data.Available_Sizes = null then
         raise FreeType_Exception with
           "FT.Faces.Size failed, there are no sizes available for this face.";
      end if;
      return Object.Data.Available_Sizes.all;
   end Size;

   --  -------------------------------------------------------------------------

   function Character_Size (Char : Character_Record) return Character_Size_Type is
   begin
      return Char.Size;
   end Character_Size;

   --  -------------------------------------------------------------------------

   function Character_Data_To_String (Char : Character;
                                      Data : Character_Record) return String is
      use GL.Types;
   begin
      return "Character" & Char & " Data" & Character'Val (10) &
             "Width: " & GL.Types.Int'Image (Data.Size.Width) & Character'Val (10) &
             "Rows: " & GL.Types.Int'Image (Data.Size.Rows) & Character'Val (10) &
             "Left: " & GL.Types.Int'Image (Data.Size.Left) & Character'Val (10) &
             "Top: " & GL.Types.Int'Image (Data.Size.Top) & Character'Val (10) &
             "Advance X: " & GL.Types.Int'Image (Data.Size.Advance_X) & " bits";
   end Character_Data_To_String;

   --  ------------------------------------------------------------------------

   function Character_Texture (Data : Character_Record)
                               return GL.Objects.Textures.Texture is
   begin
      return Data.Texture;
   end Character_Texture;

   --  ------------------------------------------------------------------------

   procedure Check_Face_Ptr (Object : Face_Reference) is
   begin
      if Object.Data = null then
         raise FreeType_Exception with
           "FT.Faces.Check_Face_Ptr - No face is loaded, Face_Ptr is null.";
      end if;
   end Check_Face_Ptr;

   --  -------------------------------------------------------------------------

   procedure Check_Glyph_Slot_Ptr (thePtr : access Glyph_Slot_Record) is
   begin
      if thePtr = null then
         raise FreeType_Exception with
           "FT.Faces.Check_Glyph_Slot_Ptr - No glyph is loaded, Glyph_Slot_Ptr is null.";
      end if;
   end Check_Glyph_Slot_Ptr;

   --  -------------------------------------------------------------------------

   procedure Kerning (Object      : Face_Reference; Left_Glyph : GL.Types.UInt;
                      Right_Glyph : GL.Types.UInt; Kern_Mode : GL.Types.UInt;
                      aKerning    : access Vector) is
   begin
      Check_Face_Ptr (Object);
      declare
         Code : constant Errors.Error_Code :=
           FT_Get_Kerning (Object.Data, Left_Glyph, Right_Glyph, Kern_Mode,
                           aKerning);
      begin
         if Code /= Errors.Ok then
            raise FT.FreeType_Exception with "FT.Faces.Kerning error: " &
              Errors.Description (Code);
         end if;
      end;
   end Kerning;

   --  ------------------------------------------------------------------------

   procedure Load_Character (Object : Face_Reference; Char_Code : GL.Types.Long;
                             Flags  : Load_Flag) is
   begin
      Check_Face_Ptr (Object);
      declare
         Code : constant Errors.Error_Code :=
           FT_Load_Char (Object.Data, ULong (Char_Code), Flags'Enum_Rep);
      begin
         if Code /= Errors.Ok then
            raise FT.FreeType_Exception with "FT.Faces.Load_Character error: " &
              Errors.Description (Code);
         end if;
      end;
   end Load_Character;

   --  -------------------------------------------------------------------------

   function Metrics (Object : Face_Reference) return Size_Metrics is
   begin
      Check_Face_Ptr (Object);
      return Object.Data.Size.Metrics;
   end Metrics;

   --  -------------------------------------------------------------------------

   procedure New_Face (Library : Library_Reference; File_Path_Name : String;
                       Face_Index : GL.Types.long;
                       Object : in out Face_Reference) is
      use Errors;
      Path : constant Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (File_Path_Name);
   begin
      --  cleanup possible old reference
      Object.Finalize;
      declare
         Code : constant Errors.Error_Code :=
           FT_New_Face (Library.Data, Path, Face_Index, Object.Data);
      begin
         if Code /= Errors.Ok then
            if Code = Errors.Cannot_Open_Resource then
               raise FT.FreeType_Exception with "The file " &
                 File_Path_Name & " cannot be found.";
            else
               raise FT.FreeType_Exception with "FT.Faces.New_Face error: " &
                 Errors.Description (Code);
            end if;
         end if;
      end;
      Object.Library := Library;
   end New_Face;

   --  -------------------------------------------------------------------------

   procedure Set_Char_Size (Char_Data : in out Character_Record;
                            Value : Character_Size_Type) is
   begin
      Char_Data.Size := Value;
   end Set_Char_Size;

   --  -------------------------------------------------------------------------

   procedure Set_Pixel_Sizes (Object : Face_Reference;
                              Pixel_Width : GL.Types.UInt;
                              Pixel_Height : GL.Types.UInt) is
      use Errors;
      Code : constant Errors.Error_Code :=
        FT_Set_Pixel_Sizes (Object.Data, Pixel_Width, Pixel_Height);
   begin
      if Code /= Errors.Ok then
         raise FT.FreeType_Exception with "FT.Faces.Set_Pixel_Sizes error: " &
           Errors.Description (Code);
      end if;
   end Set_Pixel_Sizes;

   --  -------------------------------------------------------------------------

   procedure Set_Texture (Char_Data : in out Character_Record;
                          Texture   : GL.Objects.Textures.Texture) is
   begin
      Char_Data.Texture := Texture;
   end Set_Texture;

   --  -------------------------------------------------------------------------

   function Slot_Ptr (Object : Face_Reference) return Glyph_Slot_Ptr is
     theFace : constant access Face_Record := Object.Data;
   begin
      if theFace.Glyph_Slot = Null then
         raise FreeType_Exception with "FT.Faces.Slot_Ptr - No Glyph is loaded.";
      end if;
      return theFace.Glyph_Slot;
   end Slot_Ptr;

   --  ------------------------------------------------------------------------

end FT.Faces;
