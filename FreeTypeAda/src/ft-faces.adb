--  part of FreeTypeAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with FT.Errors;
with FT.API;

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

   --  ------------------------------------------------------------------------

   function Initialized (Object : Face_Reference) return Boolean is
   begin
      return Object.Data /= null;
   end Initialized;

   --  ------------------------------------------------------------------------

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
      Check_Face_Ptr (Object);
      if Object.Data.Available_Sizes = null then
         raise FreeType_Exception with
           "FT.Faces.Size failed, there are no sizes available for this face.";
      end if;
      return Object.Data.Available_Sizes.all;
   end Size;

   --  ------------------------------------------------------------------------

   procedure Check_Face_Ptr (Object : Face_Reference) is
   begin
      if Object.Data = null then
         raise Constraint_Error with "Face_Reference not initialized";
      end if;
   end Check_Face_Ptr;

   --  -------------------------------------------------------------------------

   procedure Kerning (Object      : Face_Reference; Left_Glyph : UInt;
                      Right_Glyph : UInt; Kern_Mode : UInt;
                      aKerning    : access Vector) is
   begin
      Check_Face_Ptr (Object);
      declare
         Code : constant Errors.Error_Code :=
           API.FT_Get_Kerning (Object.Data, Left_Glyph, Right_Glyph, Kern_Mode,
                               aKerning);
      begin
         if Code /= Errors.Ok then
            raise FT.FreeType_Exception with "FT.Faces.Kerning error: " &
              Errors.Description (Code);
         end if;
      end;
   end Kerning;

   function Character_Index (Object : Face_Reference; Char_Code : ULong)
                             return Char_Index_Type is
   begin
      Check_Face_Ptr (Object);
      return API.FT_Get_Char_Index (Object.Data, Char_Code);
   end Character_Index;

   procedure Load_Glyph (Object : Face_Reference; Glyph_Index : Char_Index_Type;
                         Flags : Load_Flag) is
   begin
      Check_Face_Ptr (Object);
      declare
         Code : constant Errors.Error_Code :=
           API.FT_Load_Glyph (Object.Data, Glyph_Index, Flags);
      begin
         if Code /= Errors.Ok then
            raise FT.FreeType_Exception with "FT.Faces.Load_Glyph error: " &
              Errors.Description (Code) & Character'Val (10);
         end if;
      end;
   end Load_Glyph;

   --  ------------------------------------------------------------------------

   procedure Load_Character (Object : Face_Reference; Char_Code : ULong;
                             Flags  : Load_Flag) is
   begin
      Check_Face_Ptr (Object);
      declare
         Code : constant Errors.Error_Code :=
           API.FT_Load_Char (Object.Data, Char_Code, Flags);
      begin
         if Code /= Errors.Ok then
            raise FT.FreeType_Exception with "FT.Faces.Load_Character error: " &
              Errors.Description (Code) & Character'Val (10) &
              "while loading character #" & Char_Code'Img;
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
                       Face_Index : Face_Index_Type;
                       Object : in out Face_Reference) is
      Path : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (File_Path_Name);
   begin
      --  cleanup possible old reference
      Object.Finalize;
      declare
         Code : constant Errors.Error_Code :=
           API.FT_New_Face (Library.Data, Path, Face_Index,
                            Object.Data'Unchecked_Access);
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
      Interfaces.C.Strings.Free (Path);
      Object.Library := Library;
   end New_Face;

   --  -------------------------------------------------------------------------

   procedure Set_Pixel_Sizes (Object : Face_Reference;
                              Pixel_Width : UInt;
                              Pixel_Height : UInt) is
   begin
      Check_Face_Ptr (Object);
      declare
         Code : constant Errors.Error_Code :=
           API.FT_Set_Pixel_Sizes (Object.Data, Pixel_Width, Pixel_Height);
      begin
         if Code /= Errors.Ok then
            raise FT.FreeType_Exception with "FT.Faces.Set_Pixel_Sizes error: " &
              Errors.Description (Code);
         end if;
      end;
   end Set_Pixel_Sizes;

   --  -------------------------------------------------------------------------

   function Glyph_Slot (Object : Face_Reference) return Glyph_Slot_Reference is
   begin
      Check_Face_Ptr (Object);
      if Object.Data.Glyph_Slot = null then
         raise FreeType_Exception with "FT.Faces.Sloc - no Glyph is loaded.";
      end if;
      return (Data => Object.Data.Glyph_Slot);
   end Glyph_Slot;

   --  ------------------------------------------------------------------------

end FT.Faces;
