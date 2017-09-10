
with System.Address_To_Access_Conversions;

with Ada.Text_IO; use Ada.Text_IO;

with FT.API.Glyphs;
with FT.Errors;

package body FT.Glyphs is
   package Glyph_Slot_Access is new
       System.Address_To_Access_Conversions (Glyph_Slot_Record);
   package Glyph_Access is new
       System.Address_To_Access_Conversions (Glyph_Record);

   procedure Check_Glyph_Ptr (thePtr : Glyph_Ptr);
   procedure Check_Glyph_Slot_Ptr (thePtr : FT.API.Glyph_Slot_Ptr);

   function Glyph (Slot_Ptr     : FT.API.Glyph_Slot_Ptr;
                   theGlyph_Ptr : in out Glyph_Ptr) return FT.FT_Error;

   --  -------------------------------------------------------------------------

   procedure Done_Glyph (Glyph : Glyph_Ptr) is
   begin
      Check_Glyph_Ptr (Glyph);
      FT.API.Glyphs.FT_Done_Glyph (Glyph);
   exception
      when others =>
         Put_Line ("FT.Glyphs.Done_Glyph raised an Exception");
         raise FT.FT_Exception;
   end Done_Glyph;

   --  -------------------------------------------------------------------------
   --  Glyph_Slot (SA) => Glyph_Slot_Record => Bitmap_Record
   --  Bitmap_Record => Buffer (access unsigned_char)

   function Bitmap (Face_Ptr : FT.API.Face_Ptr;
                    theBitmap : out FT.Image.Bitmap_Record)
                    return FT.FT_Error is
      use GL.Types;
      use Glyph_Slot_Access;
      Glyph_Slot    : constant FT.API.Glyph_Slot_Ptr
                      := FT.Interfac.Glyph_Slot (Face_Ptr);
      aGlyph_Ptr    : Glyph_Ptr;
      Glyph_Pointer :  Object_Pointer;
      theGlyph      :  Glyph_Slot_Record;
      Code          :  FT.FT_Error;
   begin
      Check_Glyph_Slot_Ptr (Glyph_Slot);
      Glyph_Pointer:= To_Pointer (System.Address (Glyph_Slot));
      theGlyph := Glyph_Pointer.all;
      Code := Glyph (Glyph_Slot, aGlyph_Ptr);
      --  Glyph calls the FT_Glyph C function.
      theBitmap := theGlyph.Bitmap;
      return Code;
   exception
      when others =>
         Put_Line ("FT.Glyphs.Bitmap raised an Exception");
         raise FT.FT_Exception;
   end Bitmap;

   --  -------------------------------------------------------------------------

   function Bitmap_Image (Face_Ptr : FT.API.Face_Ptr;
                          theImage : out GL.Objects.Textures.Image_Source)
                          return FT.FT_Error is
      use GL.Types;
      theBitmap  : FT.Image.Bitmap_Record;
      Error_Code : constant FT.FT_Error := Bitmap (Face_Ptr, theBitmap);
   begin
      if Error_Code = 0 then
         theImage := FT.Image.Buffer (theBitmap);
      end if;
      return Error_Code;
 --     return FT.Image.Buffer (Bitmap (Slot_Ptr));
   exception
      when others =>
         Put_Line ("FT.Glyphs.Bitmap_Image raised an Exception");
         raise FT.FT_Exception;
   end Bitmap_Image;

   --  -------------------------------------------------------------------------

   function Bitmap_Left (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Int is
      use Glyph_Slot_Access;
      Slot_Ptr : constant FT.API.Glyph_Slot_Ptr := FT.Interfac.Glyph_Slot (Face_Ptr);
      Glyph    :  Glyph_Slot_Record;
   begin
      Check_Glyph_Slot_Ptr (Slot_Ptr);
      Glyph := To_Pointer (System.Address (Slot_Ptr)).all;
      return Glyph.Bitmap_Left;
   exception
      when others =>
         Put_Line ("FT.Glyphs.Bitmap_Left raised an Exception");
         raise FT.FT_Exception;
   end Bitmap_Left;

   --  -------------------------------------------------------------------------

   function Bitmap_Width (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Single is
      use GL.Types;
      theBitmap  : FT.Image.Bitmap_Record;
      Error_Code : constant FT.FT_Error := Bitmap (Face_Ptr, theBitmap);
   begin
      if Error_Code /= 0 then
         Put_Line ("FT.Glyphs.Bitmap_Width raised an exception " &
              FT.Errors.Error (Error_Code));
         raise FT.FT_Exception;
      end if;
      return GL.Types.Single (FT.Image.Width (theBitmap));
   end Bitmap_Width;

   --  -------------------------------------------------------------------------

   function Bitmap_Height (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Single is
      use GL.Types;
      theBitmap  : FT.Image.Bitmap_Record;
      Error_Code : constant FT.FT_Error := Bitmap (Face_Ptr, theBitmap);
   begin
      if Error_Code /= 0 then
         Put_Line ("FT.Glyphs.Bitmap_Width raised an exception " &
              FT.Errors.Error (Error_Code));
         raise FT.FT_Exception;
      end if;
      return GL.Types.Single (FT.Image.Rows (theBitmap));
   end Bitmap_Height;

   --  -------------------------------------------------------------------------

   function Bitmap_Rows (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Int is
      use GL.Types;
      theBitmap  : FT.Image.Bitmap_Record;
      Error_Code : constant FT.FT_Error := Bitmap (Face_Ptr, theBitmap);
   begin
      if Error_Code /= 0 then
         Put ("FT.Glyphs.Bitmap_Width raised an exception " &
              FT.Errors.Error (Error_Code));
         raise FT.FT_Exception;
      end if;
      return FT.Image.Rows (theBitmap);
   end Bitmap_Rows;

   --  -------------------------------------------------------------------------

   function Bitmap_Top (Face_Ptr : FT.API.Face_Ptr)
                            return GL.Types.Int is
      use Glyph_Slot_Access;
      Slot_Ptr  : constant FT.API.Glyph_Slot_Ptr := FT.Interfac.Glyph_Slot (Face_Ptr);
      Glyph     : Glyph_Slot_Record;
   begin
      Check_Glyph_Slot_Ptr (Slot_Ptr);
      Glyph := To_Pointer (System.Address (Slot_Ptr)).all;
      return Glyph.Bitmap_Top;
   exception
      when others =>
         Put_Line ("FT.Glyphs.Bitmap_Top raised an Exception");
         raise FT.FT_Exception;
   end Bitmap_Top;

   --  -------------------------------------------------------------------------

   procedure Check_Glyph_Ptr (thePtr : Glyph_Ptr) is
      use System;
   begin
      if System.Address (thePtr) = System.Null_Address then
         Put_Line ("No glyph is loaded.");
         raise FT.FT_Exception;
      end if;
   end Check_Glyph_Ptr;

   --  -------------------------------------------------------------------------

   procedure Check_Glyph_Slot_Ptr (thePtr : FT.API.Glyph_Slot_Ptr) is
      use System;
   begin
      if System.Address (thePtr) = System.Null_Address then
         Put_Line ("No glyph is loaded.");
         raise FT.FT_Exception;
      end if;
   end Check_Glyph_Slot_Ptr;

   --  -------------------------------------------------------------------------

   function Glyph (Face_Ptr : FT.API.Face_Ptr; theGlyph : out Glyph_Record)
                   return FT.FT_Error is
   use GL.Types;
      use Glyph_Access;
      aGlyph_Slot : constant FT.API.Glyph_Slot_Ptr :=
                      FT.Interfac.Glyph_Slot (Face_Ptr);
      aGlyph_Ptr : Glyph_Ptr;
      Code       : FT.FT_Error;
   begin
      Check_Glyph_Slot_Ptr (aGlyph_Slot);
      Code := Glyph (aGlyph_Slot, aGlyph_Ptr);
      if Code = 0 then
         theGlyph := Glyph (aGlyph_Ptr);
      end if;
      return Code;
   exception
      when others =>
         Put_Line ("FT.Glyphs.Glyph raised an Exception");
         raise FT.FT_Exception;
   end Glyph;

   --  -------------------------------------------------------------------------

   function Glyph (aGlyph_Ptr : Glyph_Ptr) return Glyph_Record is
      use Glyph_Access;
      Glyph_Acc : access Glyph_Record;
   begin
      Check_Glyph_Ptr (aGlyph_Ptr);
      Glyph_Acc := To_Pointer (System.Address (aGlyph_Ptr));
      return Glyph_Acc.all;
   exception
         when others =>
            Put_Line ("FT.Glyphs.Glyph aGlyph_Ptr raised an Exception");
            raise FT.FT_Exception;
   end Glyph;

   --  -------------------------------------------------------------------------

   function Glyph (Slot_Ptr     : FT.API.Glyph_Slot_Ptr;
                   theGlyph_Ptr : in out Glyph_Ptr) return FT.FT_Error is
   begin
      Check_Glyph_Slot_Ptr (Slot_Ptr);
      return FT.API.Glyphs.FT_Get_Glyph (Slot_Ptr, System.Address (theGlyph_Ptr));
   exception
         when others =>
            Put_Line ("FT.Glyphs.Glyph Slot_Ptr raised an Exception");
            raise FT.FT_Exception;
   end Glyph;

   --  -------------------------------------------------------------------------

   function Glyph_Advance (Face_Ptr : FT.API.Face_Ptr)
                               return FT.Image.FT_Vector is
      use Glyph_Slot_Access;
      Slot_Ptr : constant FT.API.Glyph_Slot_Ptr :=
                      FT.Interfac.Glyph_Slot (Face_Ptr);
      Glyph : Glyph_Slot_Record;
   begin
      Check_Glyph_Slot_Ptr (Slot_Ptr);
      Glyph := To_Pointer (System.Address (Slot_Ptr)).all;
      return Glyph.Advance;
   exception
      when others =>
         Put_Line ("FT.Glyphs.Glyph_Advance raised an Exception");
         raise FT.FT_Exception;
   end Glyph_Advance;

   --  -------------------------------------------------------------------------

   function Glyph_Format (Face_Ptr : FT.API.Face_Ptr)
                              return FT.Image.Glyph_Format is
      use Glyph_Slot_Access;
      Slot_Ptr : constant FT.API.Glyph_Slot_Ptr :=
                      FT.Interfac.Glyph_Slot (Face_Ptr);
      Glyph : Glyph_Slot_Record;
   begin
      Check_Glyph_Slot_Ptr (Slot_Ptr);
      Glyph := To_Pointer (System.Address (Slot_Ptr)).all;
      return Glyph.Format;
   exception
      when others =>
         Put_Line ("FT.Glyphs.Glyph_Format raised an Exception");
         raise FT.FT_Exception;
   end Glyph_Format;

   --  -------------------------------------------------------------------------

   function Glyph_To_Bitmap
     (theGlyph    : System.Address; Mode : FT.API.Render_Mode;
      Origin      : access FT.Image.FT_Vector;
      Destroy     : FT.FT_Bool) return FT.FT_Error is
   begin
      return FT.API.Glyphs.FT_Glyph_To_Bitmap (theGlyph, Mode,
                                               Origin, Destroy);
   exception
      when others =>
         Put_Line ("FT.Glyphs.Glyph_To_Bitmap raised an Exception");
         raise FT.FT_Exception;
   end Glyph_To_Bitmap;

   --  -------------------------------------------------------------------------

   function Render_Glyph (aFace : FT.API.Face_Ptr; Mode : FT.API.Render_Mode)
                          return FT_Error is
      Slot : constant FT.API.Glyph_Slot_Ptr := FT.Interfac.Glyph_Slot (aFace);
   begin
      return  FT.API.Glyphs.FT_Render_Glyph (Slot, Mode);
   end Render_Glyph;

   --  -------------------------------------------------------------------------

end FT.Glyphs;
