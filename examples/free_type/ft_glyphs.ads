
with System;
with Interfaces.C;

with GL.Objects.Textures;
with GL.Types;

with FT_Image;
with FT_Types;

package FT_Glyphs is

   type FT_Bitmap_Glyph is new System.Address;
   type FT_Glyph_Record is private;
   type FT_Glyph is new System.Address;
   type FT_Outline_Glyph is new System.Address;
   subtype FT_Pos is Interfaces.C.long;

   procedure Done_Glyph (Glyph_Ptr : FT_Glyph);

   function Get_Bitmap_Height (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Single;
   function Get_Bitmap_Image (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Objects.Textures.Image_Source;
   function Get_Bitmap_Left (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Int;
   function Get_Bitmap_Rows (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Int;
   function Get_Bitmap_Top (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Int;
   function Get_Bitmap_Width (Slot_Ptr : FT_Types.FT_Glyph_Slot) return GL.Types.Single;
   function Get_Glyph_Advance (Slot_Ptr : FT_Types.FT_Glyph_Slot) return FT_Image.FT_Vector;
   function Glyph_To_Bitmap
       (theGlyph    : System.Address; Render_Mode : FT_Types.FT_Render_Mode;
        Origin      : access FT_Image.FT_Vector;
        Destroy     : FT_Types.FT_Bool) return FT_Types.FT_Error;
   function Get_Glyph (Slot_Ptr  : FT_Types.FT_Glyph_Slot;
                       Glyph_Ptr : in out System.Address) return FT_Types.FT_Error;
private

   type FT_Bitmap_Glyph_Record is record
      Root   : aliased FT_Glyph_Record;
      Left   : aliased FT_Types.FT_Int;
      Top    : aliased FT_Types.FT_Int;
      Bitmap : aliased FT_Image.FT_Bitmap;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Bitmap_Glyph_Record);

   type FT_Glyph_Record is record
      Library : FT_Types.FT_Library;
      Clazz   : System.Address;
      Format  : aliased FT_Image.FT_Glyph_Format;
      Advance : aliased FT_Image.FT_Vector;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Glyph_Record);

   type FT_Outline_Glyph_Record is record
      root : aliased FT_Glyph_Record;
      outline : aliased FT_Image.FT_Outline;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Outline_Glyph_Record);

end FT_Glyphs;
