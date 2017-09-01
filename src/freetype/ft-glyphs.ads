
with System;
with Interfaces.C;

with GL.Objects.Textures;
with GL.Types;

with FT.Image;
with FT.Interfac;

package FT.Glyphs is

   type FT_Bitmap_Glyph is new System.Address;
   type FT_Glyph_Record is private;
   type FT_Glyph_Slot_Record is private;
   type FT_Glyph is new System.Address;
   type FT_Outline_Glyph is new System.Address;
   type Slot_Internal_Ptr is new System.Address;

   subtype FT_Pos is Interfaces.C.long;

   procedure Done_Glyph (Glyph_Ptr : FT_Glyph);

   function Get_Bitmap (Glyph_Slot : FT.Interfac.Glyph_Slot_Ptr)
                        return FT.Image.FT_Bitmap;
   function Get_Bitmap_Height (Slot_Ptr : FT.Interfac.Glyph_Slot_Ptr)
                               return GL.Types.Single;
   function Get_Bitmap_Image (Slot_Ptr : FT.Interfac.Glyph_Slot_Ptr)
                              return GL.Objects.Textures.Image_Source;
   function Get_Bitmap_Left (Slot_Ptr : FT.Interfac.Glyph_Slot_Ptr)
                             return GL.Types.Int;
   function Get_Bitmap_Rows (Slot_Ptr : FT.Interfac.Glyph_Slot_Ptr)
                             return GL.Types.Int;
   function Get_Bitmap_Top (Slot_Ptr : FT.Interfac.Glyph_Slot_Ptr)
                            return GL.Types.Int;
   function Get_Bitmap_Width (Slot_Ptr : FT.Interfac.Glyph_Slot_Ptr)
                              return GL.Types.Single;
   function Get_Glyph_Advance (Slot_Ptr : FT.Interfac.Glyph_Slot_Ptr)
                               return FT.Image.FT_Vector;
   function Get_Glyph_Record (aFace : FT.Interfac.Face_Ptr) return FT_Glyph_Record;
   function Get_Glyph_Format (Slot_Ptr : FT.Interfac.Glyph_Slot_Ptr)
                              return FT.Image.FT_Glyph_Format;
   function Glyph_To_Bitmap
     (theGlyph    : System.Address; Mode : FT.Interfac.Render_Mode;
      Origin      : access FT.Image.FT_Vector;
      Destroy     : FT.FT_Bool) return FT.FT_Error;
   function Get_Glyph (Slot_Ptr  : FT.Interfac.Glyph_Slot_Ptr;
                       Glyph_Ptr : in out System.Address) return FT.FT_Error;
private

   type Subglyph_Ptr is new System.Address;

   type FT_Glyph_Metrics is record
      Width        : FT.Image.FT_Pos;
      Height       : FT.Image.FT_Pos;
      HoriBearingX : FT.Image.FT_Pos;
      HoriBearingY : FT.Image.FT_Pos;
      HoriAdvance  : FT.Image.FT_Pos;
      VertBearingX : FT.Image.FT_Pos;
      VertBearingY : FT.Image.FT_Pos;
      VertAdvance  : FT.Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Glyph_Metrics);

   type FT_Glyph_Record is record
      Library : FT.Interfac.Library_Ptr;
      Clazz   : System.Address;
      Format  : FT.Image.FT_Glyph_Format;
      Advance : FT.Image.FT_Vector;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Glyph_Record);

   type FT_Outline_Glyph_Record is record
      Root    : FT_Glyph_Record;
      Outline : FT.Image.FT_Outline;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Outline_Glyph_Record);

   type FT_Glyph_Slot_Record is record
      Library           : FT.Interfac.Library_Ptr;
      Face              : FT.Interfac.Face_Ptr;
      Next              : FT.Interfac.Glyph_Slot_Ptr;
      Reserved          : GL.Types.UInt;
      C_Generic         : FT.Interfac.Generic_Record;
      Metrics           : FT_Glyph_Metrics;
      LinearHoriAdvance : GL.Types.long;
      LinearVertAdvance : GL.Types.long;
      Advance           : FT.Image.FT_Vector;
      Format            : FT.Image.FT_Glyph_Format;
      Bitmap            : FT.Image.FT_Bitmap;
      Bitmap_left       : GL.Types.Int;
      Bitmap_top        : GL.Types.Int;
      Outline           : FT.Image.FT_Outline;
      Num_subglyphs     : GL.Types.UInt;
      Subglyphs         : Subglyph_Ptr;
      Control_data      : System.Address;
      Control_len       : GL.Types.long;
      Lsb_Delta         : FT.Image.FT_Pos;
      Rsb_Delta         : FT.Image.FT_Pos;
      Other             : System.Address;
      Internal          : Slot_Internal_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, FT_Glyph_Slot_Record);

end FT.Glyphs;
