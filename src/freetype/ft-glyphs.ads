
with System;

with GL.Objects.Textures;
with GL.Types;

with FT.API;
with FT.Image;
with FT.Interfac;

package FT.Glyphs is

   type Glyph_Record is private;
   type Glyph_Slot_Record is private;
   type Glyph_Ptr is private;

   procedure Done_Glyph (Glyph : Glyph_Ptr);

   function Bitmap (Glyph_Slot : FT.API.Glyph_Slot_Ptr)
                        return FT.Image.Bitmap_Record;
   function Bitmap_Height (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                               return GL.Types.Single;
   function Bitmap_Image (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                              return GL.Objects.Textures.Image_Source;
   function Bitmap_Left (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                             return GL.Types.Int;
   function Bitmap_Rows (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                             return GL.Types.Int;
   function Bitmap_Top (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                            return GL.Types.Int;
   function Bitmap_Width (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                              return GL.Types.Single;
   function Glyph_Advance (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                               return FT.Image.FT_Vector;
   function Glyph_Format (Slot_Ptr : FT.API.Glyph_Slot_Ptr)
                              return FT.Image.Glyph_Format;
   function Glyph_To_Bitmap
     (theGlyph    : System.Address; Mode : FT.API.Render_Mode;
      Origin      : access FT.Image.FT_Vector;
      Destroy     : FT.FT_Bool) return FT.FT_Error;
   function Glyph (Slot_Ptr  : FT.API.Glyph_Slot_Ptr;
                       Glyph_Ptr : in out System.Address) return FT.FT_Error;
private

   type Bitmap_Glyph_Ptr is new System.Address;
   type Glyph_Ptr is new System.Address;
   type Outline_Glyph_Ptr is new System.Address;
   type Slot_Internal_Ptr is new System.Address;
   type Subglyph_Ptr is new System.Address;

   type Glyph_Metrics is record
      Width        : FT.Image.FT_Pos;
      Height       : FT.Image.FT_Pos;
      HoriBearingX : FT.Image.FT_Pos;
      HoriBearingY : FT.Image.FT_Pos;
      HoriAdvance  : FT.Image.FT_Pos;
      VertBearingX : FT.Image.FT_Pos;
      VertBearingY : FT.Image.FT_Pos;
      VertAdvance  : FT.Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Metrics);

   type Glyph_Record is record
      Library : FT.API.Library_Ptr;
      Clazz   : System.Address;
      Format  : FT.Image.Glyph_Format;
      Advance : FT.Image.FT_Vector;
   end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Record);

   type Outline_Glyph_Record is record
      Root    : Glyph_Record;
      Outline : FT.Image.Outline_Record;
   end record;
   pragma Convention (C_Pass_By_Copy, Outline_Glyph_Record);

   type Glyph_Slot_Record is record
      Library           : FT.API.Library_Ptr;
      Face              : FT.API.Face_Ptr;
      Next              : FT.API.Glyph_Slot_Ptr;
      Reserved          : GL.Types.UInt;
      C_Generic         : FT.Interfac.Generic_Record;
      Metrics           : Glyph_Metrics;
      LinearHoriAdvance : GL.Types.long;
      LinearVertAdvance : GL.Types.long;
      Advance           : FT.Image.FT_Vector;
      Format            : FT.Image.Glyph_Format;
      Bitmap            : FT.Image.Bitmap_Record;
      Bitmap_left       : GL.Types.Int;
      Bitmap_top        : GL.Types.Int;
      Outline           : FT.Image.Outline_Record;
      Num_subglyphs     : GL.Types.UInt;
      Subglyphs         : Subglyph_Ptr;
      Control_data      : System.Address;
      Control_len       : GL.Types.long;
      Lsb_Delta         : FT.Image.FT_Pos;
      Rsb_Delta         : FT.Image.FT_Pos;
      Other             : System.Address;
      Internal          : Slot_Internal_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Slot_Record);

end FT.Glyphs;
