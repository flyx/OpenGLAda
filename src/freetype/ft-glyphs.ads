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

with System;

with GL.Objects.Textures;
with GL.Types;

with FT.API;
with FT.Errors;
with FT.Image;
with FT.Faces;

package FT.Glyphs is
   pragma Preelaborate;

   type Glyph_Record is private;
   type Glyph_Slot_Record is private;
   type Glyph_Ptr is private;

   procedure Done_Glyph (Glyph : Glyph_Ptr);

   procedure Bitmap (Face_Ptr : FT.API.Face_Ptr;
                    theBitmap : out FT.Image.Bitmap_Record);
   function Bitmap_Height (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Single;
   procedure Bitmap_Image (Face_Ptr : FT.API.Face_Ptr;
                          theImage : out GL.Objects.Textures.Image_Source);
   function Bitmap_Left (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Int;
   function Bitmap_Rows (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Int;
   function Bitmap_Top (Face_Ptr : FT.API.Face_Ptr)
                        return GL.Types.Int;
   function Bitmap_Width (Face_Ptr : FT.API.Face_Ptr) return GL.Types.Single;
   procedure Glyph (Face_Ptr : FT.API.Face_Ptr; theGlyph : out Glyph_Record);
   function Glyph (aGlyph_Ptr : Glyph_Ptr) return Glyph_Record;
   function Glyph_Advance (Face_Ptr : FT.API.Face_Ptr) return FT.Image.FT_Vector;
   function Glyph_Format (Face_Ptr : FT.API.Face_Ptr)
                          return FT.Image.Glyph_Format;
   function Glyph_To_Bitmap
     (theGlyph    : System.Address; Mode : FT.API.Render_Mode;
      Origin      : access FT.Image.FT_Vector;
      Destroy     : Bool) return Errors.Error_Code;
   function Render_Glyph (aFace : FT.API.Face_Ptr; Mode : FT.API.Render_Mode)
                          return Errors.Error_Code;
private
   type Bitmap_Glyph_Ptr is new System.Address;
   type Glyph_Ptr is new System.Address;
   type Outline_Glyph_Ptr is new System.Address;
   type Slot_Internal_Ptr is new System.Address;
   type Subglyph_Ptr is new System.Address;

   type Glyph_Metrics is record
      Width        : FT.Image.FT_Pos;
      Height       : FT.Image.FT_Pos;
      Horiz_Bearing_X : FT.Image.FT_Pos;
      Horiz_Bearing_Y : FT.Image.FT_Pos;
      Horiz_Advance  : FT.Image.FT_Pos;
      Vert_Bearing_X : FT.Image.FT_Pos;
      Vert_Bearing_Y : FT.Image.FT_Pos;
      Vert_Advance   : FT.Image.FT_Pos;
   end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Metrics);

   type Glyph_Record is record
      Library : FT.API.Library_Ptr;
      Clazz   : System.Address;
      Format  : FT.Image.Glyph_Format;
      Advance : FT.Image.FT_Vector;
   end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Record);

   --  A FT_Glyph can be typecast to a FT_OutlineGlyph if
   --  glyph->format == FT_GLYPH_FORMAT_OUTLINE.
   --  This provides easy access the outline's content.
   --  As the outline is extracted from a glyph slot, its coordinates are
   --  expressed normally in 26.6 pixels, unless the flag
   --  FT_LOAD_NO_SCALE was used in FT_Load_Glyph() or FT_Load_Char().
   type Outline_Glyph_Record is record
      Root    : Glyph_Record;
      Outline : FT.Image.Outline_Record;
   end record;
   pragma Convention (C_Pass_By_Copy, Outline_Glyph_Record);

   type Glyph_Slot_Record is record
      Library              : FT.API.Library_Ptr;
      Face                 : FT.API.Face_Ptr;
      Next                 : FT.API.Glyph_Slot_Ptr;
      Reserved             : GL.Types.UInt;
      C_Generic            : FT.Faces.Generic_Record;
      Metrics              : Glyph_Metrics;
      Linear_Horiz_Advance : GL.Types.long;
      Linear_Vert_Advance  : GL.Types.long;
      Advance              : FT.Image.FT_Vector;
      Format               : FT.Image.Glyph_Format;
      Bitmap               : FT.Image.Bitmap_Record;
      Bitmap_Left          : GL.Types.Int;
      Bitmap_Top           : GL.Types.Int;
      Outline              : FT.Image.Outline_Record;
      Num_Subglyphs        : GL.Types.UInt;
      Subglyphs            : Subglyph_Ptr;
      Control_Data         : System.Address;
      Control_Length       : GL.Types.long;
      Lsb_Delta            : FT.Image.FT_Pos;
      Rsb_Delta            : FT.Image.FT_Pos;
      Other                : System.Address;
      Internal             : Slot_Internal_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Slot_Record);

end FT.Glyphs;
