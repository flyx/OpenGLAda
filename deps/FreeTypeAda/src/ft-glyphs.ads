--  part of FreeTypeAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with FT.Faces;

package FT.Glyphs is
   pragma Preelaborate;

   type Glyph_Reference is limited new Ada.Finalization.Limited_Controlled
   with private;

   --  call this to destruct the
   overriding procedure Finalize (Object : in out Glyph_Reference);

   procedure Get_Glyph (Object : Glyph_Slot_Reference;
                        Target : out Glyph_Reference);

   function Bitmap (Object : Glyph_Slot_Reference) return Bitmap_Record;
   function Bitmap_Top (Object : Glyph_Slot_Reference)
                        return Interfaces.C.int;
   function Bitmap_Left (Object : Glyph_Slot_Reference) return Interfaces.C.int;
   function Advance (Object : Glyph_Slot_Reference) return Vector;
   function Format (Object : Glyph_Slot_Reference) return Glyph_Format;
   procedure Glyph_To_Bitmap
     (Object : Glyph_Reference; Mode : FT.Faces.Render_Mode;
      Origin : access Vector; Destroy     : Boolean);
   procedure Render_Glyph (Object : Glyph_Slot_Reference; Mode : FT.Faces.Render_Mode);
private
   type Glyph_Reference is limited new Ada.Finalization.Limited_Controlled with
      record
         Data : Glyph_Ptr;
      end record;

   type Outline_Glyph_Record;
   type Outline_Glyph_Ptr is access Outline_Glyph_Record;
   pragma Convention (C, Outline_Glyph_Ptr);

   --  A Glyph can be typecast to an Outline_Glyph if
   --  glyph->format == GLYPH_FORMAT_OUTLINE.
   --  This provides easy access the outline's content.
   --  As the outline is extracted from a glyph slot, its coordinates are
   --  expressed normally in 26.6 pixels, unless the flag
   --  LOAD_NO_SCALE was used in FT_Load_Glyph() or FT_Load_Char().
   type Outline_Glyph_Record is record
      Root    : Glyph_Record;
      Outline : Outline_Record;
   end record;
   pragma Convention (C_Pass_By_Copy, Outline_Glyph_Record);
end FT.Glyphs;
