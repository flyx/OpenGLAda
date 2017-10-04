--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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
with FT.API.Glyphs;

package body FT.Glyphs is
   use type Errors.Error_Code;

   procedure Finalize (Object : in out Glyph_Reference) is
      Ptr : constant Glyph_Ptr := Object.Data;
   begin
      Object.Data := null;
      if Ptr /= null then
         API.Glyphs.FT_Done_Glyph (Ptr);
      end if;
   end Finalize;

   procedure Get_Glyph (Object : Glyph_Slot_Reference;
                        Target : out Glyph_Reference) is
      Ret  : Glyph_Ptr;
      Code : Errors.Error_Code;
   begin
      Code := API.Glyphs.FT_Get_Glyph (Object.Data, Ret);
      if Code /= Errors.Ok then
         raise FreeType_Exception with
           "FT.Faces.Glyphs.Get_Glyph error :" & Errors.Description (Code);
      end if;
      Target.Finalize;
      Target.Data := Ret;
   exception
      when others =>
         raise FreeType_Exception with
           "FT.Glyphs.Glyph raised an Exception";
   end Get_Glyph;

   --  -------------------------------------------------------------------------

   function Bitmap (Object : Glyph_Slot_Reference) return Bitmap_Record is
   begin
      return Object.Data.Bitmap;
   end Bitmap;

   --  -------------------------------------------------------------------------

   function Bitmap_Left (Object : Glyph_Slot_Reference) return GL.Types.Int is
   begin
      return Object.Data.Bitmap_Left;
   end Bitmap_Left;

   --  -------------------------------------------------------------------------

   function Bitmap_Top (Object : Glyph_Slot_Reference)
                            return GL.Types.Int is
   begin
      return Object.Data.Bitmap_Top;
   end Bitmap_Top;

   --  -------------------------------------------------------------------------

   function Advance (Object : Glyph_Slot_Reference) return Vector is
   begin
      return Object.Data.Advance;
   end Advance;

   --  -------------------------------------------------------------------------

   function Format (Object : Glyph_Slot_Reference) return Glyph_Format is
   begin
      return Object.Data.Format;
   end Format;

   --  -------------------------------------------------------------------------

   procedure Glyph_To_Bitmap
     (Object : Glyph_Reference; Mode : FT.Faces.Render_Mode;
      Origin : access Vector; Destroy     : Boolean) is
      use Errors;
      Code : constant Errors.Error_Code :=
               API.Glyphs.FT_Glyph_To_Bitmap (Object.Data, Mode, Origin, FT.API.Bool (Destroy));
   begin
      if Code /= Errors.Ok then
         raise FT.FreeType_Exception with "FT.Glyphs.Glyph_To_Bitmap error: " &
             Errors.Description (Code);
      end if;
   end Glyph_To_Bitmap;

   --  -------------------------------------------------------------------------

   procedure Render_Glyph (Object : Glyph_Slot_Reference;
                           Mode : FT.Faces.Render_Mode) is
      Code : Errors.Error_Code;
   begin
      Code := FT.API.Glyphs.FT_Render_Glyph (Object.Data, Mode);
      if Code /= Errors.Ok then
         raise FT.FreeType_Exception with "FT.Glyphs.Render_Glyph error: " &
             Errors.Description (Code);
      end if;
   end Render_Glyph;

   --  -------------------------------------------------------------------------

end FT.Glyphs;
