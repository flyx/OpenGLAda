--------------------------------------------------------------------------------
-- Copyright (c) 2017, Felix Krause <contact@flyx.org>
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
with FT.Faces;

package FT.API.Glyphs is
   pragma Preelaborate;

   procedure FT_Done_Glyph (Glyph : Glyph_Ptr);
   pragma Import (C, FT_Done_Glyph, "FT_Done_Glyph");

   --  tell the compiler that we are aware that Bool is 8-bit and will need to
   --  be a char on the C side.
   pragma Warnings (Off, "8-bit Ada Boolean");
   function FT_Glyph_To_Bitmap
     (theGlyph   : Glyph_Ptr; Mode : Faces.Render_Mode;
      Origin     : access Vector;
      Destroy    : Bool) return Errors.Error_Code;
   pragma Import (C, FT_Glyph_To_Bitmap, "FT_Glyph_To_Bitmap");
   pragma Warnings (On, "8-bit Ada Boolean");

   function FT_Get_Glyph (Slot_Ptr : Glyph_Slot_Ptr;
                          aGlyph   : out Glyph_Ptr)
                          return Errors.Error_Code;
   pragma Import (C, FT_Get_Glyph, "FT_Get_Glyph");

   function FT_Render_Glyph (Slot : access Glyph_Slot_Record;
                             Mode : Faces.Render_Mode)
                             return Errors.Error_Code;
   pragma Import (C, FT_Render_Glyph, "FT_Render_Glyph");

end FT.API.Glyphs;
