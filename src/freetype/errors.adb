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

package body Errors is
   function Description (Code : Error_Code) return String is
   begin
      case Code is
         when Ok => return "No error";
         when Cannot_Open_Resource => return "cannot open resource";
         when Unknown_File_Format => return "unknown file format";
         when Invalid_File_Format => return "broken file";
         when Invalid_Version => return "invalid FreeType version";
         when Lower_Module_Version => return "module version is too low";
         when Invalid_Argument => return "invalid argument";
         when Unimplemented_Feature => return "unimplemented feature";
         when Invalid_Table => return "broken table";
         when Invalid_Offset => return "broken offset within table";
         when Array_Too_Large => return "array allocation size too large";
         when Missing_Module => return "missing module";
         when Missing_Property => return "missing property";
         when Invalid_Glyph_Index => return "invalid glyph index";
         when Invalid_Character_Code => return "invalid character code";
         when Invalid_Glyph_Format => return "unsupported glyph image format";
         when Cannot_Render_Glyph => return "cannot render this glyph format";
         when Invalid_Outline => return "invalid outline";
         when Invalid_Composite => return "invalid composite glyph";
         when Too_Many_Hints => return "too many hints";
         when Invalid_Pixel_Size => return "invalid pixel size";
         when Invalid_Handle => return "invalid object handle";
         when Invalid_Library_Handle => return "invalid library handle";
         when Invalid_Driver_Handle => return "invalid module handle";
         when Invalid_Face_Handle => return "invalid face handle";
         when Invalid_Size_Handle => return "invalid size handle";
         when Invalid_Slot_Handle => return "invalid glyph slot handle";
         when Invalid_CharMap_Handle => return "invalid charmap handle";
         when Invalid_Cache_Handle => return "invalid cache manager handle";
         when Invalid_Stream_Handle => return "invalid stream handle";
         when Too_Many_Drivers => return "too many modules";
         when Too_Many_Extensions => return "too many extensions";
         when Out_Of_Memory => return "out of memory";
         when Unlisted_Object => return "unlisted object";
         when Cannot_Open_Stream => return "cannot open stream";
         when Invalid_Stream_Seek => return "invalid stream seek";
         when Invalid_Stream_Skip => return "invalid stream skip";
         when Invalid_Stream_Read => return "invalid stream read";
         when Invalid_Stream_Operation => return "invalid stream operation";
         when Invalid_Frame_Operation => return "invalid frame operation";
         when Nested_Frame_Access => return "nested frame access";
         when Invalid_Frame_Read => return "invalid frame read";
         when Raster_Uninitialized => return "raster uninitialized";
         when Raster_Corrupted => return "raster corrupted";
         when Raster_Overflow => return "raster overflow";
         when Raster_Negative_Height => return "negative height while rastering";
         when Too_Many_Caches => return "too many registered caches";
         when Invalid_Opcode => return "invalid opcode";
         when Too_Few_Arguments => return "too few arguments";
         when Stack_Overflow => return "stack overflow";
         when Code_Overflow => return "code overflow";
         when Bad_Argument => return "bad argument";
         when Divide_By_Zero => return "division by zero";
         when Invalid_Reference => return "invalid reference";
         when Debug_OpCode => return "found debug opcode";
         when ENDF_In_Exec_Stream => return "found ENDF opcode in execution stream";
         when Nested_DEFS => return "nested DEFS";
         when Invalid_CodeRange => return "invalid code range";
         when Execution_Too_Long => return "execution context too long";
         when Too_Many_Function_Defs => return "too many function definitions";
         when Too_Many_Instruction_Defs => return "too many instruction definitions";
         when Table_Missing => return "SFNT font table missing";
         when Horiz_Header_Missing => return "horizontal header (hhea) table missing";
         when Locations_Missing => return "locations (loca) table missing";
         when Name_Table_Missing => return "name table missing";
         when CMap_Table_Missing => return "character map (cmap) table missing";
         when Hmtx_Table_Missing => return "horizontal metrics (htmx) table missing";
         when Post_Table_Missing => return "PostScript (post) table missing";
         when Invalid_Horiz_Metrics => return "invalid horizontal metrics";
         when Invalid_CharMap_Format => return "invalid character map (cmap) format";
         when Invalid_PPem => return "invalid ppem value";
         when Invalid_Vert_Metrics => return "invalid vertical metrics";
         when Could_Not_Find_Context => return "could not find context";
         when Invalid_Post_Table_Format => return "invalid PostScript (post) table format";
         when Invalid_Post_Table => return "invalid PostScript (post) table";
         when DEF_In_Glyf_Bytecode => return "found FDEF or IDEF opcode in glyf bytecode";
         when Missing_Bitmap => return "missing bitmap in strike";
         when Syntax_Error => return "opcode syntax error";
         when Stack_Underflow => return "argument stack underflow";
         when Ignore => return "ignore";
         when No_Unicode_Glyph_Name => return "no Unicode glyph name found";
         when Glyph_Too_Big => return "glyph too big for hinting";
         when Missing_Startfont_Field => return "`STARTFONT' field missing";
         when Missing_Font_Field => return "`FONT' field missing";
         when Missing_Size_Field => return "`SIZE' field missing";
         when Missing_Fontboundingbox_Field => return "`FONTBOUNDINGBOX' field missing";
         when Missing_Chars_Field => return "`CHARS' field missing";
         when Missing_Startchar_Field => return "`STARTCHAR' field missing";
         when Missing_Encoding_Field => return "`ENCODING' field missing";
         when Missing_Bbx_Field => return "`BBX' field missing";
         when Bbx_Too_Big => return "`BBX' too big";
         when Corrupted_Font_Header => return "Font header corrupted or missing fields";
         when Corrupted_Font_Glyphs => return "Font glyphs corrupted or missing fields";
      end case;
   end Description;
end Errors;
