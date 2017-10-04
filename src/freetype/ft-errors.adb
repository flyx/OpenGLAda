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

package body FT.Errors is
   function Description (Code : Error_Code) return String is
   begin
      case Code is
         when Ok => return "No error";
         when Cannot_Open_Resource => return "Cannot open resource";
         when Unknown_File_Format => return "Unknown file format";
         when Invalid_File_Format => return "Broken file";
         when Invalid_Version => return "Invalid FreeType version";
         when Lower_Module_Version => return "Module version is too low";
         when Invalid_Argument => return "Invalid argument";
         when Unimplemented_Feature => return "Unimplemented feature";
         when Invalid_Table => return "Broken table";
         when Invalid_Offset => return "Broken offset within table";
         when Array_Too_Large => return "Array allocation size too large";
         when Missing_Module => return "Missing module";
         when Missing_Property => return "Missing property";
         when Invalid_Glyph_Index => return "Invalid glyph index";
         when Invalid_Character_Code => return "Invalid character code";
         when Invalid_Glyph_Format => return "Unsupported glyph image format";
         when Cannot_Render_Glyph => return "Cannot render this glyph format";
         when Invalid_Outline => return "Invalid outline";
         when Invalid_Composite => return "Invalid composite glyph";
         when Too_Many_Hints => return "Too many hints";
         when Invalid_Pixel_Size => return "Invalid pixel size";
         when Invalid_Handle => return "Invalid object handle";
         when Invalid_Library_Handle => return "Invalid library handle";
         when Invalid_Driver_Handle => return "Invalid module handle";
         when Invalid_Face_Handle => return "Invalid face handle";
         when Invalid_Size_Handle => return "Invalid size handle";
         when Invalid_Slot_Handle => return "Invalid glyph slot handle";
         when Invalid_CharMap_Handle => return "Invalid charmap handle";
         when Invalid_Cache_Handle => return "Invalid cache manager handle";
         when Invalid_Stream_Handle => return "Invalid stream handle";
         when Too_Many_Drivers => return "Too many modules";
         when Too_Many_Extensions => return "Too many extensions";
         when Out_Of_Memory => return "Out of memory";
         when Unlisted_Object => return "Unlisted object";
         when Cannot_Open_Stream => return "Cannot open stream";
         when Invalid_Stream_Seek => return "Invalid stream seek";
         when Invalid_Stream_Skip => return "Invalid stream skip";
         when Invalid_Stream_Read => return "Invalid stream read";
         when Invalid_Stream_Operation => return "Invalid stream operation";
         when Invalid_Frame_Operation => return "Invalid frame operation";
         when Nested_Frame_Access => return "Nested frame access";
         when Invalid_Frame_Read => return "Invalid frame read";
         when Raster_Uninitialized => return "Raster uninitialized";
         when Raster_Corrupted => return "Raster corrupted";
         when Raster_Overflow => return "Raster overflow";
         when Raster_Negative_Height => return "Negative height while rastering";
         when Too_Many_Caches => return "Too many registered caches";
         when Invalid_Opcode => return "Invalid opcode";
         when Too_Few_Arguments => return "Too few arguments";
         when Stack_Overflow => return "Stack overflow";
         when Code_Overflow => return "Code overflow";
         when Bad_Argument => return "Bad argument";
         when Divide_By_Zero => return "Division by zero";
         when Invalid_Reference => return "Invalid reference";
         when Debug_OpCode => return "Found debug opcode";
         when ENDF_In_Exec_Stream => return "Found ENDF opcode in execution stream";
         when Nested_DEFS => return "Nested DEFS";
         when Invalid_CodeRange => return "Invalid code range";
         when Execution_Too_Long => return "Execution context too long";
         when Too_Many_Function_Defs => return "Too many function definitions";
         when Too_Many_Instruction_Defs => return "Too many instruction definitions";
         when Table_Missing => return "SFNT font table missing";
         when Horiz_Header_Missing => return "Horizontal header (hhea) table missing";
         when Locations_Missing => return "Locations (loca) table missing";
         when Name_Table_Missing => return "Name table missing";
         when CMap_Table_Missing => return "Character map (cmap) table missing";
         when Hmtx_Table_Missing => return "Horizontal metrics (htmx) table missing";
         when Post_Table_Missing => return "PostScript (post) table missing";
         when Invalid_Horiz_Metrics => return "Invalid horizontal metrics";
         when Invalid_CharMap_Format => return "Invalid character map (cmap) format";
         when Invalid_PPem => return "Invalid ppem value";
         when Invalid_Vert_Metrics => return "Invalid vertical metrics";
         when Could_Not_Find_Context => return "Could not find context";
         when Invalid_Post_Table_Format => return "Invalid PostScript (post) table format";
         when Invalid_Post_Table => return "Invalid PostScript (post) table";
         when DEF_In_Glyf_Bytecode => return "Found FDEF or IDEF opcode in glyf bytecode";
         when Missing_Bitmap => return "Missing bitmap in strike";
         when Syntax_Error => return "Opcode syntax error";
         when Stack_Underflow => return "Argument stack underflow";
         when Ignore => return "Ignore";
         when No_Unicode_Glyph_Name => return "No Unicode glyph name found";
         when Glyph_Too_Big => return "Glyph too big for hinting";
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
end FT.Errors;
