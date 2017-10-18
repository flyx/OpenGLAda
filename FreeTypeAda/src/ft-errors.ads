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

package FT.Errors is
   pragma Preelaborate;

   type Error_Code is
     (Ok, Cannot_Open_Resource, Unknown_File_Format, Invalid_File_Format,
      Invalid_Version, Lower_Module_Version, Invalid_Argument,
      Unimplemented_Feature, Invalid_Table, Invalid_Offset, Array_Too_Large,
      Missing_Module, Missing_Property, Invalid_Glyph_Index,
      Invalid_Character_Code, Invalid_Glyph_Format, Cannot_Render_Glyph,
      Invalid_Outline, Invalid_Composite, Too_Many_Hints, Invalid_Pixel_Size,
      Invalid_Handle, Invalid_Library_Handle, Invalid_Driver_Handle,
      Invalid_Face_Handle, Invalid_Size_Handle, Invalid_Slot_Handle,
      Invalid_CharMap_Handle, Invalid_Cache_Handle, Invalid_Stream_Handle,
      Too_Many_Drivers, Too_Many_Extensions, Out_Of_Memory, Unlisted_Object,
      Cannot_Open_Stream, Invalid_Stream_Seek, Invalid_Stream_Skip,
      Invalid_Stream_Read, Invalid_Stream_Operation, Invalid_Frame_Operation,
      Nested_Frame_Access, Invalid_Frame_Read, Raster_Uninitialized,
      Raster_Corrupted, Raster_Overflow, Raster_Negative_Height,
      Too_Many_Caches, Invalid_Opcode, Too_Few_Arguments, Stack_Overflow,
      Code_Overflow, Bad_Argument, Divide_By_Zero, Invalid_Reference,
      Debug_OpCode, ENDF_In_Exec_Stream, Nested_DEFS, Invalid_CodeRange,
      Execution_Too_Long, Too_Many_Function_Defs, Too_Many_Instruction_Defs,
      Table_Missing, Horiz_Header_Missing, Locations_Missing,
      Name_Table_Missing, CMap_Table_Missing, Hmtx_Table_Missing,
      Post_Table_Missing, Invalid_Horiz_Metrics, Invalid_CharMap_Format,
      Invalid_PPem, Invalid_Vert_Metrics, Could_Not_Find_Context,
      Invalid_Post_Table_Format, Invalid_Post_Table, DEF_In_Glyf_Bytecode,
      Missing_Bitmap, Syntax_Error, Stack_Underflow, Ignore,
      No_Unicode_Glyph_Name, Glyph_Too_Big, Missing_Startfont_Field,
      Missing_Font_Field, Missing_Size_Field, Missing_Fontboundingbox_Field,
      Missing_Chars_Field, Missing_Startchar_Field, Missing_Encoding_Field,
      Missing_Bbx_Field, Bbx_Too_Big, Corrupted_Font_Header,
      Corrupted_Font_Glyphs);

   for Error_Code use
     (Ok                            => 16#00#,
      Cannot_Open_Resource          => 16#01#,
      Unknown_File_Format           => 16#02#,
      Invalid_File_Format           => 16#03#,
      Invalid_Version               => 16#04#,
      Lower_Module_Version          => 16#05#,
      Invalid_Argument              => 16#06#,
      Unimplemented_Feature         => 16#07#,
      Invalid_Table                 => 16#08#,
      Invalid_Offset                => 16#09#,
      Array_Too_Large               => 16#0A#,
      Missing_Module                => 16#0B#,
      Missing_Property              => 16#0C#,
      Invalid_Glyph_Index           => 16#10#,
      Invalid_Character_Code        => 16#11#,
      Invalid_Glyph_Format          => 16#12#,
      Cannot_Render_Glyph           => 16#13#,
      Invalid_Outline               => 16#14#,
      Invalid_Composite             => 16#15#,
      Too_Many_Hints                => 16#16#,
      Invalid_Pixel_Size            => 16#17#,
      Invalid_Handle                => 16#20#,
      Invalid_Library_Handle        => 16#21#,
      Invalid_Driver_Handle         => 16#22#,
      Invalid_Face_Handle           => 16#23#,
      Invalid_Size_Handle           => 16#24#,
      Invalid_Slot_Handle           => 16#25#,
      Invalid_CharMap_Handle        => 16#26#,
      Invalid_Cache_Handle          => 16#27#,
      Invalid_Stream_Handle         => 16#28#,
      Too_Many_Drivers              => 16#30#,
      Too_Many_Extensions           => 16#31#,
      Out_Of_Memory                 => 16#40#,
      Unlisted_Object               => 16#41#,
      Cannot_Open_Stream            => 16#51#,
      Invalid_Stream_Seek           => 16#52#,
      Invalid_Stream_Skip           => 16#53#,
      Invalid_Stream_Read           => 16#54#,
      Invalid_Stream_Operation      => 16#55#,
      Invalid_Frame_Operation       => 16#56#,
      Nested_Frame_Access           => 16#57#,
      Invalid_Frame_Read            => 16#58#,
      Raster_Uninitialized          => 16#60#,
      Raster_Corrupted              => 16#61#,
      Raster_Overflow               => 16#62#,
      Raster_Negative_Height        => 16#63#,
      Too_Many_Caches               => 16#70#,
      Invalid_Opcode                => 16#80#,
      Too_Few_Arguments             => 16#81#,
      Stack_Overflow                => 16#82#,
      Code_Overflow                 => 16#83#,
      Bad_Argument                  => 16#84#,
      Divide_By_Zero                => 16#85#,
      Invalid_Reference             => 16#86#,
      Debug_OpCode                  => 16#87#,
      ENDF_In_Exec_Stream           => 16#88#,
      Nested_DEFS                   => 16#89#,
      Invalid_CodeRange             => 16#8A#,
      Execution_Too_Long            => 16#8B#,
      Too_Many_Function_Defs        => 16#8C#,
      Too_Many_Instruction_Defs     => 16#8D#,
      Table_Missing                 => 16#8E#,
      Horiz_Header_Missing          => 16#8F#,
      Locations_Missing             => 16#90#,
      Name_Table_Missing            => 16#91#,
      CMap_Table_Missing            => 16#92#,
      Hmtx_Table_Missing            => 16#93#,
      Post_Table_Missing            => 16#94#,
      Invalid_Horiz_Metrics         => 16#95#,
      Invalid_CharMap_Format        => 16#96#,
      Invalid_PPem                  => 16#97#,
      Invalid_Vert_Metrics          => 16#98#,
      Could_Not_Find_Context        => 16#99#,
      Invalid_Post_Table_Format     => 16#9A#,
      Invalid_Post_Table            => 16#9B#,
      DEF_In_Glyf_Bytecode          => 16#9C#,
      Missing_Bitmap                => 16#9D#,
      Syntax_Error                  => 16#A0#,
      Stack_Underflow               => 16#A1#,
      Ignore                        => 16#A2#,
      No_Unicode_Glyph_Name         => 16#A3#,
      Glyph_Too_Big                 => 16#A4#,
      Missing_Startfont_Field       => 16#B0#,
      Missing_Font_Field            => 16#B1#,
      Missing_Size_Field            => 16#B2#,
      Missing_Fontboundingbox_Field => 16#B3#,
      Missing_Chars_Field           => 16#B4#,
      Missing_Startchar_Field       => 16#B5#,
      Missing_Encoding_Field        => 16#B6#,
      Missing_Bbx_Field             => 16#B7#,
      Bbx_Too_Big                   => 16#B8#,
      Corrupted_Font_Header         => 16#B9#,
      Corrupted_Font_Glyphs         => 16#BA#);
   for Error_Code'Size use Interfaces.C.int'Size;

   subtype Generic_Errors is Error_Code range Ok .. Missing_Property;
   subtype Glyph_Character_Errors is Error_Code range
     Invalid_Glyph_Index .. Invalid_Pixel_Size;
   subtype Handle_Errors is Error_Code range
     Invalid_Handle .. Invalid_Stream_Handle;
   subtype Driver_Errors is Error_Code range
     Too_Many_Drivers .. Too_Many_Extensions;
   subtype Memory_Errors is Error_Code range Out_Of_Memory .. Unlisted_Object;
   subtype Stream_Errors is Error_Code range
     Cannot_Open_Stream .. Invalid_Frame_Read;
   subtype Raster_Errors is Error_Code range
     Raster_Uninitialized .. Raster_Negative_Height;
   subtype Cache_Errors is Error_Code range Too_Many_Caches .. Too_Many_Caches;
   subtype TrueType_And_SFNT_Errors is Error_Code range
     Invalid_Opcode .. Missing_Bitmap;
   subtype CFF_CID_And_Type_1_Errors is Error_Code range
     Syntax_Error .. Glyph_Too_Big;
   subtype BDF_Errors is Error_Code range
     Missing_Startfont_Field .. Corrupted_Font_Glyphs;

   function Description (Code : Error_Code) return String;
end FT.Errors;
