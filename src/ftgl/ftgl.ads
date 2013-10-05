--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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

with GL.Types;

private with Ada.Unchecked_Conversion;
private with Interfaces.C;

package FTGL is
   --pragma Preelaborate;
   
   type Render_Mode is record
      Front, Back, Side : Boolean;
   end record;
   Render_All : constant Render_Mode;
   
   type Text_Alignment is (Left, Center, Right, Justify);
   
   type Bounding_Box_Index is range 1 .. 6;
   type Bounding_Box is array (Bounding_Box_Index) of GL.Types.Single;
   
   type Charset is (None, Adobe_Custom, Adobe_Expert, Adobe_Standard,
                    Apple_Roman, Big5, GB2312, Johab, Adobe_Latin_1,
                    Old_Latin_2, SJIS, MS_Symbol, Unicode, Wansung);
   
   FTGL_Error : exception;
private
   for Render_Mode use record
      Front at 0 range 0 .. 0;
      Back  at 0 range 1 .. 1;
      Side  at 0 range 2 .. 2;
   end record;
   pragma Convention (C_Pass_By_Copy, Render_Mode);
   pragma Warnings (Off);
   for Render_Mode'Size use Interfaces.C.int'Size;
   pragma Warnings (On);
   
   Render_All : constant Render_Mode := (others => True);
   
   for Text_Alignment use (Left => 0, Center => 1, Right => 2, Justify => 3);
   for Text_Alignment'Size use Interfaces.C.int'Size;
   
   pragma Convention (C, Bounding_Box);
   pragma Pack (Bounding_Box);
   
   subtype Raw_Charset is Interfaces.C.char_array (1 .. 4);
   function To_Rep is new Ada.Unchecked_Conversion (Raw_Charset, Integer'Base);
   
   -- ugliest representation clause ever
   -- (the FreeType header does this with preprocessor macros)
   for Charset use (None => 0,
                    MS_Symbol => Character'Pos ('s') * 2**24 +
                                 Character'Pos ('y') * 2**16 +
                                 Character'Pos ('m') * 2**8  +
                                 Character'Pos ('b'),
                    
                    Unicode   => Character'Pos ('u') * 2**24 +
                                 Character'Pos ('n') * 2**16 +
                                 Character'Pos ('i') * 2**8 +
                                 Character'Pos ('c'),
                    
                    SJIS      => Character'Pos ('s') * 2**24 +
                                 Character'Pos ('j') * 2**16 +
                                 Character'Pos ('i') * 2**8 +
                                 Character'Pos ('s'),
                    
                    GB2312    => Character'Pos ('g') * 2**24 +
                                 Character'Pos ('b') * 2**16 +
                                 Character'Pos (' ') * 2**8 +
                                 Character'Pos (' '),
                    
                    Big5      => Character'Pos ('b') * 2**24 +
                                 Character'Pos ('i') * 2**16 +
                                 Character'Pos ('g') * 2**8 +
                                 Character'Pos ('5'),
                    
                    Wansung   => Character'Pos ('w') * 2**24 +
                                 Character'Pos ('a') * 2**16 +
                                 Character'Pos ('n') * 2**8 +
                                 Character'Pos ('s'),
                    
                    Johab     => Character'Pos ('j') * 2**24 +
                                 Character'Pos ('o') * 2**16 +
                                 Character'Pos ('h') * 2**8 +
                                 Character'Pos ('a'),
                    
                    Adobe_Standard => Character'Pos ('A') * 2**24 +
                                      Character'Pos ('D') * 2**16 +
                                      Character'Pos ('O') * 2**8 +
                                      Character'Pos ('B'),
                    
                    Adobe_Expert   => Character'Pos ('A') * 2**24 +
                                      Character'Pos ('D') * 2**16 +
                                      Character'Pos ('B') * 2**8 +
                                      Character'Pos ('E'),
                    
                    Adobe_Custom   => Character'Pos ('A') * 2**24 +
                                      Character'Pos ('D') * 2**16 +
                                      Character'Pos ('B') * 2**8 +
                                      Character'Pos ('C'),
                    
                    Adobe_Latin_1  => Character'Pos ('l') * 2**24 +
                                      Character'Pos ('a') * 2**16 +
                                      Character'Pos ('t') * 2**8 +
                                      Character'Pos ('1'),
                    
                    Old_Latin_2    => Character'Pos ('l') * 2**24 +
                                      Character'Pos ('a') * 2**16 +
                                      Character'Pos ('t') * 2**8 +
                                      Character'Pos ('2'),
                    
                    Apple_Roman    => Character'Pos ('a') * 2**24 +
                                      Character'Pos ('r') * 2**16 +
                                      Character'Pos ('m') * 2**8 +
                                      Character'Pos ('n'));
   for Charset'Size use 32;
end FTGL;
