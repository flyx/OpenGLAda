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

private with GL.Low_Level;

package GL.Buffers is
   
   type Buffer_Bits is record
      Depth   : Boolean := False;
      Accum   : Boolean := False;
      Stencil : Boolean := False;
      Color   : Boolean := False;
   end record;
   
   type Color_Buffer_Selector is (None, Front_Left, Front_Right, Back_Left, Back_Right,
                                  Front, Back, Left, Right, Front_And_Back, Aux0, Aux1,
                                  Aux2, Aux3);
   
   procedure Clear (Bits : Buffer_Bits);
   
   procedure Enable_Color_Buffers (Selector : Color_Buffer_Selector);
   
private
   for Buffer_Bits use record
      Depth   at 0 range 8 .. 8;
      Accum   at 0 range 9 .. 9;
      Stencil at 0 range 10 .. 10;
      Color   at 0 range 14 .. 14;
   end record;
   for Buffer_Bits'Size use Low_Level.Bitfield'Size;
   
   for Color_Buffer_Selector use (None           => 0,
                                  Front_Left     => 16#0400#,
                                  Front_Right    => 16#0401#,
                                  Back_Left      => 16#0402#,
                                  Back_Right     => 16#0403#,
                                  Front          => 16#0404#,
                                  Back           => 16#0405#,
                                  Left           => 16#0406#,
                                  Right          => 16#0407#,
                                  Front_And_Back => 16#0408#,
                                  Aux0           => 16#0409#,
                                  Aux1           => 16#040A#,
                                  Aux2           => 16#040B#,
                                  Aux3           => 16#040C#);
   for Color_Buffer_Selector'Size use Low_Level.Enum'Size;
end GL.Buffers;