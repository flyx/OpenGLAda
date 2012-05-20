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

with GL.Low_Level;
with GL.Types.Colors;

package GL.Buffers is
   pragma Preelaborate;
   
   use GL.Types;
   
   type Buffer_Bits is record
      Depth   : Boolean := False;
      Accum   : Boolean := False;
      Stencil : Boolean := False;
      Color   : Boolean := False;
   end record;
   
   subtype Depth is Double range 0.0 .. 1.0;
   
   subtype Stencil_Index is Int;
   
   -- Aux buffer support was dropped with OpenGL 3
   type Color_Buffer_Selector is (None, Front_Left,
                                  Front_Right, Back_Left, Back_Right,
                                  Front, Back, Left, Right, Front_And_Back,
                                  Aux0, Aux1, Aux2, Aux3);
   
   -- defined here because of following subtype declaration
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
   
   subtype Base_Color_Buffer_Selector is Color_Buffer_Selector
      range Front .. Front_And_Back;
   
   procedure Clear (Bits : Buffer_Bits);
   
   procedure Set_Active_Buffer (Selector : Color_Buffer_Selector);
   
   procedure Set_Color_Clear_Value (Value : Colors.Color);
   function Color_Clear_Value return Colors.Color;
   
   procedure Set_Depth_Clear_Value (Value : Depth);
   function Depth_Clear_Value return Depth;
   
   procedure Set_Stencil_Clear_Value (Value : Stencil_Index);
   function Stencil_Clear_Value return Stencil_Index;
   
   -- dropped in OpenGL 3
   procedure Set_Accum_Clear_Value (Value : Colors.Color);
   function Accum_Clear_Value return Colors.Color;
   
   -- The following procedures are available since OpenGL 3.0
   
   procedure Clear_Color_Buffer (Selector : Base_Color_Buffer_Selector;
                                 Value    : Colors.Color);
   
   procedure Clear_Depth_Buffer (Value : Depth);
   
   procedure Clear_Stencil_Buffer (Value : Stencil_Index);
   
   procedure Clear_Depth_And_Stencil_Buffer (Depth_Value   : Depth;
                                             Stencil_Value : Stencil_Index);
   
private
   for Buffer_Bits use record
      Depth   at 0 range 8 .. 8;
      Accum   at 0 range 9 .. 9;
      Stencil at 0 range 10 .. 10;
      Color   at 0 range 14 .. 14;
   end record;
   for Buffer_Bits'Size use Low_Level.Bitfield'Size;
   
   
end GL.Buffers;