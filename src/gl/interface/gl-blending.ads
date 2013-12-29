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

with GL.Buffers;
with GL.Types.Colors;

private with GL.Low_Level;

package GL.Blending is
   pragma Preelaborate;

   type Blend_Factor is (Zero, One, Src_Color, One_Minus_Src_Color, Src_Alpha,
                         One_Minus_Src_Alpha, Dst_Alpha, One_Minus_Dst_Alpha,
                         Dst_Color, One_Minus_Dst_Color, Src_Alpha_Saturate,
                         Constant_Color, One_Minus_Constant_Color,
                         Constant_Alpha, One_Minus_Constant_Alpha, Src1_Alpha,
                         Src1_Color, One_Minus_Src1_Color,
                         One_Minus_Src1_Alpha);

   type Equation is (Func_Add, Min, Max, Func_Subtract, Func_Reverse_Substract);

   procedure Set_Blend_Func (Src_Factor, Dst_Factor : Blend_Factor);
   procedure Set_Blend_Func (Draw_Buffer : Buffers.Draw_Buffer_Index;
                             Src_Factor, Dst_Factor : Blend_Factor);

   procedure Set_Blend_Func_Separate (Src_RGB, Dst_RGB, Src_Alpha, Dst_Alpha
                                      : Blend_Factor);
   procedure Set_Blend_Func_Separate (Draw_Buffer : Buffers.Draw_Buffer_Index;
                                      Src_RGB, Dst_RGB, Src_Alpha, Dst_Alpha
                                      : Blend_Factor);

   function Blend_Func_Src_RGB return Blend_Factor;
   function Blend_Func_Src_Alpha return Blend_Factor;
   function Blend_Func_Dst_RGB return Blend_Factor;
   function Blend_Func_Dst_Alpha return Blend_Factor;

   procedure Set_Blend_Color (Value : Types.Colors.Color);
   function Blend_Color return Types.Colors.Color;

   procedure Set_Blend_Equation (Value : Equation);
   procedure Set_Blend_Equation (Draw_Buffer : Buffers.Draw_Buffer_Index;
                                 Value : Equation);

   procedure Set_Blend_Equation_Separate (RGB, Alpha : Equation);
   procedure Set_Blend_Equation_Separate
     (Draw_Buffer : Buffers.Draw_Buffer_Index; RGB, Alpha : Equation);

   function Blend_Equation_RGB return Equation;
   function Blend_Equation_Alpha return Equation;

private
   for Blend_Factor use (Zero                     => 0,
                         One                      => 1,
                         Src_Color                => 16#0300#,
                         One_Minus_Src_Color      => 16#0301#,
                         Src_Alpha                => 16#0302#,
                         One_Minus_Src_Alpha      => 16#0303#,
                         Dst_Alpha                => 16#0304#,
                         One_Minus_Dst_Alpha      => 16#0305#,
                         Dst_Color                => 16#0306#,
                         One_Minus_Dst_Color      => 16#0307#,
                         Src_Alpha_Saturate       => 16#0308#,
                         Constant_Color           => 16#8001#,
                         One_Minus_Constant_Color => 16#8002#,
                         Constant_Alpha           => 16#8003#,
                         One_Minus_Constant_Alpha => 16#8004#,
                         Src1_Alpha               => 16#8589#,
                         Src1_Color               => 16#88F9#,
                         One_Minus_Src1_Color     => 16#88FA#,
                         One_Minus_Src1_Alpha     => 16#88FB#);
   for Blend_Factor'Size use Low_Level.Enum'Size;

   for Equation use (Func_Add               => 16#8006#,
                     Min                    => 16#8007#,
                     Max                    => 16#8008#,
                     Func_Subtract          => 16#800A#,
                     Func_Reverse_Substract => 16#800B#);
   for Equation'Size use Low_Level.Enum'Size;
end GL.Blending;
