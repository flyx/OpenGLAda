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

with GL.Low_Level;
with GL.Types.Colors;

package GL.Fixed.Textures is
   pragma Preelaborate;

   type Texture_Function is (Add, Blend, Replace, Modulate, Decal, Combine);
   type Combine_Function is (Add, Replace, Modulate, Subtract, Add_Signed,
                             Interpolate, Dot3_RGB, Dot3_RGBA);

   -- needs to be declared here because of following subtype declaration
   for Combine_Function use (Add         => 16#0104#,
                             Replace     => 16#1E01#,
                             Modulate    => 16#2100#,
                             Subtract    => 16#84E7#,
                             Add_Signed  => 16#8574#,
                             Interpolate => 16#8575#,
                             Dot3_RGB    => 16#86AE#,
                             Dot3_RGBA   => 16#86AF#);
   for Combine_Function'Size use Low_Level.Enum'Size;

   subtype Alpha_Combine_Function is Combine_Function range Add .. Interpolate;

   type Source_Kind is (Texture, Constant_Src, Primary_Color, Previous);

   subtype Scaling_Factor is Double range 1.0 .. 4.0;

   type Source_Index is range 0 .. 2;

   procedure Set_Tex_Function (Func : Texture_Function);
   function Tex_Function return Texture_Function;

   procedure Set_RGB_Combine (Func : Combine_Function);
   function RGB_Combine return Combine_Function;

   procedure Set_Alpha_Combine (Func : Alpha_Combine_Function);
   function Alpha_Combine return Alpha_Combine_Function;

   procedure Set_RGB_Source (Source : Source_Kind; Index : Source_Index);
   function RGB_Source (Index : Source_Index) return Source_Kind;

   procedure Set_Alpha_Source (Source : Source_Kind; Index : Source_Index);
   function Alpha_Source (Index : Source_Index) return Source_Kind;

   procedure Set_RGB_Scale (Value : Scaling_Factor);
   function RGB_Scale return Scaling_Factor;

   procedure Set_Alpha_Scale (Value : Scaling_Factor);
   function Alpha_Scale return Scaling_Factor;

   procedure Set_LoD_Bias (Value : Double);
   function LoD_Bias return Double;

   procedure Set_Env_Color (Value : Colors.Color);
   function Env_Color return Colors.Color;

   procedure Toggle_Point_Sprite_Coord_Replace (Enabled : Boolean);
   function Point_Sprite_Coord_Replace return Boolean;

private
   for Texture_Function use (Add      => 16#0104#,
                             Blend    => 16#0BE2#,
                             Replace  => 16#1E01#,
                             Modulate => 16#2100#,
                             Decal    => 16#2101#,
                             Combine  => 16#8570#);
   for Texture_Function'Size use Low_Level.Enum'Size;

   for Source_Kind use (Texture       => 16#1702#,
                        Constant_Src  => 16#8576#,
                        Primary_Color => 16#8577#,
                        Previous      => 16#8578#);
   for Source_Kind'Size use Low_Level.Enum'Size;

end GL.Fixed.Textures;
