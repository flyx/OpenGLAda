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

with Ada.Finalization;

with GL.Low_Level;
with GL.Colors;
with GL.Common;

private with GL.Enums.Textures;

package GL.Objects.Textures is

   -----------------------------------------------------------------------------
   --                            Basic Types                                  --
   -----------------------------------------------------------------------------
   
   type Texture_Kind is (Texture_1D, Texture_2D, Texture_3D, Texture_Cube_Map);

   type Minifying_Function is (Nearest, Linear, Nearest_Mipmap_Nearest,
                               Linear_Mipmap_Nearest, Nearest_Mipmap_Linear,
                               Linear_Mipmap_Linear);

   -- has to be defined here because of following subtype declaration.
   for Minifying_Function use (Nearest                => 16#2600#,
                               Linear                 => 16#2601#,
                               Nearest_Mipmap_Nearest => 16#2700#,
                               Linear_Mipmap_Nearest  => 16#2701#,
                               Nearest_Mipmap_Linear  => 16#2702#,
                               Linear_Mipmap_Linear   => 16#2703#);
   for Minifying_Function'Size use Low_Level.Int'Size;

   subtype Magnifying_Function is Minifying_Function range Nearest .. Linear;

   type Wrapping_Mode is (Clamp, Repeat, Clamp_To_Border, Clamp_To_Edge,
                          Mirrored_Repeat);

   subtype Priority is Real range 0.0 .. 1.0;

   type Depth_Mode is (Alpha, Luminance, Intensity);

   -----------------------------------------------------------------------------
   --                          Texture Objects                                --
   -----------------------------------------------------------------------------
   
   type Texture (Kind : Texture_Kind) is
      new GL_Object with private;
   
   overriding procedure Bind (Object : Texture);

   procedure Set_Minifying_Filter (Target : Texture;
                                   Filter : Minifying_Function);
   function Minifying_Filter (Target : Texture) return Minifying_Function;

   procedure Set_Magnifying_Filter (Target : Texture;
                                    Filter : Magnifying_Function);
   function Magnifying_Filter (Target : Texture)
                              return Magnifying_Function;

   procedure Set_Minimum_LoD (Target : Texture; Level : Real);
   function Minimum_LoD (Target : Texture) return Real;

   procedure Set_Maximum_LoD (Target : Texture; Level : Real);
   function Maximum_LoD (Target : Texture) return Real;

   procedure Set_Lowest_Mipmap_Level (Target : Texture; Level : Integer);
   function Lowest_Mipmap_Level (Target : Texture) return Integer;

   procedure Set_Highest_Mipmap_Level (Target : Texture; Level : Integer);
   function Highest_Mipmap_Level (Target : Texture) return Integer;

   procedure Set_X_Wrapping (Target : Texture; Mode : Wrapping_Mode);
   function X_Wrapping (Target : Texture) return Wrapping_Mode;

   procedure Set_Y_Wrapping (Target : Texture; Mode : Wrapping_Mode);
   function Y_Wrapping (Target : Texture) return Wrapping_Mode;

   procedure Set_Z_Wrapping (Target : Texture; Mode : Wrapping_Mode);
   function Z_Wrapping (Target : Texture) return Wrapping_Mode;

   procedure Set_Border_Color (Target : Texture; Color : Colors.Color);
   function Border_Color (Target : Texture) return Colors.Color;

   procedure Set_Texture_Priority (Target : Texture;
                                   Value : Priority);
   function Texture_Priority (Target : Texture) return Priority;

   procedure Toggle_Compare_X_To_Texture (Target  : Texture;
                                          Enabled : Boolean);
   function Compare_X_To_Texture_Enabled (Target : Texture) return Boolean;

   procedure Set_Compare_Function (Target : Texture;
                                   Func : Common.Compare_Function);
   function Compare_Function (Target : Texture)
                             return Common.Compare_Function;

   procedure Set_Depth_Texture_Mode (Target : Texture; Mode : Depth_Mode);
   function Depth_Texture_Mode (Target : Texture) return Depth_Mode;

   procedure Toggle_Mipmap_Autoupdate (Target : Texture; Enabled : Boolean);
   function Mipmap_Autoupdate_Enabled (Target : Texture) return Boolean;

private
   
   procedure Create_Id (Object : in out Texture);
   
   procedure Delete_Id (Object : in out Texture);
      
   for Texture_Kind use (Texture_1D       => 16#0DE0#,
                         Texture_2D       => 16#0DE1#,
                         Texture_3D       => 16#806F#,
                         Texture_Cube_Map => 16#8513#);
   for Texture_Kind'Size use Low_Level.Enum'Size;

   for Wrapping_Mode use (Clamp           => 16#2900#,
                          Repeat          => 16#2901#,
                          Clamp_To_Border => 16#812D#,
                          Clamp_To_Edge   => 16#812F#,
                          Mirrored_Repeat => 16#8370#);
   for Wrapping_Mode'Size use Low_Level.Int'Size;

   for Depth_Mode use (Alpha     => 16#1906#,
                       Luminance => 16#1909#,
                       Intensity => 16#8049#);
   for Depth_Mode'Size use Low_Level.Int'Size;

   type Texture (Kind : Texture_Kind) is
     new GL_Object with null record;
   
end GL.Objects.Textures;
