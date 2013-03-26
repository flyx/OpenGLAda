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

with GL.Low_Level.Enums;
with GL.Types.Colors;

private with GL.Enums.Textures;

package GL.Objects.Textures is
   pragma Preelaborate;

   -----------------------------------------------------------------------------
   --                            Basic Types                                  --
   -----------------------------------------------------------------------------

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
   for Minifying_Function'Size use Int'Size;

   subtype Magnifying_Function is Minifying_Function range Nearest .. Linear;

   type Wrapping_Mode is (Clamp, Repeat, Clamp_To_Border, Clamp_To_Edge,
                          Mirrored_Repeat);

   subtype Priority is Double range 0.0 .. 1.0;

   type Depth_Mode is (Alpha, Luminance, Intensity);
   
   type Compare_Function is (Never, Less, Equal, LEqual, Greater, Not_Equal,
                             GEqual, Always);
   
   for Compare_Function use (Never     => 16#0200#,
                             Less      => 16#0201#,
                             Equal     => 16#0202#,
                             LEqual    => 16#0203#,
                             Greater   => 16#0204#,
                             Not_Equal => 16#0205#,
                             GEqual    => 16#0206#,
                             Always    => 16#0207#);
   for Compare_Function'Size use Low_Level.Enum'Size;
   
   -- Actual range is implementation-defined.
   --  OpenGL 2.x: At least 2
   --  OpenGL 3.x: At least 48
   --  OpenGL 4.x: At least 80
   subtype Texture_Unit is Int range 0 .. Int'Last;

   -----------------------------------------------------------------------------
   --                          Texture Targets                                --
   -----------------------------------------------------------------------------
   
   type Texture_Target (<>) is tagged private;

   procedure Set_Minifying_Filter (Target : Texture_Target;
                                   Filter : Minifying_Function);
   function Minifying_Filter (Target : Texture_Target) return Minifying_Function;

   procedure Set_Magnifying_Filter (Target : Texture_Target;
                                    Filter : Magnifying_Function);
   function Magnifying_Filter (Target : Texture_Target)
                              return Magnifying_Function;

   procedure Set_Minimum_LoD (Target : Texture_Target; Level : Double);
   function Minimum_LoD (Target : Texture_Target) return Double;

   procedure Set_Maximum_LoD (Target : Texture_Target; Level : Double);
   function Maximum_LoD (Target : Texture_Target) return Double;

   procedure Set_Lowest_Mipmap_Level (Target : Texture_Target; Level : Integer);
   function Lowest_Mipmap_Level (Target : Texture_Target) return Integer;

   procedure Set_Highest_Mipmap_Level (Target : Texture_Target; Level : Integer);
   function Highest_Mipmap_Level (Target : Texture_Target) return Integer;

   procedure Set_X_Wrapping (Target : Texture_Target; Mode : Wrapping_Mode);
   function X_Wrapping (Target : Texture_Target) return Wrapping_Mode;

   procedure Set_Y_Wrapping (Target : Texture_Target; Mode : Wrapping_Mode);
   function Y_Wrapping (Target : Texture_Target) return Wrapping_Mode;

   procedure Set_Z_Wrapping (Target : Texture_Target; Mode : Wrapping_Mode);
   function Z_Wrapping (Target : Texture_Target) return Wrapping_Mode;

   procedure Set_Border_Color (Target : Texture_Target; Color : Colors.Color);
   function Border_Color (Target : Texture_Target) return Colors.Color;

   procedure Set_Texture_Priority (Target : Texture_Target;
                                   Value : Priority);
   function Texture_Priority (Target : Texture_Target) return Priority;

   procedure Toggle_Compare_X_To_Texture (Target  : Texture_Target;
                                          Enabled : Boolean);
   function Compare_X_To_Texture_Enabled (Target : Texture_Target) return Boolean;

   procedure Set_Compare_Function (Target : Texture_Target;
                                   Func : Compare_Function);
   function Current_Compare_Function (Target : Texture_Target)
                                     return Compare_Function;

   procedure Set_Depth_Texture_Mode (Target : Texture_Target; Mode : Depth_Mode);
   function Depth_Texture_Mode (Target : Texture_Target) return Depth_Mode;

   procedure Toggle_Mipmap_Autoupdate (Target : Texture_Target; Enabled : Boolean);
   function Mipmap_Autoupdate_Enabled (Target : Texture_Target) return Boolean;
   
   -----------------------------------------------------------------------------
   --                          Texture Objects                                --
   -----------------------------------------------------------------------------
   
   type Texture is new GL_Object with private;
   
   procedure Bind (Target : Texture_Target; Object : Texture'Class);
   
   function Current_Texture (Target : Texture_Target) return Texture'Class;
   
   overriding
   procedure Initialize_Id (Object : in out Texture);
   
   overriding
   procedure Delete_Id (Object : in out Texture);
   
   -----------------------------------------------------------------------------
   --                      Texture Target Instances                           --
   -----------------------------------------------------------------------------
   
   Texture_1D       : constant Texture_Target;
   Texture_2D       : constant Texture_Target;
   Texture_3D       : constant Texture_Target;
   Texture_Cube_Map : constant Texture_Target;
   
   -----------------------------------------------------------------------------
   --                            Texture Units                                --
   -----------------------------------------------------------------------------
   
   procedure Set_Active_Unit (Unit : Texture_Unit);
   function Active_Unit return Texture_Unit;
   
   function Texture_Unit_Count return Natural;
   
private

   for Wrapping_Mode use (Clamp           => 16#2900#,
                          Repeat          => 16#2901#,
                          Clamp_To_Border => 16#812D#,
                          Clamp_To_Edge   => 16#812F#,
                          Mirrored_Repeat => 16#8370#);
   for Wrapping_Mode'Size use Int'Size;

   for Depth_Mode use (Alpha     => 16#1906#,
                       Luminance => 16#1909#,
                       Intensity => 16#8049#);
   for Depth_Mode'Size use Int'Size;
   
   type Texture_Target (Kind : Low_Level.Enums.Texture_Kind) is tagged null record;
   
   Texture_1D       : constant Texture_Target
     := Texture_Target'(Kind => Low_Level.Enums.Texture_1D);
   Texture_2D       : constant Texture_Target
     := Texture_Target'(Kind => Low_Level.Enums.Texture_2D);
   Texture_3D       : constant Texture_Target
     := Texture_Target'(Kind => Low_Level.Enums.Texture_3D);
   Texture_Cube_Map : constant Texture_Target
     := Texture_Target'(Kind => Low_Level.Enums.Texture_Cube_Map);

   type Texture is new GL_Object with null record;
   
end GL.Objects.Textures;
