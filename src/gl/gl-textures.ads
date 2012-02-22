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
with GL.Low_Level.Enums;
with GL.Colors;
with GL.Common;

private with GL.Enums.Textures;

package GL.Textures is

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
   for Minifying_Function'Size use Low_Level.Int'Size;

   subtype Magnifying_Function is Minifying_Function range Nearest .. Linear;

   type Wrapping_Mode is (Clamp, Repeat, Clamp_To_Border, Clamp_To_Edge,
                          Mirrored_Repeat);

   subtype Priority is Real range 0.0 .. 1.0;

   type Depth_Mode is (Alpha, Luminance, Intensity);

   -----------------------------------------------------------------------------
   --                            Texture IDs                                  --
   -----------------------------------------------------------------------------

   -- Pointer to a texture. Will be reference-counted and automatically destroyed.
   type Texture_Id is abstract new Ada.Finalization.Controlled with private;

   -- Increases reference count.
   overriding procedure Adjust (Id : in out Texture_Id);

   -- Decreases reference count. Destroys texture when it reaches zero.
   overriding procedure Finalize (Id : in out Texture_Id);
   
   -- Low level access
   function Raw_Id (Id : Texture_Id) return Low_Level.UInt;

   type Tex_1D_Id is new Texture_Id with private;
   type Tex_2D_Id is new Texture_Id with private;
   type Tex_3D_Id is new Texture_Id with private;
   type Tex_Cube_Map_Id is new Texture_Id with private;

   overriding procedure Initialize (Id : in out Tex_1D_id);
   overriding procedure Initialize (Id : in out Tex_2D_id);
   overriding procedure Initialize (Id : in out Tex_3D_id);
   overriding procedure Initialize (Id : in out Tex_Cube_Map_id);

   -----------------------------------------------------------------------------
   --                          Texture Targets                                --
   -----------------------------------------------------------------------------

   type Texture_Target (<>) is abstract tagged private;

   procedure Set_Minifying_Filter (Target : Texture_Target;
                                   Filter : Minifying_Function);
   function Minifying_Filter (Target : Texture_Target) return Minifying_Function;

   procedure Set_Magnifying_Filter (Target : Texture_Target;
                                    Filter : Magnifying_Function);
   function Magnifying_Filter (Target : Texture_Target)
                              return Magnifying_Function;

   procedure Set_Minimum_LoD (Target : Texture_Target; Level : Real);
   function Minimum_LoD (Target : Texture_Target) return Real;

   procedure Set_Maximum_LoD (Target : Texture_Target; Level : Real);
   function Maximum_LoD (Target : Texture_Target) return Real;

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
                                   Func : Common.Compare_Function);
   function Compare_Function (Target : Texture_Target)
                             return Common.Compare_Function;

   procedure Set_Depth_Texture_Mode (Target : Texture_Target; Mode : Depth_Mode);
   function Depth_Texture_Mode (Target : Texture_Target) return Depth_Mode;

   procedure Toggle_Mipmap_Autoupdate (Target : Texture_Target; Enabled : Boolean);
   function Mipmap_Autoupdate_Enabled (Target : Texture_Target) return Boolean;

   -----------------------------------------------------------------------------
   --                      Texture Target Instances                           --
   -----------------------------------------------------------------------------

   type Tex_1D_Target       (<>) is new Texture_Target with private;
   type Tex_2D_Target       (<>) is new Texture_Target with private;
   type Tex_3D_Target       (<>) is new Texture_Target with private;
   type Tex_Cube_Map_Target (<>) is new Texture_Target with private;

   procedure Bind (Target : Tex_1D_Target; Id : Tex_1D_Id'Class);
   procedure Bind (Target : Tex_2D_Target; Id : Tex_2D_Id'Class);
   procedure Bind (Target : Tex_3D_Target; Id : Tex_3D_Id'Class);
   procedure Bind (Target : Tex_Cube_Map_Target; Id : Tex_Cube_Map_Id'Class);


   Texture_1D       : constant Tex_1D_Target;
   Texture_2D       : constant Tex_2D_Target;
   Texture_3D       : constant Tex_3D_Target;
   Texture_Cube_Map : constant Tex_Cube_Map_Target;

private
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

   type Reference_Counted_Texture is limited record
      GL_Id           : Low_Level.UInt;
      Reference_Count : Natural;
   end record;

   type Texture_Id is abstract
     new Ada.Finalization.Controlled with record
      Kind      : Low_Level.Enums.Texture_Kind;
      Reference : access Reference_Counted_Texture;
   end record;

   type Tex_1D_Id       is new Texture_Id with null record;
   type Tex_2D_Id       is new Texture_Id with null record;
   type Tex_3D_Id       is new Texture_Id with null record;
   type Tex_Cube_Map_Id is new Texture_Id with null record;

   type Texture_Target (Kind : Low_Level.Enums.Texture_Kind) is tagged null record;
   type Tex_1D_Target
     is new Texture_Target (Kind => Low_Level.Enums.TK_1D) with null record;
   type Tex_2D_Target
     is new Texture_Target (Kind => Low_Level.Enums.TK_2D) with null record;
   type Tex_3D_Target
     is new Texture_Target (Kind => Low_Level.Enums.TK_3D) with null record;
   type Tex_Cube_Map_Target
     is new Texture_Target (Kind => Low_Level.Enums.TK_Cube_Map) with null record;

   Texture_1D       : constant Tex_1D_Target
     := Tex_1D_Target'(Kind => Low_Level.Enums.TK_1D);
   Texture_2D       : constant Tex_2D_Target
     := Tex_2D_Target'(Kind => Low_Level.Enums.TK_2D);
   Texture_3D       : constant Tex_3D_Target
     := Tex_3D_Target'(Kind => Low_Level.Enums.TK_3D);
   Texture_Cube_Map : constant Tex_Cube_Map_Target
     := Tex_Cube_Map_Target'(Kind => Low_Level.Enums.TK_Cube_Map);

end GL.Textures;
