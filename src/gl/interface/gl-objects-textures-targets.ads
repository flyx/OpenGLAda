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

with GL.Objects.Textures.With_1D_Loader;
with GL.Objects.Textures.With_2D_Loader;
with GL.Objects.Textures.With_3D_Loader;

package GL.Objects.Textures.Targets is
   pragma Preelaborate;

   -----------------------------------------------------------------------------
   --                   Texture Target Specializations                        --
   -----------------------------------------------------------------------------

   package Texture_1D_Target is new With_1D_Loader (Texture_Target);
   package Texture_2D_Target is new With_2D_Loader (Texture_Target);
   package Texture_3D_Target is new With_3D_Loader (Texture_Target);

   package Texture_1D_Proxy_Target is new With_1D_Loader (Texture_Proxy);
   package Texture_2D_Proxy_Target is new With_2D_Loader (Texture_Proxy);
   package Texture_3D_Proxy_Target is new With_3D_Loader (Texture_Proxy);

   type Texture_Cube_Map_Target (<>) is new Texture_3D_Target.Target
     with private;

   package Cube_Map_Side_Target is new With_2D_Loader (Texture_Proxy);

   type Texture_Buffer_Target (<>) is new Texture_Target with private;

   function Buffer_Offset (Object : Texture_Buffer_Target; Level : Mipmap_Level)
                           return Size;
   function Buffer_Size (Object : Texture_Buffer_Target; Level : Mipmap_Level)
                         return Size;

   -----------------------------------------------------------------------------
   --                      Texture Target Instances                           --
   -----------------------------------------------------------------------------


   Texture_1D       : aliased constant Texture_1D_Target.Fillable_Target;
   Texture_2D       : aliased constant Texture_2D_Target.Fillable_Target;
   Texture_3D       : aliased constant Texture_3D_Target.Fillable_Target;

   Texture_1D_Proxy : aliased constant Texture_1D_Proxy_Target.Target;
   Texture_2D_Proxy : aliased constant Texture_2D_Proxy_Target.Target;
   Texture_3D_Proxy : aliased constant Texture_3D_Proxy_Target.Target;

   Texture_Cube_Map : aliased constant Texture_Cube_Map_Target;
   Texture_Cube_Map_Positive_X :
     aliased constant Cube_Map_Side_Target.Fillable_Target;
   Texture_Cube_Map_Negative_X :
     aliased constant Cube_Map_Side_Target.Fillable_Target;
   Texture_Cube_Map_Positive_Y :
     aliased constant Cube_Map_Side_Target.Fillable_Target;
   Texture_Cube_Map_Negative_Y :
     aliased constant Cube_Map_Side_Target.Fillable_Target;
   Texture_Cube_Map_Positive_Z :
     aliased constant Cube_Map_Side_Target.Fillable_Target;
   Texture_Cube_Map_Negative_Z :
     aliased constant Cube_Map_Side_Target.Fillable_Target;
   Texture_Cube_Map_Proxy : aliased constant  Texture_2D_Proxy_Target.Target;

   Texture_Buffer : aliased constant Texture_Buffer_Target;

   function Target_From_Kind (Kind : Low_Level.Enums.Texture_Kind)
     return not null access constant Texture_Proxy'Class;

private
   type Texture_Cube_Map_Target is new Texture_3D_Target.Target
     with null record;

   type Texture_Buffer_Target is new Texture_Target with null record;

   Texture_1D       : aliased constant Texture_1D_Target.Fillable_Target
     := Texture_1D_Target.Fillable_Target'
       (Texture_Target'(Kind => Low_Level.Enums.Texture_1D) with null record);
   Texture_2D       : aliased constant Texture_2D_Target.Fillable_Target
     := Texture_2D_Target.Fillable_Target'
       (Texture_Target'(Kind => Low_Level.Enums.Texture_2D) with null record);
   Texture_3D       : aliased constant Texture_3D_Target.Fillable_Target
     := Texture_3D_Target.Fillable_Target'
       (Texture_Target'(Kind => Low_Level.Enums.Texture_3D) with null record);

   Texture_1D_Proxy : aliased constant Texture_1D_Proxy_Target.Target
     := Texture_1D_Proxy_Target.Target'
       (Texture_Proxy'(Kind => Low_Level.Enums.Proxy_Texture_1D)
        with null record);
   Texture_2D_Proxy : aliased constant Texture_2D_Proxy_Target.Target
     := Texture_2D_Proxy_Target.Target'
       (Texture_Proxy'(Kind => Low_Level.Enums.Proxy_Texture_2D)
        with null record);
   Texture_3D_Proxy : aliased constant Texture_3D_Proxy_Target.Target
     := Texture_3D_Proxy_Target.Target'
       (Texture_Proxy'(Kind => Low_Level.Enums.Proxy_Texture_3D)
        with null record);

   Texture_Cube_Map : aliased constant Texture_Cube_Map_Target
     := Texture_Cube_Map_Target'
       (Texture_Target'(Kind => Low_Level.Enums.Texture_Cube_Map)
        with null record);
   Texture_Cube_Map_Positive_X :
     aliased constant Cube_Map_Side_Target.Fillable_Target
     := Cube_Map_Side_Target.Fillable_Target'
       (Texture_Proxy'(Kind => Low_Level.Enums.Texture_Cube_Map_Positive_X)
        with null record);
   Texture_Cube_Map_Negative_X :
     aliased constant Cube_Map_Side_Target.Fillable_Target
     := Cube_Map_Side_Target.Fillable_Target'
       (Texture_Proxy'(Kind => Low_Level.Enums.Texture_Cube_Map_Negative_X)
        with null record);
   Texture_Cube_Map_Positive_Y :
     aliased constant Cube_Map_Side_Target.Fillable_Target
     := Cube_Map_Side_Target.Fillable_Target'
       (Texture_Proxy'(Kind => Low_Level.Enums.Texture_Cube_Map_Positive_Y)
        with null record);
   Texture_Cube_Map_Negative_Y :
     aliased constant Cube_Map_Side_Target.Fillable_Target
     := Cube_Map_Side_Target.Fillable_Target'
       (Texture_Proxy'(Kind => Low_Level.Enums.Texture_Cube_Map_Negative_Y)
        with null record);
   Texture_Cube_Map_Positive_Z :
     aliased constant Cube_Map_Side_Target.Fillable_Target
     := Cube_Map_Side_Target.Fillable_Target'
       (Texture_Proxy'(Kind => Low_Level.Enums.Texture_Cube_Map_Positive_Z)
        with null record);
   Texture_Cube_Map_Negative_Z :
     aliased constant Cube_Map_Side_Target.Fillable_Target
     := Cube_Map_Side_Target.Fillable_Target'
       (Texture_Proxy'(Kind => Low_Level.Enums.Texture_Cube_Map_Negative_Z)
        with null record);

   Texture_Cube_Map_Proxy : aliased constant  Texture_2D_Proxy_Target.Target
     := Texture_2D_Proxy_Target.Target'
       (Texture_Proxy'(Kind => Low_Level.Enums.Proxy_Texture_Cube_Map)
        with null record);

   Texture_Buffer   : aliased constant Texture_Buffer_Target
     := Texture_Buffer_Target'
       (Texture_Target'(Kind => Low_Level.Enums.Texture_Buffer)
        with null record);


end GL.Objects.Textures.Targets;
