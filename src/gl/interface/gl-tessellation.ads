--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types;

package GL.Tessellation is
   pragma Preelaborate;

   use GL.Types;

   procedure Set_Patch_Vertices (Value : Int);

   procedure Set_Patch_Default_Inner_Level (Values : Single_Array);

   procedure Set_Patch_Default_Outer_Level (Values : Single_Array);
end GL.Tessellation;
