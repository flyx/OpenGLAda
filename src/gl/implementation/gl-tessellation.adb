--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;
with GL.Enums;

package body GL.Tessellation is

   procedure Set_Patch_Vertices (Value : Int) is
   begin
      API.Set_Patch_Parameter_Int (Enums.Vertices, Value);
   end Set_Patch_Vertices;

   procedure Set_Patch_Default_Inner_Level (Values : Single_Array) is
   begin
      API.Set_Patch_Parameter_Float_Array (Enums.Default_Inner_Level, Values);
   end Set_Patch_Default_Inner_Level;

   procedure Set_Patch_Default_Outer_Level (Values : Single_Array) is
   begin
      API.Set_Patch_Parameter_Float_Array (Enums.Default_Outer_Level, Values);
   end Set_Patch_Default_Outer_Level;
end GL.Tessellation;
