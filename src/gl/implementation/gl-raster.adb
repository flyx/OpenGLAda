--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;

package body GL.Raster is
   procedure Set_Pos (Position : Vector2) is
   begin
      API.Raster_Pos2 (Position);
   end Set_Pos;

   procedure Set_Pos (Position : Vector3) is
   begin
      API.Raster_Pos3 (Position);
   end Set_Pos;

   procedure Set_Pos (Position : Vector4) is
   begin
      API.Raster_Pos4 (Position);
   end Set_Pos;
end GL.Raster;
