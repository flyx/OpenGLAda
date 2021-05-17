--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types;

package GL.Raster is
   -- Obsolete as of OpenGL 3. Supplied here to support some older 3rd party
   -- functionality like Bitmap and Pixmap fonts of FTGL

   use GL.Types;
   use GL.Types.Doubles;

   procedure Set_Pos (Position : Vector2);
   procedure Set_Pos (Position : Vector3);
   procedure Set_Pos (Position : Vector4);
end GL.Raster;
