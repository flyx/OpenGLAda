--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Text_IO;

with GL.Buffers;
with GL.Types.Colors;
with GL.Fixed.Matrix;
with GL.Immediate;
with GL.Objects.Textures.Targets;
with GL.Toggles;
with GL.Types;

with SOIL.Images;

with GL_Test.Display_Backend;


procedure SOIL_Test.Image is
   use GL.Fixed.Matrix;
   use GL.Types;
   use GL.Types.Doubles;

   use Ada.Text_IO;

   Texture : GL.Objects.Textures.Texture;
   Image   : SOIL.Images.Image;
begin
   GL_Test.Display_Backend.Init;

   Image.Load ("../tests/soil/ada2012png-black.jpg");

   Put_Line ("Channels: " & Image.Channels'Img);
   Put_Line ("Width:" & Image.Width'Img);
   Put_Line ("Height:" & Image.Height'Img);

   GL_Test.Display_Backend.Open_Window (Integer (Image.Width / 2),
                                        Integer (Image.Height / 2));

   Image.To_Texture (Texture, Flags => (Invert_Y => True, others => False));

   Projection.Load_Identity;
   Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);

   GL.Buffers.Set_Color_Clear_Value (Colors.Color'(1.0, 0.0, 0.0, 1.0));

   while GL_Test.Display_Backend.Window_Opened loop
      GL.Buffers.Clear ((others => True));

      GL.Toggles.Enable (GL.Toggles.Texture_2D);
      GL.Objects.Textures.Targets.Texture_2D.Bind (Texture);
      declare
         Token : GL.Immediate.Input_Token := GL.Immediate.Start (Quads);
      begin
         GL.Immediate.Set_Texture_Coordinates (Vector2'(0.0, 0.0));
         Token.Add_Vertex (Vector2'(-1.0, -1.0));
         GL.Immediate.Set_Texture_Coordinates (Vector2'(0.0, 1.0));
         Token.Add_Vertex (Vector2'(-1.0,  1.0));
         GL.Immediate.Set_Texture_Coordinates (Vector2'(1.0, 1.0));
         Token.Add_Vertex (Vector2'( 1.0,  1.0));
         GL.Immediate.Set_Texture_Coordinates (Vector2'(1.0, 0.0));
         Token.Add_Vertex (Vector2'( 1.0, -1.0));
      end;

      GL_Test.Display_Backend.Swap_Buffers;

      GL_Test.Display_Backend.Poll_Events;
   end loop;

   GL_Test.Display_Backend.Shutdown;
end SOIL_Test.Image;
