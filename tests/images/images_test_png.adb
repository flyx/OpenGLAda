--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Streams.Stream_IO;

with GL.Blending;
with GL.Buffers;
with GL.Types.Colors;
with GL.Fixed.Matrix;
with GL.Images;
with GL.Immediate;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Toggles;
with GL.Types;

with GL_Test.Display_Backend;

procedure Images_Test_PNG is
   use GL.Fixed.Matrix;
   use GL.Types;
   use GL.Types.Doubles;

   Input_File : Ada.Streams.Stream_IO.File_Type;
   Input_Stream : Ada.Streams.Stream_IO.Stream_Access;

   Texture : GL.Objects.Textures.Texture;
begin
   GL_Test.Display_Backend.Init;
   GL_Test.Display_Backend.Open_Window (1000, 498);
   
   Ada.Streams.Stream_IO.Open (Input_File, Ada.Streams.Stream_IO.In_File, 
     "../tests/images/ada2012-black.png");
   Input_Stream := Ada.Streams.Stream_IO.Stream (Input_File);
   
   GL.Images.Load_Image_To_Texture (Input_Stream.all, Texture, GL.Pixels.RGBA);

   Ada.Streams.Stream_IO.Close (Input_File);

   Projection.Load_Identity;
   Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);

   GL.Buffers.Set_Color_Clear_Value (Colors.Color'(1.0, 0.0, 0.0, 1.0));
   
   GL.Toggles.Enable (GL.Toggles.Blend);
   GL.Blending.Set_Blend_Func
     (GL.Blending.Src_Alpha, GL.Blending.One_Minus_Src_Alpha);

   while GL_Test.Display_Backend.Window_Opened loop
      GL.Buffers.Clear ((others => True));

      GL.Objects.Textures.Set_Active_Unit (0);
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
         Token.Add_Vertex (Vector2'(1.0,  1.0));
         GL.Immediate.Set_Texture_Coordinates (Vector2'(1.0, 0.0));
         Token.Add_Vertex (Vector2'(1.0, -1.0));
      end;
      GL.Toggles.Disable (GL.Toggles.Texture_2D);

      GL_Test.Display_Backend.Swap_Buffers;

      GL_Test.Display_Backend.Poll_Events;
   end loop;

   GL_Test.Display_Backend.Shutdown;
end Images_Test_PNG;
