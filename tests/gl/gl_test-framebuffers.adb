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

with Ada.Text_IO;

with GL.Buffers;         use GL.Buffers;
with GL.Immediate;       use GL.Immediate;
with GL.Fixed.Matrix;    use GL.Fixed.Matrix; use GL.Fixed;
with GL.Fixed.Textures;
with GL.Types.Colors;    use GL.Types;
with GL.Objects.Textures.Targets;
with GL.Objects.Renderbuffers;
with GL.Objects.Framebuffers;
with GL.Pixels;
with GL.Toggles;
with GL.Window;

with GL_Test.Display_Backend;

procedure GL_Test.Framebuffers is
   use GL.Types.Doubles;

   Target_Texture : GL.Objects.Textures.Texture;
begin
   Display_Backend.Init;

   Display_Backend.Open_Window (Width => 500, Height => 500);

   Projection.Load_Identity;
   Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);

   -- draw stuff to a texture
   declare
      use GL.Objects.Textures.Targets;
      use type GL.Objects.Framebuffers.Framebuffer_Status;

      Framebuffer  : GL.Objects.Framebuffers.Framebuffer;
      Depth_Buffer : GL.Objects.Renderbuffers.Renderbuffer;
   begin
      Target_Texture.Initialize_Id;
      Texture_2D.Bind (Target_Texture);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Repeat);
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Repeat);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Nearest);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Nearest);
      Texture_2D.Load_Empty_Texture (0, GL.Pixels.RGB8, 256, 256);

      Framebuffer.Initialize_Id;
      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind (Framebuffer);
      GL.Objects.Framebuffers.Read_And_Draw_Target.Attach_Texture
        (GL.Objects.Framebuffers.Color_Attachment_0, Target_Texture, 0);

      Depth_Buffer.Initialize_Id;
      GL.Objects.Renderbuffers.Active_Renderbuffer.Bind (Depth_Buffer);
      GL.Objects.Renderbuffers.Active_Renderbuffer.Allocate
        (GL.Pixels.Depth_Component24, 256, 256);
      GL.Objects.Framebuffers.Read_And_Draw_Target.Attach_Renderbuffer
        (GL.Objects.Framebuffers.Depth_Attachment, Depth_Buffer);

      if GL.Objects.Framebuffers.Read_And_Draw_Target.Status
        /= GL.Objects.Framebuffers.Complete then
         Ada.Text_IO.Put_Line
           ("Error: Framebuffer status is " &
              GL.Objects.Framebuffers.Read_And_Draw_Target.Status'Img);
         return;
      end if;

      GL.Buffers.Set_Active_Buffer (GL.Buffers.Color_Attachment0);
      GL.Window.Set_Viewport (0, 0, 256, 256);

      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      Clear (Buffer_Bits'(others => True));

      declare
         Token : Input_Token := Start (Quads);
      begin
         Set_Color (Colors.Color'(1.0, 0.0, 0.0, 1.0));
         Token.Add_Vertex (Vector3'(-0.5, -0.5, -0.5));
         Token.Add_Vertex (Vector3'(-0.5, 0.25, -0.5));
         Token.Add_Vertex (Vector3'(0.25, 0.25, -0.5));
         Token.Add_Vertex (Vector3'(0.25, -0.5, -0.5));

         Set_Color (Colors.Color'(0.0, 1.0, 0.0, 1.0));
         Token.Add_Vertex (Vector3'(0.0, -0.75, 0.5));
         Token.Add_Vertex (Vector3'(0.9, -0.75, 0.5));
         Token.Add_Vertex (Vector3'(0.9, 0.0, 0.5));
         Token.Add_Vertex (Vector3'(0.0, 0.0, 0.5));

         Set_Color (Colors.Color'(0.0, 0.0, 1.0, 1.0));
         Token.Add_Vertex (Vector3'(-0.25, -0.25, 0.0));
         Token.Add_Vertex (Vector3'(-0.25, 0.5, 0.0));
         Token.Add_Vertex (Vector3'(0.5, 0.5, 0.0));
         Token.Add_Vertex (Vector3'(0.5, -0.25, 0.0));
      end;

      GL.Finish;

      GL.Objects.Framebuffers.Read_And_Draw_Target.Bind
        (GL.Objects.Framebuffers.Default_Framebuffer);
      GL.Objects.Renderbuffers.Active_Renderbuffer.Bind
        (GL.Objects.Renderbuffers.No_Renderbuffer);
   end;

   GL.Window.Set_Viewport (0, 0, 500, 500);

   while not Display_Backend.Escape_Pressed and
         Display_Backend.Window_Opened loop
      Clear (Buffer_Bits'(others => True));

      GL.Toggles.Enable (GL.Toggles.Texture_2D);

      -- don't let the color affect the texture rendering
      GL.Fixed.Textures.Set_Tex_Function (GL.Fixed.Textures.Decal);

      declare
         Token : Input_Token := Start (Quads);
      begin
         GL.Immediate.Set_Texture_Coordinates (Vector2'(0.0, 0.0));
         Token.Add_Vertex (Vector2'(-1.0, -1.0));
         GL.Immediate.Set_Texture_Coordinates (Vector2'(1.0, 0.0));
         Token.Add_Vertex (Vector2'(1.0, -1.0));
         GL.Immediate.Set_Texture_Coordinates (Vector2'(1.0, 1.0));
         Token.Add_Vertex (Vector2'(1.0, 1.0));
         GL.Immediate.Set_Texture_Coordinates (Vector2'(0.0, 1.0));
         Token.Add_Vertex (Vector2'(-1.0, 1.0));
      end;

      GL.Flush;

      Display_Backend.Swap_Buffers;

      Display_Backend.Poll_Events;
   end loop;

   Display_Backend.Shutdown;

end GL_Test.Framebuffers;
