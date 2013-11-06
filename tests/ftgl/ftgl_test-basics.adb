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

with Ada.Command_Line;
with Ada.Text_IO;

with GL.Buffers;
with GL.Fixed.Matrix;
with GL.Immediate;
with GL.Raster;
with GL.Toggles;
with GL.Types;

with FTGL.Fonts;

with GL_Test.Display_Backend;

procedure FTGL_Test.Basics is
   use GL.Fixed.Matrix;
   use GL.Types;
   use GL.Types.Doubles;
   
   Bitmap_Font : FTGL.Fonts.Bitmap_Font;
   Pixmap_Font : FTGL.Fonts.Pixmap_Font;
   
   Polygon_Font : FTGL.Fonts.Polygon_Font;
   Outline_Font : FTGL.Fonts.Outline_Font;
   Extrude_Font : FTGL.Fonts.Extrude_Font;
   
   Texture_Font : FTGL.Fonts.Texture_Font;
   Buffer_Font  : FTGL.Fonts.Buffer_Font;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Please give the path to a font file as argument.");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;
   
   GL_Test.Display_Backend.Init;
   GL_Test.Display_Backend.Open_Window (800, 600, 8);
   
   GL.Toggles.Enable (GL.Toggles.Depth_Test);
   
   declare
      Test_Font : constant String := Ada.Command_Line.Argument (1);
      
      procedure Setup_Font (Object : in out FTGL.Fonts.Font'Class) is
      begin
         Object.Load (Test_Font);
         Object.Set_Font_Face_Size (72);
      end Setup_Font;
   begin
      Setup_Font (Bitmap_Font);
      Setup_Font (Pixmap_Font);
      Setup_Font (Polygon_Font);
      Setup_Font (Outline_Font);
      Setup_Font (Extrude_Font);
      Setup_Font (Texture_Font);
      Setup_Font (Buffer_Font);
   exception
      when FTGL.FTGL_Error =>
         Ada.Text_IO.Put_Line ("Could not load font file " & Test_Font);
         Ada.Command_Line.Set_Exit_Status (1);
         return;
   end;
   
   declare
      List : constant FTGL.Fonts.Charset_List := Bitmap_Font.Get_Char_Map_List;
   begin
      Ada.Text_IO.Put_Line ("Charsets:");
      for I in List'Range loop
         Ada.Text_IO.Put_Line (List (I)'Img);
      end loop;
   end;
   
   Extrude_Font.Set_Font_Depth (0.05);
   
   Projection.Load_Identity;
   Projection.Apply_Frustum (0.0, 800.0, 0.0, 600.0, 1.0, 10.0);
   
   while GL_Test.Display_Backend.Window_Opened loop
      GL.Buffers.Clear ((others => True));
      
      GL.Immediate.Set_Color ((1.0, 1.0, 1.0, 1.0));
      
      GL.Raster.Set_Pos (Vector3'(0.0, Double (-Bitmap_Font.Descender), -1.5));
      
      Bitmap_Font.Render ("Bitmap font", (Front => True, others => False));
      
      GL.Raster.Set_Pos
        (Vector3'(0.0, Double (-Pixmap_Font.Descender +
                               Bitmap_Font.Line_Height), -1.5));
      
      Pixmap_Font.Render ("Pixmap font", (Front => True, others => False));
      
      Modelview.Push;
      
      Modelview.Apply_Rotation (0.02, (0.0, 1.0, 0.0));
      
      Modelview.Apply_Translation (0.0, Double (Bitmap_Font.Line_Height * 2.0 +
                                                Pixmap_Font.Line_Height -
                                                Polygon_Font.Descender), -1.5);
      
      Polygon_Font.Render ("Polygon font", (Front => True, others => False));
      
      Modelview.Apply_Translation (0.0, Double (Polygon_Font.Line_Height), 0.0);
      
      Outline_Font.Render ("Outline font", (Front => True, others => False));
      
      Modelview.Apply_Translation (0.0, Double (Outline_Font.Line_Height), 0.0);
      
      Extrude_Font.Render ("Extrude font", (Front => True, others => False));
      
      GL.Immediate.Set_Color ((0.5, 0.5, 0.5, 1.0));
      
      Extrude_Font.Render ("Extrude font", (Side => True, others => False));
      
      GL.Immediate.Set_Color ((1.0, 1.0, 1.0, 1.0));
      
      Modelview.Apply_Translation (0.0, Double (Extrude_Font.Line_Height), 0.0);
      
      Texture_Font.Render ("Texture font", (Front => True, others => False));
      
      Modelview.Apply_Translation (0.0, Double (Texture_Font.Line_Height), 0.0);
      
      Buffer_Font.Render ("Buffer font", (Front => True, others => False));
      
      Modelview.Pop;
      
      GL_Test.Display_Backend.Swap_Buffers;
      
      GL_Test.Display_Backend.Poll_Events;
   end loop;
      
   GL_Test.Display_Backend.Shutdown;
end FTGL_Test.Basics;
