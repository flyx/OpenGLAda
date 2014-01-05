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

with GL.Buffers;
with GL.Files;
with GL.Fixed.Matrix;
with GL.Immediate;
with GL.Objects.Programs;
with GL.Objects.Shaders.Lists;
with GL.Types.Colors;

with GL_Test.Display_Backend;

procedure GL_Test.Shaders is
   use GL.Buffers;
   use GL.Types;
   use GL.Fixed.Matrix;
   use GL.Immediate;
   
   Vertex_Shader   : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Vertex_Shader);
   Fragment_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Fragment_Shader);
   Program         : GL.Objects.Programs.Program;
begin
   Display_Backend.Init;
   Display_Backend.Open_Window (Width => 500, Height => 500);
   
   Vertex_Shader.Initialize_Id;
   Fragment_Shader.Initialize_Id;
   Program.Initialize_Id;
   
   -- load shader sources and compile shaders
   
   GL.Files.Load_Shader_Source_From_File
     (Vertex_Shader, "../tests/gl/gl_test-shaders-vertex.glsl");
   GL.Files.Load_Shader_Source_From_File
     (Fragment_Shader, "../tests/gl/gl_test-shaders-fragment.glsl");
   
   Vertex_Shader.Compile;
   Fragment_Shader.Compile;
   
   if not Vertex_Shader.Compile_Status then
      Ada.Text_IO.Put_Line ("Compilation of vertex shader failed. log:");
      Ada.Text_IO.Put_Line (Vertex_Shader.Info_Log);
   end if;
   if not Fragment_Shader.Compile_Status then
      Ada.Text_IO.Put_Line ("Compilation of fragment shader failed. log:");
      Ada.Text_IO.Put_Line (Fragment_Shader.Info_Log);
   end if;
   
   -- set up program
   Program.Attach (Vertex_Shader);
   Program.Attach (Fragment_Shader);
   Program.Link;
   if not Program.Link_Status then
      Ada.Text_IO.Put_Line ("Program linking failed. Log:");
      Ada.Text_IO.Put_Line (Program.Info_Log);
      return;
   end if;
   Program.Use_Program;
   
   -- test iteration over program shaders
   Ada.Text_IO.Put_Line ("Listing shaders attached to program...");
   declare
      use type GL.Objects.Shaders.Lists.Cursor;
      
      List : constant GL.Objects.Shaders.Lists.List
        := Program.Attached_Shaders;
      Cursor : GL.Objects.Shaders.Lists.Cursor := List.First;
   begin
      while Cursor /= GL.Objects.Shaders.Lists.No_Element loop
         declare
            Shader : constant GL.Objects.Shaders.Shader
              := GL.Objects.Shaders.Lists.Element (Cursor);
         begin
            Ada.Text_IO.Put_Line ("----------------------------");
            Ada.Text_IO.Put_Line ("Kind: " & Shader.Kind'Img);
            Ada.Text_IO.Put_Line ("Status: " & Shader.Compile_Status'Img);
         end;
         Cursor := GL.Objects.Shaders.Lists.Next (Cursor);
      end loop;
   end;
   Ada.Text_IO.Put_Line ("-----------[Done.]----------");
   
   -- set up matrices
   Projection.Load_Identity;
   Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
   Modelview.Load_Identity;
   
   while Display_Backend.Window_Opened loop
      Clear (Buffer_Bits'(Color => True, others => False));
               
      declare
         Token : Input_Token := Start (Quads);
      begin
         Set_Color (Colors.Color'(1.0, 0.0, 0.0, 0.0));
         Token.Add_Vertex (Doubles.Vector4'(0.7, 0.7, 0.0, 1.0));
         Set_Color (Colors.Color'(0.0, 1.0, 0.0, 0.0));
         Token.Add_Vertex (Doubles.Vector4'(0.7, -0.7, 0.0, 1.0));
         Set_Color (Colors.Color'(0.0, 0.0, 1.0, 0.0));
         Token.Add_Vertex (Doubles.Vector4'(-0.7, -0.7, 0.0, 1.0));
         Set_Color (Colors.Color'(1.0, 0.0, 1.0, 0.0));
         Token.Add_Vertex (Doubles.Vector4'(-0.7, 0.7, 0.0, 1.0));
      end;
      
      Modelview.Apply_Rotation (0.8, 0.0, 0.0, 1.0);
      
      GL.Flush;
      Display_Backend.Swap_Buffers;
      
      Display_Backend.Poll_Events;
   end loop;
      
   Display_Backend.Shutdown;
end GL_Test.Shaders;
