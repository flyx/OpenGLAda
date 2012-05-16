--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

with Glfw.Display;
with Glfw.Events;

with GL.Buffers;
with GL.Colors;
with GL.Common;
with GL.Objects.Buffer;
with GL.Fixed;
with GL.Fixed.Matrix;
with GL.Fixed.Immediate;
with GL.Types;

with Ada.Text_IO;

procedure GL_Test.VBOs is
   use GL.Fixed.Matrix;
   use GL.Types;
   use GL.Types.Doubles;
   use GL;
   
   type Vector_Array is array (Integer range <>) of aliased Vector4;
   type Int_Array is array (Integer range <>) of aliased Int;
   
   procedure Load_Vector is new GL.Objects.Buffer.Load_To_Buffer
     (Element_Type => Vector4, Array_Type => Vector_Array);
   
   procedure Load_Index is new GL.Objects.Buffer.Load_To_Buffer
     (Element_Type => Int, Array_Type => Int_Array);
begin
   Glfw.Init;
   Glfw.Display.Open (Mode => Glfw.Display.Window);
   
   Projection.Load_Identity;
   Projection.Apply_Frustum (-2.0, 2.0, -1.5, 1.5, 3.0, 20.0);
      
   GL.Fixed.Immediate.Set_Color (GL.Colors.Color'(1.0, 0.0, 0.0, 0.0));
   
   declare
      use GL.Objects.Buffer;
      use GL.Buffers;
      
      Cube : constant Vector_Array :=
        (( 0.9,  0.9,  0.9, 1.0),
         ( 0.9,  0.9, -0.9, 1.0),
         ( 0.9, -0.9, -0.9, 1.0),
         ( 0.9, -0.9,  0.9, 1.0),
         (-0.9,  0.9,  0.9, 1.0),
         (-0.9,  0.9, -0.9, 1.0),
         (-0.9, -0.9, -0.9, 1.0),
         (-0.9, -0.9,  0.9, 1.0)
        );
        
      Indexes : constant Int_Array :=
        (0, 1, 2, 3,
         4, 5, 6, 7,
         0, 1, 5, 4,
         2, 3, 7, 6,
         0, 3, 7, 4,
         1, 2, 6, 5);
      
      Cube_Buffer  : Buffer_Object;
      Index_Buffer : Buffer_Object;
      
      Rotator : Double := 0.0;
   begin
      Array_Buffer.Bind (Cube_Buffer);
      Load_Vector (Array_Buffer, Cube, Static_Draw);
      
      GL.Fixed.Set_Vertex_Pointer (4, 0, 0);
      
      GL.Fixed.Enable (GL.Fixed.Vertex_Array);
      
      Element_Array_Buffer.Bind (Index_Buffer);
      Load_Index (Element_Array_Buffer, Indexes, Static_Draw);
      
      GL.Fixed.Set_Index_Pointer (Int_Type, 0, 0);
      
      GL.Fixed.Enable (GL.Fixed.Index_Array);
      
      while Glfw.Display.Opened loop
         Clear (Buffer_Bits'(Color => True, others => False));
         
         Modelview.Load_Identity;
         Modelview.Apply_Translation (0.0, -1.0, -4.5);
         Modelview.Apply_Rotation (Rotator, 0.0, 1.0, 0.0);
         Rotator := Rotator + 1.0;
         
         GL.Fixed.Draw_Elements (GL.Fixed.Quads, 24, UInt_Type);
         
         GL.Flush;
         
         Glfw.Display.Swap_Buffers;
         
         delay 0.1;
         
         Glfw.Events.Poll_Events;
      end loop;
   end;
   
   Glfw.Terminate_Glfw;
end GL_Test.VBOs;