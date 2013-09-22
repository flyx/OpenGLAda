--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <flyx@isobeef.org>
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
with Ada.Numerics.Generic_Elementary_Functions;

with GL.Buffers;         use GL.Buffers;
with GL.Immediate;       use GL.Immediate;
with GL.Fixed.Matrix;    use GL.Fixed.Matrix;
with GL.Types.Colors;    use GL.Types;
use GL.Fixed;

with Glfw.Display;
with Glfw.Events.Keys;
with Glfw.Events.Joysticks;

procedure GLFW_Test.Joystick is
   package Double_Math is new Ada.Numerics.Generic_Elementary_Functions (Double);
   
   Joystick : GLFW.Events.Joysticks.Joystick;
   use GL.Types.Doubles;
begin
   Glfw.Init;
   
   if not Joystick.Present then
      Ada.Text_IO.Put_Line ("No joystick present!");
      Glfw.Terminate_Glfw;
      return;
   end if;
   
   Glfw.Display.Open (Mode => Glfw.Display.Window);
   
   Projection.Load_Identity;
   Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
   
   declare
      X_Offset, Y_Offset : Glfw.Events.Joysticks.Axis_Position := 0.0;
      Rotation : Double := 0.0;
      Zoom : Double := 1.0;
      States : Glfw.Events.Joysticks.Axis_Positions (1 .. Joystick.Num_Axis);
   begin
   
      while not Glfw.Events.Keys.Pressed (Glfw.Events.Keys.Esc) and
            Glfw.Display.Opened loop
         Joystick.Get_Positions (States);
         if States'Length > 0 then
            X_Offset := States(1);
         end if;
         if States'Length > 1 then
            Y_Offset := States(2);
         end if;
         if States'Length > 2 then
            Rotation := Double (States(3)) * 90.0;
         end if;
         if States'Length > 3 then
            Zoom := Double_Math.Exp (Double(States(4)) + 0.5);
         end if;
         
         Clear (Buffer_Bits'(others => True));
            
         Projection.Push;
         Projection.Apply_Translation (Double (X_Offset), Double (Y_Offset), 0.0);  
         Projection.Apply_Rotation (Rotation, 0.0, 0.0, 1.0);
         Projection.Apply_Scaling (Zoom, Zoom, 1.0);
      
         declare
            Token : Input_Token := Start (Quads);
         begin
            Set_Color (Colors.Color'(0.0, 0.0, 1.0, 0.0));
            Token.Add_Vertex (Vector4'(0.1, 0.1, 0.0, 1.0));
            Token.Add_Vertex (Vector4'(0.1, -0.1, 0.0, 1.0));
            Token.Add_Vertex (Vector4'(-0.1, -0.1, 0.0, 1.0));
            Token.Add_Vertex (Vector4'(-0.1, 0.1, 0.0, 1.0));
         end;
      
         Projection.Pop;
      
         GL.Flush;
      
         Glfw.Display.Swap_Buffers;
               
         Glfw.Events.Poll_Events;
      end loop;
   end;
   
   Glfw.Terminate_Glfw;
end GLFW_Test.Joystick;