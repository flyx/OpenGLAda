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

with GL.Buffers;         use GL.Buffers;
with GL.Immediate;       use GL.Immediate;
with GL.Fixed.Matrix;    use GL.Fixed.Matrix;
with GL.Types.Colors;    use GL.Types;
use GL.Fixed;

with GL_Test.Display_Backend;

procedure GL_Test.Immediate is
   use GL.Types.Doubles;
begin
   Display_Backend.Init;

   Display_Backend.Open_Window (Width => 500, Height => 500);

   Projection.Load_Identity;
   Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
   
   while not Display_Backend.Escape_Pressed and
         Display_Backend.Window_Opened loop
      Clear (Buffer_Bits'(others => True));
         
      Projection.Push;
      
      for I in 1 .. 12 loop
         declare
            Token : Input_Token := Start (Line_Strip);
         begin
            Set_Color (Colors.Color'(1.0, 0.0, 0.0, 0.0));
            Token.Add_Vertex (Vector4'(0.1, 0.4, 0.0, 1.0));
            Token.Add_Vertex (Vector4'(0.1, 0.6, 0.0, 1.0));
            Token.Add_Vertex (Vector4'(-0.1, 0.6, 0.0, 1.0));
            Token.Add_Vertex (Vector4'(-0.1, 0.4, 0.0, 1.0));
         end;
         Projection.Apply_Rotation (360.0 / 12.0, 0.0, 0.0, 1.0);
      end loop;
      
      Projection.Pop;
      Projection.Apply_Rotation (0.8, 0.0, 0.0, 1.0);

      GL.Flush;

      Display_Backend.Swap_Buffers;
            
      Display_Backend.Poll_Events;
   end loop;

   Display_Backend.Shutdown;

end GL_Test.Immediate;
