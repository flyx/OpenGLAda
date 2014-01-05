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

with Ada.Calendar;

with Interfaces.C.Pointers;

with GL.Buffers;
with GL.Objects.Buffers;
with GL.Fixed;
with GL.Fixed.Matrix;
with GL.Immediate;
with GL.Toggles;
with GL.Types.Colors;

with GL_Test.Display_Backend;

procedure GL_Test.VBOs is
   use GL.Fixed.Matrix;
   use GL.Types.Doubles;
   use GL.Types;
   use GL;
   
   procedure Load_Index is new GL.Objects.Buffers.Load_To_Buffer
     (Int_Pointers);
   
   type Colored_Vertex is record
      Vertex : Vector3;
      Color  : Colors.Color;
   end record;
   for Colored_Vertex'Size use Double'Size * 3 + Single'Size * 4;
   pragma Convention (C, Colored_Vertex);
   pragma Pack (Colored_Vertex);
   
   type Colored_Vertices is array (Size range <>) of aliased Colored_Vertex;
   
   package Colored_Pointers is new Interfaces.C.Pointers
     (Size, Colored_Vertex, Colored_Vertices, Colored_Vertex'(others => <>));
   
   procedure Load_Colored_Vertex is new GL.Objects.Buffers.Load_To_Buffer
      (Colored_Pointers);
      
begin
   Display_Backend.Init;
   Display_Backend.Open_Window (640, 480, Depth_Bits => 24);
   
   Projection.Load_Identity;
   Projection.Apply_Frustum (-2.0, 2.0, -1.5, 1.5, 3.0, 20.0);
      
   GL.Immediate.Set_Color (Colors.Color'(1.0, 0.0, 0.0, 0.0));
   
   declare
      use GL.Objects.Buffers;
      use GL.Buffers;
      
      Cube : constant Colored_Vertices :=
        ((( 0.9,  0.9,  0.9), (0.0, 0.0, 0.0, 0.0)),
         (( 0.9,  0.9, -0.9), (1.0, 0.0, 0.0, 0.0)),
         (( 0.9, -0.9, -0.9), (1.0, 1.0, 0.0, 0.0)),
         (( 0.9, -0.9,  0.9), (0.0, 1.0, 0.0, 0.0)),
         ((-0.9,  0.9,  0.9), (0.0, 1.0, 0.0, 0.0)),
         ((-0.9,  0.9, -0.9), (0.0, 1.0, 1.0, 0.0)),
         ((-0.9, -0.9, -0.9), (1.0, 1.0, 1.0, 0.0)),
         ((-0.9, -0.9,  0.9), (0.0, 1.0, 1.0, 0.0))
        );
        
      Indexes : constant Int_Array :=
        (0, 3, 2, 1,
         4, 5, 6, 7,
         0, 1, 5, 4,
         2, 3, 7, 6,
         0, 3, 7, 4,
         1, 5, 6, 2);
      
      Cube_Buffer, Index_Buffer  : Buffer;
      
      Rotator : Double := 0.0;
      
      Cur_Seconds  : Ada.Calendar.Day_Duration;
      Last_Seconds : Ada.Calendar.Day_Duration := 0.0;
      Frame_Count  : Natural := 0;
   begin
      Cube_Buffer.Initialize_Id;
      Index_Buffer.Initialize_Id;
      
      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      
      Array_Buffer.Bind (Cube_Buffer);
      Load_Colored_Vertex (Array_Buffer, Cube, Static_Draw);
      
      GL.Fixed.Set_Vertex_Pointer (3, 40, 0);
      GL.Fixed.Enable (GL.Fixed.Vertex_Array);
      
      GL.Fixed.Set_Color_Pointer (40, 24);
      GL.Fixed.Enable (GL.Fixed.Color_Array);
      
      Element_Array_Buffer.Bind (Index_Buffer);
      Load_Index (Element_Array_Buffer, Indexes, Static_Draw);
      
      while Display_Backend.Window_Opened loop
         Clear (Buffer_Bits'(Color => True, Depth => True, others => False));
         
         Modelview.Load_Identity;
         Modelview.Apply_Translation (0.5, -1.5, -6.5);
         Modelview.Apply_Rotation (Rotator, 0.0, 1.0, 0.0);
         Rotator := Rotator + 1.0;
         
         GL.Objects.Buffers.Draw_Elements (GL.Types.Quads, 24, UInt_Type);
         
         GL.Flush;
         
         Display_Backend.Swap_Buffers;
         
         Cur_Seconds := Ada.Calendar.Seconds (Ada.Calendar.Clock);
         Frame_Count := Frame_Count + 1;
         
         if Cur_Seconds > Last_Seconds + 1.0 or Cur_Seconds < Last_Seconds then
            Last_Seconds := Cur_Seconds;
            Display_Backend.Set_Window_Title ("FPS:" & Frame_Count'Img);
            Frame_Count := 0;
         end if;
         
         Display_Backend.Poll_Events;
         delay 0.01;
      end loop;
   end;
   
   Display_Backend.Shutdown;
end GL_Test.VBOs;
