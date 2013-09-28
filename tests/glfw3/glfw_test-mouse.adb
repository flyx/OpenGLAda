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

with GL.Blending;
with GL.Buffers;
with GL.Types.Colors;
with GL.Fixed.Matrix;
with GL.Immediate;
with GL.Toggles;

with Glfw.Windows.Context;
with Glfw.Input.Mouse;
with Glfw.Input.Keys;
with Glfw.Monitors;

procedure Glfw_Test.Mouse is

   Base_Title : constant String
     := "Click and drag rectangles. Hold mods for coloring. ";

   type Test_Window is new Glfw.Windows.Window with record
      Start_X, Start_Y : GL.Types.Double;
      Color : GL.Types.Colors.Color;
      Redraw : Boolean := False;
   end record;

   overriding
   procedure Init (Object : not null access Test_Window;
                   Width, Height : Glfw.Size;
                   Title   : String;
                   Monitor : Glfw.Monitors.Monitor := Glfw.Monitors.No_Monitor;
                   Share   : access Glfw.Windows.Window'Class := null);

   overriding
   procedure Mouse_Position_Changed (Object : not null access Test_Window;
                                     X, Y   : Glfw.Input.Mouse.Coordinate);

   overriding
   procedure Mouse_Button_Changed (Object  : not null access Test_Window;
                                   Button  : Glfw.Input.Mouse.Button;
                                   State   : Glfw.Input.Button_State;
                                   Mods    : Glfw.Input.Keys.Modifiers);

   procedure Init (Object : not null access Test_Window;
                   Width, Height : Glfw.Size;
                   Title   : String;
                   Monitor : Glfw.Monitors.Monitor := Glfw.Monitors.No_Monitor;
                   Share   : access Glfw.Windows.Window'Class := null) is
      Upcast : Glfw.Windows.Window_Reference
        := Glfw.Windows.Window (Object.all)'Access;
   begin
      Upcast.Init (Width, Height, Title, Monitor, Share);
      Object.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Position);
      Object.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Button);
   end Init;

   procedure Mouse_Position_Changed (Object : not null access Test_Window;
                                     X, Y   : Glfw.Input.Mouse.Coordinate) is
      use GL.Types.Doubles;

      use type Glfw.Input.Button_State;
   begin
      Object.Set_Title (Base_Title & "(" & X'Img & ", " & Y'Img & ")");

      if not Object.Redraw and then
        Object.Mouse_Button_State (0) = Glfw.Input.Pressed then
         GL.Immediate.Set_Color (Object.Color);

         GL.Toggles.Enable (GL.Toggles.Blend);
         GL.Blending.Set_Blend_Func
           (GL.Blending.Src_Alpha, GL.Blending.One_Minus_Src_Alpha);

         declare
            Token : GL.Immediate.Input_Token
              := GL.Immediate.Start (GL.Types.Quads);
         begin
            Token.Add_Vertex (Vector2'(Object.Start_X, Object.Start_Y));
            Token.Add_Vertex (Vector2'(Object.Start_X, GL.Types.Double (Y)));
            Token.Add_Vertex (Vector2'(GL.Types.Double (X), GL.Types.Double (Y)));
            Token.Add_Vertex (Vector2'(GL.Types.Double (X), Object.Start_Y));
         end;
         Object.Redraw := True;
      end if;
   end Mouse_Position_Changed;

   procedure Mouse_Button_Changed (Object  : not null access Test_Window;
                                   Button  : Glfw.Input.Mouse.Button;
                                   State   : Glfw.Input.Button_State;
                                   Mods    : Glfw.Input.Keys.Modifiers) is
      use GL.Types.Colors;

      use type Glfw.Input.Mouse.Button;
      use type Glfw.Input.Button_State;

      Colored : Boolean := False;
      X, Y    : Glfw.Input.Mouse.Coordinate;
   begin
      if Button /= 0 or else State /= Glfw.Input.Pressed then
         return;
      end if;

      if Mods.Shift then
         Object.Color (R) := 1.0;
         Colored := True;
      else
         Object.Color (R) := 0.0;
      end if;
      if Mods.Control then
         Object.Color (G) := 1.0;
         Colored := True;
      else
         Object.Color (G) := 0.0;
      end if;
      if Mods.Alt then
         Object.Color (B) := 1.0;
         Colored := True;
      else
         Object.Color (B) := 0.0;
      end if;
      if Mods.Super then
         Object.Color (A) := 0.5;
      else
         Object.Color (A) := 1.0;
      end if;
      if not Colored then
         Object.Color := (0.1, 0.1, 0.1, Object.Color (A));
      end if;
      Object.Get_Cursor_Pos (X, Y);
      Object.Start_X := GL.Types.Double (X);
      Object.Start_Y := GL.Types.Double (Y);
   end Mouse_Button_Changed;

   My_Window : aliased Test_Window;

   use GL.Fixed.Matrix;
   use GL.Buffers;

   use type GL.Types.Double;
begin
   Glfw.Init;
   Enable_Print_Errors;

   My_Window'Access.Init (800, 600, Base_Title);
   Glfw.Windows.Context.Make_Current (My_Window'Access);

   Projection.Load_Identity;
   Projection.Apply_Orthogonal (0.0, 800.0, 600.0, 0.0, -1.0, 1.0);

   while not My_Window'Access.Should_Close loop
      Clear (Buffer_Bits'(others => True));

      while not My_Window.Redraw and not My_Window'Access.Should_Close loop
         Glfw.Input.Wait_For_Events;
      end loop;
      GL.Flush;
      My_Window.Redraw := False;
      Glfw.Windows.Context.Swap_Buffers (My_Window'Access);
   end loop;

   Glfw.Shutdown;
end Glfw_Test.Mouse;
