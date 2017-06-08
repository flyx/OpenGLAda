
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;

with Maths;
with Vertex_Data;

package body Control_Wave is

   --    Mouse_X        : GL.Types.Single;
   --    Mouse_Y        : GL.Types.Single;
--     Cursor_X       : Glfw.Input.Mouse.Coordinate;
--     Cursor_Y       : Glfw.Input.Mouse.Coordinate;
   Cursor_X       : GL.Types.Single;
   Cursor_Y       : GL.Types.Single;

   Alpha          : Degree := -Degrees (210.0);
   Beta           : Degree := -Degrees (70.0);
   Zoom           : GL.Types.single := 2.0;

   procedure Check_Mouse (Window : in out Glfw.Windows.Window);
--     procedure Check_Scroll (Window : in out Glfw.Windows.Window);

   --  ------------------------------------------------------------------------

   procedure Check_Keyboard (Window : in out Glfw.Windows.Window) is
      use GL.Types;
      use Glfw.Input;
   begin
      if Window'Access.Key_State (Keys.Space) = Pressed then
         Vertex_Data.Initialize_Grid;
      elsif Window'Access.Key_State (Keys.Left) = Pressed then
         Alpha := Alpha + 5.0;
      elsif Window'Access.Key_State (Keys.Right) = Pressed then
         Alpha := Alpha - 5.0;
      elsif Window'Access.Key_State (Keys.Up) = Pressed then
         Beta := Beta + 5.0;
      elsif Window'Access.Key_State (Keys.Down) = Pressed then
         Beta := Beta - 5.0;
      elsif Window'Access.Key_State (Keys.Page_Up) = Pressed then
         Zoom := Zoom - 0.25;
         if Zoom < 0.0 then
            Zoom := 0.0;
         end if;
      elsif Window'Access.Key_State (Keys.Page_Down) = Pressed then
         Zoom := Zoom + 0.25;
      end if;

   end Check_Keyboard;

   --  ------------------------------------------------------------------------

   procedure Check_Input (Window : in out Glfw.Windows.Window) is
      use Maths;
   begin
      Check_Keyboard (Window);
      Check_Mouse (Window);
--        Check_Scroll (Window);
   end Check_Input;

   --  ------------------------------------------------------------------------

   procedure Check_Mouse (Window : in out Glfw.Windows.Window) is
      use GL.Types;
      use Glfw.Input;
      X : Glfw.Input.Mouse.Coordinate;
      Y : Glfw.Input.Mouse.Coordinate;
   begin
      if Window'Access.Mouse_Button_State (Mouse.Left_Button) = Pressed then
         Window'Access.Set_Cursor_Mode (Mouse.Disabled);
         Window'Access.Get_Cursor_Pos (X, Y);
         Cursor_X := Single (X);
         Cursor_Y := Single (Y);
      elsif Window'Access.Mouse_Button_State (Mouse.Left_Button) = Released then
         Window.Set_Cursor_Mode (Mouse.Normal);
      end if;
   end Check_Mouse;

   --  ------------------------------------------------------------------------

   procedure Check_Cursor (Window : in out Glfw.Windows.Window) is
      use GL.Types;
      use Glfw.Input.Mouse;
      X_Coord : Coordinate;
      Y_Coord : Coordinate;
      X       : Single;
      Y       : Single;
   begin
      Window'Access.Get_Cursor_Pos (X_Coord, Y_Coord);
      X := Single (X_Coord);
      Y := Single (Y_Coord);
      if Window'Access.Get_Cursor_Mode = Disabled then
         Alpha := Alpha + Degree ((X -  Cursor_X) / 10.0);
         Beta := Beta + Degree ((Y - Cursor_Y) / 10.0);
      end if;
      Cursor_X := X;
      Cursor_Y := Y;
   end Check_Cursor;

   --  ------------------------------------------------------------------------

--     procedure Mouse_Scrolled (Object : not null access Glfw.Windows.Window;
--                                 X, Y   : Glfw.Input.Mouse.Scroll_Offset) is
--        use GL.Types;
--     begin
--        Put_Line ("Mouse_Scrolled called");
--        Zoom := Zoom + Single (Y) / 4.0;
--        if Zoom < 0.0 then
--           Zoom := 0.0;
--        end if;
--     end Mouse_Scrolled;

--     procedure Check_Scroll (Window : in out Glfw.Windows.Window) is
--        use GL.Types;
--        use Glfw.Input;
--        Scroll_X   : Mouse.Scroll_Offset;
--        Scroll_Y   : Mouse.Scroll_Offset;
--     begin
--        Window'Access.Mouse_Scrolled (Scroll_X, Scroll_Y);
--        Zoom := Zoom + Single (Scroll_Y) / 4.0;
--        if Zoom < 0.0 then
--           Zoom := 0.0;
--        end if;
--     end Check_Scroll;

   --  ------------------------------------------------------------------------

   procedure Get_Settings (A, B : out Maths.Degree; Z : out GL.Types.single) is
   begin
      A := Alpha;
      B := Beta;
      Z := Zoom;
   end Get_Settings;

   --  ------------------------------------------------------------------------

end Control_Wave;
