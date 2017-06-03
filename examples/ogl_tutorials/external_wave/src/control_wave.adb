
with Glfw.Input.Keys;
with Glfw.Input.Mouse;

with Maths;

package body Control_Wave is

--    Mouse_X        : GL.Types.Single;
--    Mouse_Y        : GL.Types.Single;
    Cursor_X       : Glfw.Input.Mouse.Coordinate;
    Cursor_Y       : Glfw.Input.Mouse.Coordinate;

    Pressure       : Vertex_Data.Grid_Array;
    Vel_X          : Vertex_Data.Grid_Array;
    Vel_Y          : Vertex_Data.Grid_Array;
    --      Acc_X          : Vertex_Data.Grid_Array;
    --      Acc_Y          : Vertex_Data.Grid_Array;
    Alpha          : Degree := -Degrees (210.0);
    Beta           : Degree := -Degrees (70.0);
    Zoom           : GL.Types.single := 2.0;

    procedure Check_Mouse (Window : in out Glfw.Windows.Window);
    procedure Check_Scroll (Window : in out Glfw.Windows.Window);

    --  ------------------------------------------------------------------------

    procedure Check_Keyboard (Window : in out Glfw.Windows.Window) is
        use GL.Types;
        use Glfw.Input;
    begin
        if Window'Access.Key_State (Keys.Space) = Pressed then
            Vertex_Data.Initialize_Grid (Pressure, Vel_X, Vel_Y);
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
        Check_Scroll (Window);
    end Check_Input;

    --  ------------------------------------------------------------------------

    procedure Check_Mouse (Window : in out Glfw.Windows.Window) is
        use GL.Types;
        use Glfw.Input;
    begin
        if Window'Access.Mouse_Button_State (Mouse.Left_Button) = Pressed then
            Window'Access.Set_Cursor_Mode (Mouse.Disabled);
            Window'Access.Get_Cursor_Pos (Cursor_X, Cursor_Y);
        elsif Window'Access.Mouse_Button_State (Mouse.Left_Button) = Released then
            Window.Set_Cursor_Mode (Mouse.Normal);
        end if;
    end Check_Mouse;

    --  ------------------------------------------------------------------------

    --      procedure Check_Cursor (Window : in out Glfw.Windows.Window;
    --                             X, Y : in out GL.Types.Single) is
    --          use GL.Types;
    --          use Glfw.Input;
    --      begin
    --          if Window'Access.Get_Input_Mode (Cursor.Disabled) then
    --              Alpha := Alpha + Degree ((X - Single (Cursor_X)) / 10.0);
    --              Beta := Beta + Degree ((Y - Single (Cursor_Y)) / 10.0);
    --          end if;
    --          Cursor_X := Mouse.Coordinate (X);
    --          Cursor_Y := Mouse.Coordinate (Y);
    --      end Check_Cursor;

    --  ------------------------------------------------------------------------

    procedure Check_Scroll (Window : in out Glfw.Windows.Window) is
        use GL.Types;
        use Glfw.Input;
        Scroll_X   : Mouse.Scroll_Offset;
        Scroll_Y   : Mouse.Scroll_Offset;
    begin
        Window'Access.Mouse_Scrolled (Scroll_X, Scroll_Y);
        Zoom := Zoom + Single (Scroll_Y) / 4.0;
        if Zoom < 0.0 then
            Zoom := 0.0;
        end if;
    end Check_Scroll;

    --  ------------------------------------------------------------------------

    procedure Get_Settings (A, B : out Maths.Degree; Z : out GL.Types.single) is
    begin
        A := Alpha;
        B := Beta;
        Z := Zoom;
    end Get_Settings;

    --  ------------------------------------------------------------------------

    procedure Get_Data (Press, VX, VY : out Vertex_Data.Grid_Array) is
    begin
        Press := Pressure;
        VX := Vel_X;
        VY := Vel_Y;
    end Get_Data;

    --  ------------------------------------------------------------------------

end Control_Wave;
