
with Glfw.Input.Keys;
with Glfw.Input.Mouse;

package body Control_Wave is

    Cursor_X       : GL.Types.Single;
    Cursor_Y       : GL.Types.Single;

    Alpha          : Degree := -210.0;
    Beta           : Degree := -70.0;
    Zoom           : GL.Types.Single := 2.0;

    Key_Pressed    : Boolean := False;

    procedure Check_Keyboard (Window : in out Glfw.Windows.Window);
    procedure Check_Mouse_Buttons (Window : in out Glfw.Windows.Window);

    --  ------------------------------------------------------------------------

    procedure Check_Cursor (Window : in out Glfw.Windows.Window) is
        use GL.Types;
        use Glfw.Input.Mouse;
        X_Coord : Coordinate;
        Y_Coord : Coordinate;
        X       : Single;
        Y       : Single;
    begin
        if Window'Access.Get_Cursor_Mode = Disabled then
            Window'Access.Get_Cursor_Pos (X_Coord, Y_Coord);
            X := Single (X_Coord);
            Y := Single (Y_Coord);

            Alpha := Alpha + Degree ((X -  Cursor_X) / 10.0);
            Beta := Beta + Degree ((Y - Cursor_Y) / 10.0);
            Cursor_X := X;
            Cursor_Y := Y;
        end if;
    end Check_Cursor;

    --  ------------------------------------------------------------------------

    procedure Check_Input (Window : in out Glfw.Windows.Window) is
    begin
        Check_Keyboard (Window);
        Check_Mouse_Buttons (Window);
        Check_Cursor (Window);
        --        Check_Scroll (Window);
    end Check_Input;

    --  ------------------------------------------------------------------------

    procedure Check_Keyboard (Window : in out Glfw.Windows.Window) is
        use GL.Types;
        use Glfw.Input;
    begin
        if Window'Access.Key_State (Keys.Space) = Pressed then
            if not Key_Pressed then
                Vertex_Data.Initialize_Grid;
                Key_Pressed := True;
            end if;
        elsif Window'Access.Key_State (Keys.Left) = Pressed then
            if not Key_Pressed then
                Alpha := Alpha + 5.0;
                Key_Pressed := True;
                if Alpha > 360.0 then
                    Alpha := Alpha - 360.0;
                end if;
            end if;
        elsif Window'Access.Key_State (Keys.Right) = Pressed then
            if not Key_Pressed then
                Alpha := Alpha - 5.0;
                Key_Pressed := True;
                if Alpha < 0.0 then
                    Alpha := Alpha + 360.0;
                end if;
            end if;
        elsif Window'Access.Key_State (Keys.Up) = Pressed then
            if not Key_Pressed then
                Beta := Beta + 5.0;
                Key_Pressed := True;
                if Beta > 360.0 then
                    Beta := Beta - 360.0;
                end if;
            end if;
        elsif Window'Access.Key_State (Keys.Down) = Pressed then
            if not Key_Pressed then
                Beta := Beta - 5.0;
                Key_Pressed := True;
                if Beta < 0.0 then
                    Beta := Beta + 360.0;
                end if;
            end if;
        elsif Window'Access.Key_State (Keys.Page_Up) = Pressed then
            if not Key_Pressed then
                Zoom := Zoom - 0.25;
                Key_Pressed := True;
                if Zoom < 0.0 then
                    Zoom := 0.0;
                end if;
            end if;
        elsif Window'Access.Key_State (Keys.Page_Down) = Pressed then
            if not Key_Pressed then
                Zoom := Zoom + 0.25;
                Key_Pressed := True;
            end if;
        else
            Key_Pressed := False;
        end if;

    end Check_Keyboard;

    --  ------------------------------------------------------------------------

    procedure Check_Mouse_Buttons (Window : in out Glfw.Windows.Window) is
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
        else
            Window.Set_Cursor_Mode (Mouse.Normal);
        end if;
    end Check_Mouse_Buttons;

    --  ------------------------------------------------------------------------

    procedure Get_Settings (A, B : out Maths.Degree; Z : out GL.Types.Single) is
    begin
        A := Alpha;
        B := Beta;
        Z := Zoom;
    end Get_Settings;

    --  ------------------------------------------------------------------------

end Control_Wave;
