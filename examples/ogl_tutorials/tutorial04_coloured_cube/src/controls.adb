
with Ada.Numerics;

with Interfaces.C;

with GL.Types; use GL.Types;

with Glfw;
with Glfw.Input.Keys;

with Maths;

package body Controls is
    Position           : GL.Types.Singles.Vector3 := (0.0, 0.0, 5.0);
    Horizontal_Angle   : Single := 3.14;
    Vertical_Angle     : Single := 0.0;
    Initial_View_Angle : Single := 45.0;
    Speed              : Single := 3.0;  -- units per second
    Mouse_Speed        : Single := 0.005;
    Last_Time          : Double := Double (Glfw.Time);

    Projection_Matrix  : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
    View_Matrix        : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;

    --  ------------------------------------------------------------------------

    Procedure Compute_Matrices_From_Inputs (Window : in out Glfw.Windows.Window) is
        use Interfaces.C;
        use GL.Types.Singles;
        use Glfw.Input;
        use Maths.pSingle_Math_Functions;

        Current_Time       : GL.Types.Double := GL.Types.Double (Glfw.Time);
        Delta_Time         : GL.Types.Single := Single (Current_Time - Last_Time);
        Window_Width       : Glfw.Size;
        Window_Height      : Glfw.Size;
        Half_Window_Width  : Interfaces.C.Double;
        Half_Window_Height : Interfaces.C.Double;
        --  GLFW 3 requires setting up a callback for setting
        --  View_Angle to Initial_View_Angle - 5.0 * glfwGetMouseWheel();
        --  But this is a bit too complicated for a beginner's tutorial,
        --  so it's not implemented in this Tutorial 14.
        View_Angle         : Single := Initial_View_Angle;
        X_Position         : Interfaces.C.Double;
        Y_Position         : Interfaces.C.Double;
        Direction          : GL.Types.Singles.Vector3;
        Right              : GL.Types.Singles.Vector3;
        Up                 : GL.Types.Singles.Vector3;
    begin
        Window'Access.Get_Cursor_Pos (X_Position, Y_Position);
        Window'Access.Get_Size (Window_Width, Window_Height);
        Half_Window_Width := Interfaces.C.Double (Window_Width) / 2.0;
        Half_Window_Height := Interfaces.C.Double (Window_Height) / 2.0;
        Window'Access.Set_Cursor_Pos (Half_Window_Height, Half_Window_Height);

        Horizontal_Angle := Horizontal_Angle +
          Mouse_Speed * Single (Half_Window_Width - X_Position);
        Vertical_Angle := Vertical_Angle +
          Mouse_Speed * Single (Half_Window_Height - Y_Position);
        Direction := (Cos (Vertical_Angle) * Sin (Horizontal_Angle),
                      Sin (Vertical_Angle),
                      Cos (Vertical_Angle) * Cos (Horizontal_Angle));
        Right := (Sin (Horizontal_Angle - Ada.Numerics.Pi / 2.0), 0.0,
                  Cos (Horizontal_Angle - Ada.Numerics.Pi / 2.0));
        Up := Singles.Cross_Product (Right, Direction);

        if Window'Access.Key_State (Keys.Up) = Pressed then
            Position := Position + Direction * Delta_Time * Speed;
        elsif Window'Access.Key_State (Keys.Down) = Pressed then
            Position := Position - Direction * Delta_Time * Speed;
        elsif Window'Access.Key_State (Keys.Right) = Pressed then
            Position := Position + Right * Delta_Time * Speed;
        elsif Window'Access.Key_State (Keys.Down) = Pressed then
            Position := Position - Right * Delta_Time * Speed;
        end if;

        Maths.Init_Perspective_Transform (View_Angle, Single (Window_Width),
                                    Single (Window_Height), 0.1, 100.0,
                                    Projection_Matrix);
        Maths.Init_Lookat_Transform (Position, Position + Direction, Up,
                                     View_Matrix);

        Last_Time := Current_Time;
    end Compute_Matrices_From_Inputs;

    --  ------------------------------------------------------------------------

    function Get_Projection_Matrix return GL.Types.Singles.Matrix4 is
    begin
        return Projection_Matrix;
    end Get_Projection_Matrix;

    --  ------------------------------------------------------------------------

    function Get_View_Matrix return GL.Types.Singles.Matrix4 is
    begin
        return View_Matrix;
    end Get_View_Matrix;

    --  ------------------------------------------------------------------------

end Controls;
