
with Ada.Numerics;

with GL.Types; use GL.Types;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;

with Maths;

package body Controls is
    Half_Pi            : constant Single := 0.5 * Ada.Numerics.Pi;
    -- Position, position of camera.
    Position           : GL.Types.Singles.Vector3 := (0.0, 0.0, 5.0);
    Horizontal_Angle   : Single := Ada.Numerics.Pi;
    Vertical_Angle     : Single := 0.0;
    Initial_View_Angle : Maths.Degree := 45.0;
    Speed              : Single := 3.0;  -- units per second
    Mouse_Speed        : Single := 0.5;  -- orig: 0.005
    Last_Time          : Double := Double (Glfw.Time);

    --  ------------------------------------------------------------------------

    Procedure Compute_Matrices_From_Inputs (Window : in out Glfw.Windows.Window;
           Projection_Matrix, View_Matrix : in out GL.Types.Singles.Matrix4) is
        use GL.Types;
        use GL.Types.Singles;
        use Glfw.Input;
        use Maths.Single_Math_Functions;

        Current_Time       : GL.Types.Double := GL.Types.Double (Glfw.Time);
        Delta_Time         : Single := Single (Current_Time - Last_Time);
        Window_Width       : Glfw.Size;
        Window_Height      : Glfw.Size;
        Half_Window_Width  : Single;
        Half_Window_Height : Single;
        --  GLFW 3 requires setting up a callback for setting
        --  View_Angle to Initial_View_Angle - 5.0 * glfwGetMouseWheel();
        --  But this is a bit too complicated for a beginner's tutorial,
        --  so it's not implemented in this Tutorial 14.
        View_Angle         : Maths.Degree := Initial_View_Angle;
        X_Position         : Mouse.Coordinate := 0.00001;
        Y_Position         : Mouse.Coordinate := 0.00002;
        -- Direction, the position of the target with respect to camera.
        Direction          : Vector3;
        Right              : Vector3;
        Up                 : Vector3;
    begin
        Window'Access.Get_Cursor_Pos (X_Position, Y_Position);
        Window'Access.Get_Size (Window_Width, Window_Height);
        Half_Window_Width := 0.5 * Single (Window_Width);
        Half_Window_Height := 0.5 * Single (Window_Height);

        --  Reset the cursor to the center of the screen
        --  otherwise it will soon go outside the window.

        Window'Access.Set_Cursor_Pos (Mouse.Coordinate (Half_Window_Width),
                                      Mouse.Coordinate (Half_Window_Height));
        --  For the horizontal angle, Half_Window_Width - xpos means:
        --  How far is the mouse from the centre of the window ?
        --  The further it is from the centre, the more we want to turn.
        --  mouseSpeed is speeds up or slows down the rotations. Fine-tune this at will.
        --  If the mouse isn't moved, Half_Window_Width - xpos will be 0,
        --  and Horizontal_Angle won't change.
        Horizontal_Angle := Horizontal_Angle +
          Mouse_Speed * Delta_Time * (Half_Window_Width - Single (X_Position));
        Vertical_Angle := Vertical_Angle +
          Mouse_Speed * Delta_Time * (Half_Window_Height - Single (Y_Position));

        Direction := (Cos (Vertical_Angle) * Sin (Horizontal_Angle),
                      Sin (Vertical_Angle),
                      Cos (Vertical_Angle) * Cos (Horizontal_Angle));
        Right := (Sin (Horizontal_Angle - Half_Pi), 0.0,
                  Cos (Horizontal_Angle - Half_Pi));
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
        Maths.Init_Lookat_Transform (Position, Direction, Up,
                                     View_Matrix);

        Last_Time := Current_Time;
    end Compute_Matrices_From_Inputs;

    --  ------------------------------------------------------------------------

end Controls;
