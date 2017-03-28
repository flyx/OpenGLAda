with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package body Maths is
    package Single_Math_Functions is new
      Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);

    use GL.Types;
    use type GL.Types.Singles.Matrix4;
    use type GL.Types.Singles.Vector3;

    Radians_Per_Degree : constant Radian := Ada.Numerics.Pi / 180.0;
    Degrees_Per_Radian : constant Degree := 180.0 / Ada.Numerics.Pi;

    Zero_Matrix4 : constant GL.Types.Singles.Matrix4 :=
                     (others => (others => 0.0));

    --  ------------------------------------------------------------------------

    function Degrees (Angle : Radian) return Degree is
    begin
        return Degree (Angle) * Degrees_Per_Radian;
    end Degrees;

    --  ------------------------------------------------------------------------
    --  Init_Lookat_Transform is derived from
    --  Computer Graphics Using OpenGL, Chapter 7, equation 7.2
    --  except, (W, W) = 1 (not 0)
    procedure Init_Lookat_Transform
      (Position, Target, Up : Singles.Vector3;
       Look_At              : out GL.Types.Singles.Matrix4) is
        use GL;
        use GL.Types;
        --  Reference co-ordinate frame (u, v, n)
        --  Forward (n): camera axis
        --  Side (u): axis through side of camera, perpendicular to Forward
        --  Up_New (v): vertical axis of camera, perpendicular to Forward and Side
        Forward : Singles.Vector3 := Position - Target; --  n
        Side    : Singles.Vector3
          := GL.Types.Singles.Cross_Product (Up, Forward);       --  u = Up x n
        Up_New  : Singles.Vector3
          := GL.Types.Singles.Cross_Product (Forward, Side);     --  v = n x u
    begin
        Forward := Normalized (Forward);          --  n / |n|
        Side := Normalized (Side);                --  u / |u|
        Up_New := Normalized (Up_New);            --  v / |v|

        Look_At :=
          (X => (X => Side (X),     --  u.x
                 Y => Side (Y),     --  u.y
                 Z => Side (Z),     --  u.z
                 W => -GL.Types.Singles.Dot_Product (Position, Side)),
           Y => (X => Up_New (X),   --  v.x
                 Y => Up_New (Y),   --  v.y
                 Z => Up_New (Z),   --  v.z
                 W => -GL.Types.Singles.Dot_Product (Position, Up_New)),
           Z => (X => Forward (X),  --  n.x
                 Y => Forward (Y),  --  n.y
                 Z => Forward (Z),  --  n.z
                 W => -GL.Types.Singles.Dot_Product (Position, Forward)),
           W => (0.0, 0.0, 0.0, 1.0));
    end Init_Lookat_Transform;

    --  ------------------------------------------------------------------------
    --  Init_Orthographic_Transform is derived from
    --  Computer Graphics Using OpenGL, Chapter 7, equation 7.18

    procedure Init_Orthographic_Transform (Top, Bottom, Left, Right,
                                           Z_Near, Z_Far : Single;
                                           Transform     : out GL.Types.Singles.Matrix4) is
        use GL;
        dX : constant Single := Right - Left;
        dY : constant Single := Top - Bottom;
        dZ : constant Single := Z_Far - Z_Near;
    begin
        Transform := (X => (2.0 / dX, 0.0, 0.0, -(Right + Left) / dX),
                      Y => (0.0, 2.0 / dY, 0.0, -(Top + Bottom) / dY),
                      Z => (0.0, 0.0, -2.0 / dZ, -(Z_Far + Z_Near) / dZ),
                      W => (0.0, 0.0, 0.0, 1.0));
    end Init_Orthographic_Transform;

    --  ------------------------------------------------------------------------

    procedure Init_Perspective_Transform (View_Angle                   : Degree;
                                          Width, Height, Z_Near, Z_Far : Single;
                                          Transform                    : out GL.Types.Singles.Matrix4) is
    begin
        Transform := Perspective_Matrix (View_Angle, Width / Height,
                                         Z_Near, Z_Far);
    end Init_Perspective_Transform;

    --  ------------------------------------------------------------------------

    function Length (V : GL.Types.Singles.Vector3) return GL.Types.Single is
        use Single_Math_Functions;
        use GL;
    begin
        return Sqrt (V (X) * V (X) + V (Y) * V (Y) + V (Z) * V (Z));
    end Length;

    --  ------------------------------------------------------------------------

    function Normalized (V : Singles.Vector3) return Singles.Vector3 is
        use GL;
        L : Single := Length (V);
    begin
        return (V (X) / L, V (Y) / L, V (Z) / L);
    end Normalized;

    --  ------------------------------------------------------------------------
    --  Perspective_Matrix is derived from Computer Graphics Using OpenGL
    --  Chapter 7, equation 7.13
    function Perspective_Matrix (Top, Bottom, Left, Right, Near, Far : Single)
                                 return GL.Types.Singles.Matrix4 is
        use GL;
        dX         : constant Single := Right - Left;
        dY         : constant Single := Top - Bottom;
        dZ         : constant Single := Far - Near;
        Matrix     : GL.Types.Singles.Matrix4 := Zero_Matrix4;
    begin
        Matrix (X, X) := 2.0 * Near / dX;
        Matrix (X, Z) := (Right + Left) / dX;
        Matrix (Y, Y) := 2.0 * Near / dY;
        Matrix (Y, Z) :=  (Top + Bottom) / dY;
        Matrix (Z, Z) := -(Far + Near) / dZ;
        Matrix (Z, W) := -2.0 * Far * Near / dZ;
        Matrix (W, Z) := -1.0;
        return Matrix;
    end Perspective_Matrix;

    --  ------------------------------------------------------------------------
    --  Perspective_Matrix is derived from Computer Graphics Using OpenGL
    --  Chapter 7, top, bottom, left and right equations following equation 7.13
    function Perspective_Matrix (View_Angle : Degree; Aspect, Near, Far : Single)
                                 return GL.Types.Singles.Matrix4 is
        use Single_Math_Functions;

        Top    : Single;
        Bottom : Single;
        Right  : Single;
        Left   : Single;
    begin
        Top := Near * Tan (Single (0.5 * Radians (View_Angle)));
        Bottom := -Top;
        Right  := Top * Aspect;
        Left   := -Right;
        return Perspective_Matrix (Top, Bottom, Left, Right, Near, Far);
    end Perspective_Matrix;

    --  ------------------------------------------------------------------------

    function Radians (Angle : Degree) return Radian is
    begin
        return Radian (Angle) * Radians_Per_Degree;
    end Radians;

    --  ------------------------------------------------------------------------

    function Rotation_Matrix (Angle : Degree; Axis : Singles.Vector3)
                              return Singles.Matrix4 is
    begin
        return Rotation_Matrix (Radians (Angle), Axis);
    end Rotation_Matrix;

    --  ------------------------------------------------------------------------

    --  Rotation_Matrix is derived from Computer Graphics Using OpenGL
    --  Chapter 5, matrix preceding equation (5.33)
    --  It is the transformation matrix for rotating a 4D vector by
    --  a radian angle Angle about a 3D Axis (X, Y, Z)

    function Rotation_Matrix (Angle : Radian; Axis : Singles.Vector3)
                              return Singles.Matrix4 is
        use GL;
        use Single_Math_Functions;
        CosA            : constant Single := Cos (Single (Angle));
        SinA            : constant Single := Sin (Single (Angle));
        theMatrix       : Singles.Matrix4 := Singles.Identity4;
    begin
        theMatrix (X, X) := CosA + (1.0 - CosA) * Axis (X) * Axis (X);
        theMatrix (X, Y) := (1.0 - CosA) * Axis (Y) * Axis (X) - SinA * Axis (Z);
        theMatrix (X, Z) := (1.0 - CosA) * Axis (Z) * Axis (X) + SinA * Axis (Y);

        theMatrix (Y, X) := (1.0 - CosA) * Axis (X) * Axis (Y) + SinA * Axis (Z);
        theMatrix (Y, Y) := CosA + (1.0 - CosA) * Axis (Y) * Axis (Y);
        theMatrix (Y, Z) := (1.0 - CosA) * Axis (Z) * Axis (Y)  - SinA * Axis (X);

        theMatrix (Z, X) := (1.0 - CosA) * Axis (X) * Axis (Z) - SinA * Axis (Y);
        theMatrix (Z, Y) := (1.0 - CosA) * Axis (Y) * Axis (Z) + SinA * Axis (X);
        theMatrix (Z, Z) :=  CosA + (1.0 - CosA) * Axis (Z) * Axis (Z);
        return theMatrix;
    end Rotation_Matrix;

    --  ------------------------------------------------------------------------
    --  Scaling_Matrix is derived from Computer Graphics Using OpenGL
    --  Chapter 5, matrix equation (5.25)

    function Scaling_Matrix (Scale_Factor : Singles.Vector3) return Singles.Matrix4 is
        use GL;
        theMatrix  : Singles.Matrix4 := Singles.Identity4;
    begin
        theMatrix (X, X) := Scale_Factor (X);
        theMatrix (Y, Y) := Scale_Factor (Y);
        theMatrix (Z, Z) := Scale_Factor (Z);
        return theMatrix;
    end Scaling_Matrix;

    --  ------------------------------------------------------------------------
    --  Translation_Matrix is derived from Computer Graphics Using OpenGL
    --  Chapter 5, equation preceding (5.25)

    function Translation_Matrix (Change : Singles.Vector3)
                                 return Singles.Matrix4 is
        use GL;
        theMatrix  : Singles.Matrix4 := Singles.Identity4;
    begin
        theMatrix (X, W) := Change (X);
        theMatrix (Y, W) := Change (Y);
        theMatrix (Z, W) := Change (Z);
        return theMatrix;
    end Translation_Matrix;

    --  ------------------------------------------------------------------------

end Maths;
