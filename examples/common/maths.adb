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
    --  Init_Lookat_Transform is derived from OpenGL SuperBible
    --  Chapter 4, Maths for 3D Graphics, The Lookat Matrix
    procedure Init_Lookat_Transform
      (Position, Target, Up : Singles.Vector3;
       Look_At              : out GL.Types.Singles.Matrix4) is
        use GL;
        x_axis  : Singles.Vector3 := (1.0, 0.0, 0.0);
        y_axis  : Singles.Vector3 := (0.0, 1.0, 0.0);
        z_axis  : Singles.Vector3 := (0.0, 0.0, 1.0);
        --  Reference co-ordinate frame
        Forward : GL.Types.Singles.Vector3 := Position - Target; --  f
        Side    : GL.Types.Singles.Vector3
          := GL.Types.Singles.Cross_Product (Up, Forward);       --  s = Up x f
        Up_New  : GL.Types.Singles.Vector3
          := GL.Types.Singles.Cross_Product (Forward, Side);     --  u = f x s
    begin
        Forward := Normalized (Forward);          --  f / |f|
        Side := Normalized (Side);                --  s / |s|
        Up_New := Normalized (Up_New);            --  u / |uv|

        Look_At :=
          (X => (X => Dot (Side, x_axis),     --  s.x
                 Y => Dot (Side, y_axis),     --  s.y
                 Z => Dot (Side, z_axis),     --  s.z
                 W => -Target (X),
           Y => (X => Dot (Up_New, x_axis),   --  u.x
                 Y => Dot (Up_New, y_axis),   --  u.y
                 Z => Dot (Up_New, z_axis),   --  u.z
                 W => -Target (Y),
           Z => (X => Dot (Forward, x_axis),  --  f.x
                 Y => Dot (Forward, y_axis),  --  f.y
                 Z => Dot (Forward, z_axis),  --  f.z
                 W => -Target (Z),
           W => (0.0, 0.0, 0.0, 1.0));
    end Init_Lookat_Transform;

    --  ------------------------------------------------------------------------
    --  Init_Orthographic_Transform is derived from
    --  Computer Graphics Using OpenGL, Chapter 7, equation 7.18

    procedure Init_Orthographic_Transform (Bottom, Top, Left, Right,
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

        Top    : constant Single := Near * Tan (Single (0.5 * View_Angle));
        Bottom : constant Single := -Top;
        Right  : constant Single := Top * Aspect;
        Left   : constant Single := -Right;
    begin
        return Perspective_Matrix (Top, Bottom, Left, Right, Near, Far);
    end Perspective_Matrix;

    --  ------------------------------------------------------------------------

    function Radians (Angle : Degree) return Radian is
    begin
        return Radian (Angle) * Radians_Per_Degree;
    end Radians;

    --  ------------------------------------------------------------------------
    --  Rotation_Matrix is derived from Computer Graphics Using OpenGL
    --  Chapter 5, matrix preceding equation (5.33)
    --  It is the transformation matrix for rotating a 4D vector by
    --  a radian angle Angle about a 3D Axis (X, Y, Z)

    function Rotation_Matrix (Angle : Single; Axis : Singles.Vector3)
                              return Singles.Matrix4 is
        use GL;
        use Single_Math_Functions;
        CosA            : constant Single := Cos (Angle);
        SinA            : constant Single := Sin (Angle);
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
    --  Chapter 5, matrix preceding equation (5.25)

    function Scaling_Matrix (Scale : Singles.Vector3) return Singles.Matrix4 is
        use GL;
        theMatrix  : Singles.Matrix4 := Singles.Identity4;
    begin
        theMatrix (X, X) := Scale (X);
        theMatrix (Y, Y) := Scale (Y);
        theMatrix (Z, Z) := Scale (Z);
        return theMatrix;
    end Scaling_Matrix;

    --  ------------------------------------------------------------------------
    --  Translation_Matrix is derived from Computer Graphics Using OpenGL
    --  Chapter 5, equation (5.25)

    function Translation_Matrix (Translate : Singles.Vector3)
                                 return Singles.Matrix4 is
        use GL;
        theMatrix  : Singles.Matrix4 := Singles.Identity4;
    begin
        theMatrix (X, W) := Translate (X);
        theMatrix (Y, W) := Translate (Y);
        theMatrix (Z, W) := Translate (Z);
        return theMatrix;
    end Translation_Matrix;

    --  ------------------------------------------------------------------------

end Maths;
