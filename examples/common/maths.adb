with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package body Maths is
    package pSingle_Math_Functions is new
      Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);

    use GL.Types;
    use type GL.Types.Singles.Matrix4;
    use type GL.Types.Singles.Vector3;

    Radians_Per_Degree : constant Single := Ada.Numerics.Pi / 180.0;
    Degrees_Per_Radian : constant Single := 180.0 / Ada.Numerics.Pi;

    Zero_Matrix4f : constant GL.Types.Singles.Matrix4 :=
      (others => (others => 0.0));

    --  ------------------------------------------------------------------------

function Degree (Radians : tRadian) return tDegree is
    begin
        return Radians * Degrees_Per_Radian;
    end Degree;

    --  ------------------------------------------------------------------------
    --  Init_Lookat_Transform is derived from Computer Graphics Using OpenGL
    --  Chapter 7, Figure 7.11
    procedure Init_Lookat_Transform
      (Position, Target, Up : Singles.Vector3;
       Look_At  : out GL.Types.Singles.Matrix4) is
        use GL;
        --  Reference co-ordinate frame
        Forward : GL.Types.Singles.Vector3 := Position - Target; --  n
        Side    : GL.Types.Singles.Vector3
          --  u = Up x n = |Up| |n| Sin(n, Up)
          := GL.Types.Singles.Cross_Product (Up, Forward);
        Up_New  : GL.Types.Singles.Vector3
          --  v = n x u  = |n| |u| Sin(u, n)
          := GL.Types.Singles.Cross_Product (Forward, Side);
    begin
        Normalize (Forward);             --  n / |n|
        Normalize (Side);                --  u = Sin(n, Up)  ?
        Normalize (Up_New);              --  v = Sin(u, n)   ?

        Look_At :=
         (X => (X => Side (X),        --  ux = Sin(n, Up) (Perp n, Up)x
                Y => Up_New (X),      --  vx = Sin(u, n) (Perp u, n)x
                Z => Forward (X),     --  nx / |n|
                W => -GL.Types.Singles.Dot_Product (Position, Side)),
          Y => (X => Side (Y),
                Y => Up_New (Y),
                Z => Forward (Y),
                W => -GL.Types.Singles.Dot_Product (Position, Up_New)),
          Z => (X => Side (Z),
                Y => Up_New (Z),
                Z => Forward (Z),
                W => -GL.Types.Singles.Dot_Product (Position, Forward)),
          W => (0.0, 0.0, 0.0, 1.0));
    end Init_Lookat_Transform;

    --  ------------------------------------------------------------------------
    --  Init_Orthographic_Transform is derived from
    --  Computer Graphics Using OpenGL, Chapter 7, Figure 7.18

    procedure Init_Orthographic_Transform (Bottom, Top, Left, Right,
                                Z_Near, Z_Far : Single;
                                Transform     : out GL.Types.Singles.Matrix4) is
        use GL;
        dX : Single := Right - Left;
        dY : Single := Top - Bottom;
        dZ : Single := Z_Far - Z_Near;
    begin
        Transform := (X => (2.0 / dX, 0.0, 0.0, -(Right + Left) / dX),
                      Y => (0.0, 2.0 / dY, 0.0, -(Top + Bottom) / dY),
                      Z => (0.0, 0.0, -2.0 / dZ, -(Z_Far + Z_Near) / dZ),
                      W => (0.0, 0.0, 0.0, 1.0));
    end Init_Orthographic_Transform;

    --  ------------------------------------------------------------------------
    --  Init_Lookat_Transform is derived from Computer Graphics Using OpenGL
    --  Chapter 7, Figure 7.13

    procedure Init_Perspective_Transform (Bottom, Top, Left, Right,
                                Z_Near, Z_Far : Single;
                                Transform     : out GL.Types.Singles.Matrix4) is
        use GL;
        dX : Single := Right - Left;
        dY : Single := Top - Bottom;
        dZ : Single := Z_Far - Z_Near;
    begin
        Transform :=
         (X => (2.0 * Z_Near / dX, 0.0, (Right + Left) / dX, 0.0),
          Y => (0.0, 2.0 * Z_Near / dY, (Top + Bottom) / dY, 0.0),
          Z => (0.0, 0.0, -(Z_Far + Z_Near) / dZ, -2.0 * Z_Far * Z_Near / dZ),
          W => (0.0, 0.0, - 1.0, 0.0));
    end Init_Perspective_Transform;

    --  ------------------------------------------------------------------------

    procedure Init_Perspective_Transform (View_Angle, Width, Height,
                                Z_Near, Z_Far : Single;
                                Transform     : out GL.Types.Singles.Matrix4) is
        use pSingle_Math_Functions;
        Top          : Single := Z_Near * Tan (Radian (View_Angle) / 2.0);
        Right        : Single := Top *  Width / Height;
        Bottom       : Single := - Top;
        Left         : Single := - Right;
    begin
        Init_Perspective_Transform (Bottom, Top, Left, Right, Z_Near, Z_Far,
                                    Transform);
    end Init_Perspective_Transform;

    --  ------------------------------------------------------------------------

    function Length (V : GL.Types.Singles.Vector3) return GL.Types.Single is
        use pSingle_Math_Functions;
        use GL;
    begin
        return Sqrt (V (X) * V (X) + V (Y) * V (Y) + V (Z) * V (Z));
    end Length;

    --  ------------------------------------------------------------------------

    procedure Normalize (V : in out GL.Types.Singles.Vector3) is
        use GL;
        L : Single := Length (V);
    begin
        V := (V (X) / L, V (Y) / L, V (Z) / L);
    end Normalize;

    --  ------------------------------------------------------------------------

    function Perspective (Top, Bottom, Left, Right, Near, Far : Single)
                          return GL.Types.Singles.Matrix4 is
        use GL;
        Matrix     : GL.Types.Singles.Matrix4 := Zero_Matrix4f;
    begin
        Matrix (X, X) := 2.0 * Near / (Right - Left);
        Matrix (X, Z) := (Right + Left) / (Right - Left);
        Matrix (Y, Y) := 2.0 * Near / (Top - Bottom);
        Matrix (Y, Z) :=  (Top + Bottom) / (Top - Bottom);
        Matrix (Z, Z) := -(Far + Near) / (Far - Near);
        Matrix (Z, W) := -2.0 * Far * Near / (Far - Near);
        Matrix (W, Z) := -1.0;
        return Matrix;
    end Perspective;

    --  ------------------------------------------------------------------------

    function Perspective (View_Angle, Aspect, Near, Far : Single)
                          return GL.Types.Singles.Matrix4 is
        use pSingle_Math_Functions;

        Top    : Single := Near * Tan ((Ada.Numerics.Pi / 360.0) * View_Angle);
        Bottom : Single := -Top;
        Right  : Single := Top * Aspect;
        Left   : Single := -Right;
    begin
        return Perspective (Top, Bottom, Left, Right, Near, Far);
    end Perspective;

    --  ------------------------------------------------------------------------

    function Radian (Degrees : tDegree) return tRadian is
    begin
        return Degrees * Radians_Per_Degree;
    end Radian;

    --  ------------------------------------------------------------------------
    --  Rotation_Matrix is derived from Computer Graphics Using OpenGL
    --  Chapter 5, matrix preceding equation (5.33)
    --  It is the transformation matrix for rotating a 4D vector by
    --  a radian angle Angle about a 3D Axis (X, Y, Z)

    function Rotation_Matrix (Angle : Single; Axis : Singles.Vector3)
                              return Singles.Matrix4 is
        use GL;
        use pSingle_Math_Functions;
        CosA            : Single := Cos (Angle);
        SinA            : Single := Sin (Angle);
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

    function Translation_Matrix (Translate: Singles.Vector3)
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
