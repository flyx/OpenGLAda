with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with Quaternions;

package body Maths is

    use GL.Types;
    use type GL.Types.Singles.Matrix4;
    use type GL.Types.Singles.Vector3;

    package Single_Quaternion is new Quaternions (GL.Types.Single);

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
    --  Computer Graphics Using OpenGL, Chapter 7, transpose of equation 7.2
    --  except, (W, W) = 1 (not 0)
    procedure Init_Lookat_Transform
      (Position, Target, Up : Singles.Vector3;
       Look_At              : out GL.Types.Singles.Matrix4) is
        use GL;
<<<<<<< HEAD
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
<<<<<<< HEAD
        Forward := Normalized (Forward);          --  n / |n|
        Side := Normalized (Side);                --  u / |u|
        Up_New := Normalized (Up_New);            --  v / |v|

        Look_At := GL.Types.Singles.Identity4;
        Look_At (X, X) := Side (X);     --  u.x
        Look_At (Y, X) := Side (Y);     --  u.y
        Look_At (Z, X) := Side (Z);     --  u.z

        Look_At (X, Y) := Up_New (X);   --  v.x
        Look_At (Y, Y) := Up_New (Y);   --  v.y
        Look_At (Z, Y) := Up_New (Z);   --  v.z

        Look_At (X, Z) := Forward (X);  --  n.x;
        Look_At (Y, Z) := Forward (Y);  --  n.y
        Look_At (Z, Z) := Forward (Z);  --  n.z

        Look_At (W, X) := -GL.Types.Singles.Dot_Product (Position, Side);
        Look_At (W, Y) := -GL.Types.Singles.Dot_Product (Position, Up_New);
        Look_At (W, Z) := -GL.Types.Singles.Dot_Product (Position, Forward);
    end Init_Lookat_Transform;

    --  ------------------------------------------------------------------------
    --  Init_Orthographic_Transform is derived from
    --  Computer Graphics Using OpenGL, Chapter 7, transpose of equation 7.18
=======
        Side := GL.Types.Singles.Cross_Product (Up, Forward);
        Up_New := GL.Types.Singles.Cross_Product (Forward, Side);
        Normalize (Forward);             --  n / |n|
        Normalize (Side);                --  u / |u|
        Normalize (Up_New);              --  v / |v|
        --  n,u and v are orthonormal basis vectors.
        Look_At := (
          X => (X => Side (X),        --  ux
                Y => Up_New (X),      --  vx
                Z => Forward (X),     --  nx
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
    --  Init_Lookat_Transform is derived from OpenGL SuperBible
    --
--      procedure Init_Lookat_Transform
--        (Position, Target, Up : GL.Types.Singles.Vector3;
--         Look_At              : out GL.Types.Singles.Matrix4) is
--          use GL;
--          --  Reference co-ordinate frame
--          Forward : GL.Types.Singles.Vector3 := Target - Position; --  n
--          Side    : GL.Types.Singles.Vector3;                      --  u
--          Up_New  : GL.Types.Singles.Vector3;                      --  v
--      begin
--          Side := GL.Types.Singles.Cross_Product (Up, Forward);
--          Up_New := GL.Types.Singles.Cross_Product (Forward, Side);
--          Normalize (Forward);             --  n / |n|
--          Normalize (Side);                --  u / |u|
--          Normalize (Up_New);              --  v / |v|
--          --  n,u and v are orthonormal basis vectors.
--          Look_At := (
--                      X => (X => Side (X),        --  ux
--                            Y => Up_New (X),      --  vx
--                            Z => Forward (X),     --  nx
--                            W => -Position (X)),
--                      Y => (X => Side (Y),
--                            Y => Up_New (Y),
--                            Z => Forward (Y),
--                            W => -Position (Y)),
--                      Z => (X => Side (Z),
--                            Y => Up_New (Z),
--                            Z => Forward (Z),
--                            W => -Position (Z)),
--                      W => (0.0, 0.0, 0.0, 1.0));
--      end Init_Lookat_Transform;

    --  ------------------------------------------------------------------------

    procedure Init_Orthographic_Transform (Bottom, Top, Left, Right,
                                Z_Near, Z_Far : Single;
                                Transform     : out GL.Types.Singles.Matrix4) is
        use GL;
        dX : Single := Right - Left;
        dY : Single := Top - Bottom;
        dZ : Single := Z_Far - Z_Near;
    begin
        Transform := (
                      X => (2.0 / dX, 0.0, 0.0, (Right + Left) / dX),
                      Y => (0.0, 2.0 / dY, 0.0, (Top + Bottom) / dY),
                      Z => (0.0, 0.0, -2.0 / dZ, (Z_Far + Z_Near) / dZ),
                      W => (0.0, 0.0, 0.0, 1.0));
    end Init_Orthographic_Transform;

    --  ------------------------------------------------------------------------
    --  Init_Perspective_Transform is derived from Computer Graphics Using OpenGL
    --  Chapter 7, Figure 7.13
>>>>>>> Add Init_Orthographic_Transform to Maths.

    procedure Init_Orthographic_Transform (Top, Bottom, Left, Right,
                                           Z_Near, Z_Far : Single;
                                           Transform     : out GL.Types.Singles.Matrix4) is
        use GL;
        dX : constant Single := Right - Left;
        dY : constant Single := Top - Bottom;
        dZ : constant Single := Z_Far - Z_Near;
    begin
<<<<<<< HEAD
        Transform := GL.Types.Singles.Identity4;
        Transform (X, X) := 2.0 / dX;
        Transform (W, X) := -(Right + Left) / dX;
        Transform (Y, Y) := 2.0 / dY;
        Transform (W, Y) := -(Top + Bottom) / dY;
        Transform (Z, Z) := 2.0 / dZ;
        Transform (W, Z) := -(Z_Far + Z_Near) / dZ;
    end Init_Orthographic_Transform;

    --  ------------------------------------------------------------------------

    procedure Init_Perspective_Transform (View_Angle                   : Degree;
                                          Width, Height, Z_Near, Z_Far : Single;
                                          Transform                    : out GL.Types.Singles.Matrix4) is
=======
        Transform := (
          X => (2.0 * Z_Near / dX, 0.0, (Right + Left) / dX, 0.0),
          Y => (0.0, 2.0 * Z_Near / dY, (Top + Bottom) / dY, 0.0),
          Z => (0.0, 0.0, -(Z_Far + Z_Near) / dZ, -2.0 * Z_Far * Z_Near / dZ),
          W => (0.0, 0.0, -1.0, 0.0));
    end Init_Perspective_Transform;

    --  ------------------------------------------------------------------------

    procedure Init_Perspective_Transform (View_Angle, Width, Height,
                                Z_Near, Z_Far : Single;
                                Transform     : out GL.Types.Singles.Matrix4) is
        use pSingle_Math_Functions;
        Top          : Single := Z_Near * Tan (To_Radians (View_Angle) / 2.0);
        Right        : Single := Top *  Width / Height;
        Bottom       : Single := -Top;
        Left         : Single := -Right;
>>>>>>> Add Init_Orthographic_Transform to Maths.
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
    --  Chapter 7, transpose of equation 7.13
    function Perspective_Matrix (Top, Bottom, Left, Right, Near, Far : Single)
                                 return GL.Types.Singles.Matrix4 is
        use GL;
        dX         : constant Single := Right - Left;
        dY         : constant Single := Top - Bottom;
        dZ         : constant Single := Far - Near;
        Matrix     : GL.Types.Singles.Matrix4 := Zero_Matrix4;
    begin
        Matrix (X, X) := 2.0 * Near / dX;
        Matrix (Z, X) := (Right + Left) / dX;
        Matrix (Y, Y) := 2.0 * Near / dY;
        Matrix (Z, Y) :=  (Top + Bottom) / dY;
        Matrix (Z, Z) := -(Far + Near) / dZ;
        Matrix (W, Z) := -2.0 * Far * Near / dZ;
        Matrix (Z, W) := -1.0;
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
    --  Rotation_Matrix is based on "Quaternians and spatial rotation" by
    --  en.m.wikipedia.org, with the matrix transposed

    function Rotation_Matrix (Angle : Maths.Degree; Axis : GL.Types.Singles.Vector3)
                              return GL.Types.Singles.Matrix4 is
        use GL;
        use Maths.Single_Math_Functions;
        use Single_Quaternion;

        aQuaternion : Single_Quaternion.Quaternion;
        theMatrix   : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
        NQ          : Single_Quaternion.Quaternion;
        Half_Angle  : Single := 0.5 * Single (Maths.Radians (Angle));
        Sine       : Single := Sin (Half_Angle);
    begin
<<<<<<< HEAD
        aQuaternion := (Cos (Half_Angle), Axis (GL.X) * Sine,
                          Axis (GL.Y) * Sine, Axis (GL.Z) * Sine);
        NQ := Normalized (aQuaternion);

        theMatrix (X, X) := 1.0 - 2.0 * (NQ.C * NQ.C + NQ.D * NQ.D);
        theMatrix (Y, X) := 2.0 * (NQ.B * NQ.C - NQ.A * NQ.D);
        theMatrix (Z, X) := 2.0 * (NQ.B * NQ.D + NQ.A * NQ.C);

        theMatrix (X, Y) := 2.0 * (NQ.B * NQ.C + NQ.A * NQ.D);
        theMatrix (Y, Y) := 1.0 - 2.0 * (NQ.B * NQ.B + NQ.D * NQ.D);
        theMatrix (Z, Y) := 2.0 * (NQ.C * NQ.D - NQ.A * NQ.B);

        theMatrix (X, Z) := 2.0 * (NQ.B * NQ.D - NQ.A * NQ.C);
        theMatrix (Y, Z) := 2.0 * (NQ.C * NQ.D + NQ.A * NQ.B);
        theMatrix (Z, Z) := 1.0 - 2.0 * (NQ.B * NQ.B + NQ.C * NQ.C);
        return theMatrix;
    end Rotation_Matrix;
=======
        return Degrees * Radians_Per_Degree;
    end To_Radians;
>>>>>>> Cosmetic changes

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
    --  Chapter 5, transpose of equation preceding (5.25)

    function Translation_Matrix (Change : Singles.Vector3)
                                 return Singles.Matrix4 is
        use GL;
        theMatrix  : Singles.Matrix4 := Singles.Identity4;
    begin
<<<<<<< HEAD
        theMatrix (W, X) := Change (X);
        theMatrix (W, Y) := Change (Y);
        theMatrix (W, Z) := Change (Z);
        return theMatrix;
    end Translation_Matrix;
=======
        return Radians * Degrees_Per_Radian;
    end To_Degrees;
>>>>>>> Cosmetic changes

    --  ------------------------------------------------------------------------

end Maths;
