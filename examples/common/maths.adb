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

    function Length (V : GL.Types.Singles.Vector3) return Single;
    procedure Normalize (V : in out GL.Types.Singles.Vector3);
    function To_Degrees (Radians : Single) return Single;
    function To_Radians (Degrees : Single) return Single;

    --  ------------------------------------------------------------------------
    --  Init_Lookat_Transform is derived from Computer Graphics Using OpenGL
    --  Chapter 7, Figure 7.11
    procedure Init_Lookat_Transform
      (Position, Target, Up : GL.Types.Singles.Vector3;
       Look_At  : out GL.Types.Singles.Matrix4) is
        use GL;
        --  Reference co-ordinate frame
        Forward : GL.Types.Singles.Vector3 := Position - Target; --  n
        Side    : GL.Types.Singles.Vector3;                      --  u
        Up_New  : GL.Types.Singles.Vector3;                      --  v
    begin
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

    procedure Init_Perspective_Transform (Bottom, Top, Left, Right,
                                Z_Near, Z_Far : Single;
                                Transform     : out GL.Types.Singles.Matrix4) is
        use GL;
        dX : Single := Right - Left;
        dY : Single := Top - Bottom;
        dZ : Single := Z_Far - Z_Near;
    begin
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
    begin
        Init_Perspective_Transform (Bottom, Top, Left, Right, Z_Near, Z_Far,
                                    Transform);
    end Init_Perspective_Transform;

    --  ------------------------------------------------------------------------

    function Length (V : GL.Types.Singles.Vector3) return Single is
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

    function To_Radians (Degrees : Single) return Single is
    begin
        return Degrees * Radians_Per_Degree;
    end To_Radians;

    --  ------------------------------------------------------------------------

    function To_Degrees (Radians : Single) return Single is
    begin
        return Radians * Degrees_Per_Radian;
    end To_Degrees;

    --  ------------------------------------------------------------------------

end Maths;
