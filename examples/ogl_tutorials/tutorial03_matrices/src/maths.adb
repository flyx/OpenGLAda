
with Ada.Numerics; use Ada.Numerics;

package body Maths is
    use GL.Types;

    Radians_Per_Degree : constant Single := Ada.Numerics.Pi / 180.0;
    Degrees_Per_Radian : constant Single := 180.0 / Ada.Numerics.Pi;

    function Length (V : tVector_3f) return Single;
    procedure Normalize (V : in out tVector_3f);
    function To_Degrees (Radians : Single) return Single;
    function To_Radians (Degrees : Single) return Single;
    function Zero_Matrix4f return tMatrix_4f;

    --  ------------------------------------------------------------------------
    --  Matrix multiplication
    --  AB(i,j) = sum(k = 1..4)(A(i,k)B(k,j))
    function "*" (Left :  tMatrix_4f; Right : tMatrix_4f) return  tMatrix_4f is
        Product : tMatrix_4f;
    begin
        for i in GL.Index_Homogeneous loop
            for j in GL.Index_Homogeneous loop
                Product (i, j) := 0.0;
                for k in GL.Index_Homogeneous  loop
                    Product (i, j) := Product (i, j) +
                                     Left (i, k) * Right (k, j);
                end loop;
            end loop;
        end loop;
        return Product;
    end  "*";

    --  ------------------------------------------------------------------------

    function "+" (V1, V2 : tVector_3f) return  tVector_3f is
        use GL;
    begin
        return (V1 (X) + V2 (X), V1 (Y) + V2 (Y), V1 (Z) + V2 (Z));
    end  "+";

    --  ------------------------------------------------------------------------

    function "-" (Left, Right : tVector_3f) return  tVector_3f is
        use GL;
    begin
        return (Left (X) - Right (X), Left (Y) - Right (Y), Left (Z) - Right (Z));
    end  "-";

    --  ------------------------------------------------------------------------

    function "*" (Left : Single; Right : tVector_3f) return tVector_3f is
        use GL;
    begin
        return (Left * Right (X), Left * Right (Y), Left * Right (Z));
    end  "*";

    --  ------------------------------------------------------------------------

    function "*" (Left : tVector_3f; Right : Single) return tVector_3f is
        use GL;
    begin
        return (Left (X) * Right, Left (Y) * Right, Left (Z) * Right);
    end  "*";

    --  ------------------------------------------------------------------------

      function Cross (V1 : tVector_3f; V2 : tVector_3f) return tVector_3f is
        use GL;
    begin
        return (V1 (Y) * V2 (Z) - V1 (Z) * V2 (Y),
                V1 (Z) * V2 (X) - V1 (X) * V2 (Z),
                V1 (X) * V2 (Y) - V1 (Y) * V2 (X));
    end Cross;

    --  ------------------------------------------------------------------------

      function Dot (V1 : tVector_3f; V2 : tVector_3f) return Single is
        use GL;
    begin
        return (V1 (X) * V2 (X) + V1 (Y) * V2 (Y) + V1 (Z) * V2 (Z));
    end Dot;

    --  ------------------------------------------------------------------------

    --  Init_Lookat_Transform is derived from Computer Graphics Using OpenGL
    --  Chapter 7, Figure 7.11
    procedure Init_Lookat_Transform (Position : tVector_3f; Target : tVector_3f;
                                     Up       : tVector_3f;
                                     Look_At  : out tMatrix_4f) is
        use GL;
        --  Reference co-ordinate frame
        Forward : tVector_3f := Position - Target;    --  n
        Side    : tVector_3f;
        Up_New  : tVector_3f;
    begin
        Side := Cross (Up, Forward);     --  u = Up x n = |Up| |n| Sin(n, Up)
        Up_New := Cross (Forward, Side); --  v = n x u  = |n| |u| Sin(u, n)
        Normalize (Forward);             --  n / |n|
        Normalize (Side);                --  u = Sin(n, Up)  ?
        Normalize (Up_New);              --  v = Sin(u, n)   ?

        Look_At := Singles.Identity4;
        Look_At (X, X) := Side (X);        --  ux = Sin(n, Up) (Perp n, Up)x
        Look_At (X, Y) := Up_New (X);      --  vx = Sin(u, n) (Perp u, n)x
        Look_At (X, Z) := Forward (X);     --  nx / |n|
        Look_At (X, W) := - Dot (Position, Side);

        Look_At (Y, X) := Side (Y);
        Look_At (Y, Y) := Up_New (Y);
        Look_At (Y, Z) := Forward (Y);
        Look_At (Y, W) := - Dot (Position, Up_New);

        Look_At (Z, X) := Side (Z);
        Look_At (Z, Y) := Up_New (Z);
        Look_At (Z, Z) := Forward (Z);
        Look_At (Z, W) := - Dot (Position, Forward);

    end Init_Lookat_Transform;

    --  ------------------------------------------------------------------------
    --  Init_Lookat_Transform is derived from Computer Graphics Using OpenGL
    --  Chapter 7, Figure 7.13

 procedure Init_Perspective_Transform (Bottom, Top, Left, Right,
                                          Z_Near, Z_Far : Single;
                                          Transform     : out tMatrix_4f) is
        use GL;
        use pSingle_Math_Functions;
        dX : Single := Right - Left;
        dY : Single := Top - Bottom;
        dZ : Single := Z_Far - Z_Near;
    begin
        Transform := Zero_Matrix4f;
        Transform (X, X) := 2.0 * Z_Near / dX;
        Transform (X, Z) := (Right + Left) / dX;
        Transform (Y, Y) := 2.0 * Z_Near / dY;
        Transform (Y, Z) := (Top + Bottom) / dY;
        Transform (Z, Z) := - (Z_Far + Z_Near) / dZ;
        Transform (Z, W) := - 2.0 * Z_Far * Z_Near / dZ;
        Transform (W, Z) := - 1.0;
    end Init_Perspective_Transform;

    --  ------------------------------------------------------------------------

    procedure Init_Perspective_Transform (View_Angle, Width, Height,
                                          Z_Near, Z_Far : Single;
                                          Transform     : out tMatrix_4f) is
        use GL;
        use pSingle_Math_Functions;

        Top          : Single := Z_Near * Tan (To_Radians (View_Angle) / 2.0);
        Right        : Single := Top *  Width / Height;
        Bottom       : Single := - Top;
        Left         : Single := - Right;
    begin
        Init_Perspective_Transform (Bottom, Top, Left, Right, Z_Near, Z_Far,
                                    Transform);
    end Init_Perspective_Transform;

    --  ------------------------------------------------------------------------

    function Length (V : tVector_3f) return Single is
        use pSingle_Math_Functions;
        use GL;
    begin
        return Sqrt (V (X) * V (X) + V (Y) * V (Y) + V (Z) * V (Z));
    end Length;

    --  ------------------------------------------------------------------------

    procedure Normalize (V : in out tVector_3f) is
        use GL;
        use pSingle_Math_Functions;
        L : Single := Length (V);
    begin
        V := (V (X) / L, V (Y) / L, V (Z) / L);
    end Normalize;

    --  ------------------------------------------------------------------------

    function Perspective (Top, Bottom, Left, Right, Near, Far : Single)
                          return tMatrix_4f is
        use GL;
        Matrix     : tMatrix_4f := Zero_Matrix4f;
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
                          return tMatrix_4f is
        use pSingle_Math_Functions;

        Top          : Single := Near * Tan ((Pi / 360.0) * View_Angle);
        Bottom       : Single := -Top;
        Right        : Single := Top * Aspect;
        Left         : Single := -Right;
    begin
        return Perspective (Top, Bottom, Left, Right, Near, Far);
    end Perspective;

    --  ------------------------------------------------------------------------

    function To_Radians (Degrees : Single) return Single is
    begin
        return Degrees * Radians_Per_Degree;
    end ;

    --  ------------------------------------------------------------------------

    function To_Degrees (Radians : Single) return Single is
    begin
        return Radians * Degrees_Per_Radian;
    end ;

    --  ------------------------------------------------------------------------

       function Zero_Matrix4f return tMatrix_4f is
        Zero_Matrix     : tMatrix_4f;
    begin
        for row in GL.Index_Homogeneous loop
            for col in  GL.Index_Homogeneous loop
                Zero_Matrix (row, col) := 0.0;
            end loop;
        end loop;
        return Zero_Matrix;
    end Zero_Matrix4f;

    --  ------------------------------------------------------------------------

end Maths;
