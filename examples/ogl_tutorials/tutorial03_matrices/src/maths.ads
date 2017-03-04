
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types;

package Maths is
    package pSingle_Math_Functions is new
      Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);

    subtype tVector_2i is GL.Types.Ints.Vector2;
    subtype tVector_2f is GL.Types.Singles.Vector2;
    subtype tVector_3f is GL.Types.Singles.Vector3;
    subtype tVector_4f is GL.Types.Singles.Vector4;

    subtype tMatrix_3f is GL.Types.Singles.Matrix3;
    subtype tMatrix_4f is GL.Types.Singles.Matrix4;

    type tVertex_Array is array (0 .. 2) of tVector_3f;

    function "+" (V1, V2 : tVector_3f) return  tVector_3f;
    function "-" (Left, Right : tVector_3f) return  tVector_3f;
    function "*" (Left : GL.Types.Single; Right : tVector_3f) return tVector_3f;
    function "*" (Left : tVector_3f; Right : GL.Types.Single) return tVector_3f;
    function "*" (Left :  tMatrix_4f; Right : tMatrix_4f) return tMatrix_4f;

    procedure Init_Lookat_Transform (Position : tVector_3f; Target : tVector_3f;
                                     Up       : tVector_3f;
                                     Look_At  : out tMatrix_4f);
    procedure Init_Perspective_Transform (View_Angle, Width, Height,
                                     Z_Near, Z_Far : GL.Types.Single;
                                     Transform : out tMatrix_4f);
end Maths;

