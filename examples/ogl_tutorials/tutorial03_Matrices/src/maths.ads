
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types; use GL.Types;

package Maths is
    package pSingle_Math_Functions is new
      Ada.Numerics.Generic_Elementary_Functions (Single);

    subtype tVector_2i is Ints.Vector2;
    subtype tVector_2f is Singles.Vector2;
    subtype tVector_3f is Singles.Vector3;
    subtype tVector_4f is Singles.Vector4;

    subtype tMatrix_3f is Singles.Matrix3;
    subtype tMatrix_4f is Singles.Matrix4;

    type tVertex_Array is array (0 .. 2) of tVector_3f;

    function "+" (V1, V2 : tVector_3f) return  tVector_3f;
    function "-" (Left, Right : tVector_3f) return  tVector_3f;
    function "*" (Left : Single; Right : tVector_3f) return tVector_3f;
    function "*" (Left : tVector_3f; Right : Single) return tVector_3f;
    function "*" (Left :  tMatrix_4f; Right : tMatrix_4f) return tMatrix_4f;

    procedure Init_Lookat_Transform (Position : tVector_3f; Target : tVector_3f;
                                     Up       : tVector_3f;
                                     Look_At  : out tMatrix_4f);
    procedure Init_Perspective_Transform (View_Angle, Width, Height,
                                     Z_Near, Z_Far : Single;
                                     Transform : out tMatrix_4f);
private

end Maths;

