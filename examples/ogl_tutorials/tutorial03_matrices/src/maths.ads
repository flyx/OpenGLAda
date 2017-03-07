
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types;

package Maths is

    package pSingle_Math_Functions is new
      Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);

    type tVertex_Array is array (0 .. 2) of GL.Types.Singles.Vector3;

    procedure Init_Lookat_Transform (Position : GL.Types.Singles.Vector3;
                                     Target   : GL.Types.Singles.Vector3;
                                     Up       : GL.Types.Singles.Vector3;
                                     Look_At  : out GL.Types.Singles.Matrix4);
    procedure Init_Perspective_Transform (View_Angle, Width, Height,
                                     Z_Near, Z_Far : GL.Types.Single;
                                     Transform : out GL.Types.Singles.Matrix4);
end Maths;

