
with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types; use GL.Types;

package Maths is

    package Single_Math_Functions is new
      Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);

    type Degree is new Single;
    type Radian is new Single;

    function Degrees (Angle : Radian) return Degree;
    procedure Init_Lookat_Transform
      (Position, Target, Up : Singles.Vector3; Look_At : out Singles.Matrix4);
    procedure Init_Orthographic_Transform (Top, Bottom, Left, Right,
                                           Z_Near, Z_Far : Single;
                                           Transform     : out Singles.Matrix4);
    procedure Init_Perspective_Transform
      (View_Angle : Degree; Width, Height, Z_Near, Z_Far : Single;
       Transform  : out Singles.Matrix4);
    function Length (V : Singles.Vector3) return Single;
    function Normalized (V : Singles.Vector3) return Singles.Vector3;
    function Perspective_Matrix (View_Angle : Degree; Aspect, Near, Far : Single)
                          return Singles.Matrix4;
    function Perspective_Matrix (Top, Bottom, Left, Right, Near, Far : Single)
                          return GL.Types.Singles.Matrix4;
    function Radians (Angle : Degree) return Radian;
    function Rotation_Matrix (Angle : Degree; Axis : Singles.Vector3)
                              return Singles.Matrix4;
    function Scaling_Matrix (Scale_Factor : Singles.Vector3) return Singles.Matrix4;
    function Translation_Matrix (Change : Singles.Vector3)
                                 return Singles.Matrix4;

end Maths;
