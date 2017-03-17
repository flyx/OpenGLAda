
with GL.Types; use GL.Types;

package Maths is

    subtype tDegree is Single;
    subtype tRadian is Single;

    function Degree (Radians : tRadian) return tDegree;
    procedure Init_Lookat_Transform
      (Position, Target, Up : Singles.Vector3; Look_At : out Singles.Matrix4);
    procedure Init_Orthographic_Transform (Bottom, Top, Left, Right,
                                           Z_Near, Z_Far : Single;
                                           Transform     : out Singles.Matrix4);
    procedure Init_Perspective_Transform
      (View_Angle, Width, Height, Z_Near, Z_Far : Single;
       Transform                                : out Singles.Matrix4);
    function Length (V : Singles.Vector3) return Single;
    procedure Normalize (V : in out Singles.Vector3);
    function Perspective (View_Angle, Aspect, Near, Far : Single)
                          return Singles.Matrix4;
    function Radian (Degrees : tDegree) return tRadian;
    function Rotation_Matrix (Angle : Single; Axis : Singles.Vector3)
                              return Singles.Matrix4;
    function Scaling_Matrix (Scale : Singles.Vector3) return Singles.Matrix4 ;
    function Translation_Matrix (Translate: Singles.Vector3)
                                 return Singles.Matrix4;

end Maths;

