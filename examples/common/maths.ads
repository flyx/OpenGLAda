
with GL.Types;

package Maths is
    procedure Init_Lookat_Transform
      (Position, Target, Up : GL.Types.Singles.Vector3;
       Look_At              : out GL.Types.Singles.Matrix4);
    procedure Init_Orthographic_Transform (Bottom, Top, Left, Right,
                                 Z_Near, Z_Far : GL.Types.Single;
                                 Transform     : out GL.Types.Singles.Matrix4);
    procedure Init_Perspective_Transform
      (View_Angle, Width, Height, Z_Near, Z_Far : GL.Types.Single;
       Transform                                : out GL.Types.Singles.Matrix4);
    function Perspective (View_Angle, Aspect, Near, Far : GL.Types.Single)
                          return GL.Types.Singles.Matrix4;
end Maths;
