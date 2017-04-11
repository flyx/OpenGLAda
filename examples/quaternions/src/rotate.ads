
with GL.Types;

with Maths;
with Quaternions;

package Rotate is

    package Single_Quaternion is new Quaternions (GL.Types.Single);

    function Rotation_Quaternion (Angle : Maths.Degree;
                                  Axis  : GL.Types.Singles.Vector3)
                                  return Single_Quaternion.Quaternion;
end Rotate;
