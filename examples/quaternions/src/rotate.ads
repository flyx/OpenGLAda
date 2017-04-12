
with GL.Types;

with Maths;
with Quaternions;

package Rotate is

    package Single_Quaternion is new Quaternions (GL.Types.Single);

    function Quaternion_To_Matrix4 (aQuaternion : Single_Quaternion.Quaternion)
                                    return GL.Types.Singles.Matrix4;
    function Quaternion_To_Vector4 (aQuaternion : Single_Quaternion.Quaternion)
                                    return GL.Types.Singles.Vector4;
    function Rotation_Quaternion (Angle : Maths.Degree;
                                  Axis  : GL.Types.Singles.Vector3)
                                  return Single_Quaternion.Quaternion;
    function Vector4_To_Quaternion (aVector : GL.Types.Singles.Vector4)
                                    return Single_Quaternion.Quaternion;
end Rotate;
