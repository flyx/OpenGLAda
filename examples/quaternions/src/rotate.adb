
with Ada.Unchecked_Conversion;

package body Rotate is
    use GL.Types;
    use Maths.Single_Math_Functions;

    function Vector4_To_Quaternion is new
      Ada.Unchecked_Conversion (Singles.Vector4, Single_Quaternion.Quaternion);
    function Quaternion_To_Vector4 is new
      Ada.Unchecked_Conversion (Single_Quaternion.Quaternion, Singles.Vector4);

    function Rotation_Quaternion (Angle : Maths.Degree;
                                  Axis  : GL.Types.Singles.Vector3)
                                  return Single_Quaternion.Quaternion is
        Half_Angle : Single := 0.5 * Single (Maths.Radians (Angle));
        Sine       : Single := Sin (Half_Angle);
        theQuaternion : Single_Quaternion.Quaternion;
    begin
        theQuaternion := (Cos (Half_Angle), Axis (GL.X) * Sine,
                          Axis (GL.Y) * Sine, Axis (GL.Z) * Sine);
        return theQuaternion;
    end Rotation_Quaternion;

    --  ------------------------------------------------------------------------

    procedure Rotate_Vertex (Vertex : in out Singles.Vector3;
                             Angle  : Maths.Degree; Axis : Singles.Vector3) is
       use Single_Quaternion;
       aQuaternion : Quaternion;
       Position    : Quaternion;
    begin
       aQuaternion := Rotation_Quaternion (Angle, Axis);
       Position := Vector4_To_Quaternion (Singles.To_Vector4 (Vertex));
       aQuaternion := aQuaternion * Position * Single_Quaternion.Conj (aQuaternion);
       Vertex := Singles.To_Vector3 (Quaternion_To_Vector4 (aQuaternion));
    end Rotate_Vertex;

end Rotate;
