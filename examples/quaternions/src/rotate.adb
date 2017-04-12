
package body Rotate is
    use GL.Types;
    use Maths.Single_Math_Functions;

    --  ------------------------------------------------------------------------

    function Vector4_To_Quaternion (aVector : GL.Types.Singles.Vector4)
                                    return Single_Quaternion.Quaternion is
       theQuaternion : Single_Quaternion.Quaternion;
    begin
        theQuaternion.A := aVector (GL.X);
        theQuaternion.B := aVector (GL.Y);
        theQuaternion.C := aVector (GL.Z);
        theQuaternion.D := aVector (GL.W);
        return theQuaternion;
    end Vector4_To_Quaternion;

    --  ------------------------------------------------------------------------

    function Quaternion_To_Matrix4 (aQuaternion : Single_Quaternion.Quaternion)
                                    return GL.Types.Singles.Matrix4 is
        USE GL;
        theMatrix   : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
        Norm        : GL.Types.Single;
        NQ          : Single_Quaternion.Quaternion;
    begin
        Norm := Sqrt (aQuaternion.A * aQuaternion.A + aQuaternion.B * aQuaternion.B
           + aQuaternion.C * aQuaternion.C + aQuaternion.D * aQuaternion.D);
        NQ.A := aQuaternion.A / Norm;
        NQ.B := aQuaternion.B/ Norm;
        NQ.C := aQuaternion.C / Norm;
        NQ.D := aQuaternion.D / Norm;

        theMatrix (X, X) := 1.0 - 2.0 * (NQ.B * NQ.B + NQ.C * NQ.C);
        theMatrix (X, Y) := 2.0 * (NQ.A * NQ.B - NQ.C * NQ.D);
        theMatrix (X, Z) := 2.0 * (NQ.A * NQ.C + NQ.B * NQ.D);

        theMatrix (Y, X) := 2.0 * (NQ.A * NQ.B + NQ.C * NQ.D);
        theMatrix (Y, Y) := 1.0 - 2.0 * (NQ.A * NQ.A + NQ.C * NQ.C);
        theMatrix (Y, Z) := 2.0 * (NQ.B * NQ.C - NQ.A * NQ.D);

        theMatrix (Z, X) := 2.0 * (NQ.A * NQ.C - NQ.B * NQ.D);
        theMatrix (Z, Y) := 2.0 * (NQ.B * NQ.C + NQ.A * NQ.D);
        theMatrix (Z, Z) := 1.0 - 2.0 * (NQ.A * NQ.A + NQ.B * NQ.B);
        return theMatrix;
    end Quaternion_To_Matrix4;

    --  ------------------------------------------------------------------------

    function Quaternion_To_Vector4 (aQuaternion : Single_Quaternion.Quaternion)
                                    return GL.Types.Singles.Vector4 is
        theVector : GL.Types.Singles.Vector4;
    begin
        theVector (GL.X) := aQuaternion.A;
        theVector (GL.Y) := aQuaternion.B;
        theVector (GL.Z) := aQuaternion.C;
        theVector (GL.W) := aQuaternion.D;
        return theVector;
    end Quaternion_To_Vector4;

    --  ------------------------------------------------------------------------

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
