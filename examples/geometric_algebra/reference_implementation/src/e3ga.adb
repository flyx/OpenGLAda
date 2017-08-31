
package body E3GA is
    use GA_Maths;

    e1_basis : Array_3D := (1.0, 0.0, 0.0);
    e2_basis : Array_3D := (0.0, 1.0, 0.0);
    e3_basis : Array_3D := (0.0, 0.0, 1.0);

    --  ------------------------------------------------------------------------

    function "*" (Weight : float; BV : Bivector) return Bivector is
    begin
        return (Weight * BV.C1_e1e2, Weight * BV.C2_e2e3, Weight * BV.C3_e3e1);
    end "*";

    --  ------------------------------------------------------------------------


    function Dot_Product (BV1, BV2 : BiVector) return float is
    begin
        return BV1.C1_e1e2 * BV2.C1_e1e2 + BV1.C2_e2e3 * BV2.C2_e2e3 +
          BV1.C3_e3e1 * BV2.C3_e3e1;
    end Dot_Product;

    --  ------------------------------------------------------------------------

    function e1 (V : GA_Maths.Vector_2D) return float is
    begin
        return Get_Coord_1 (V);
    end e1;

    --  ------------------------------------------------------------------------

    function e2 (V : GA_Maths.Vector_2D) return float is
    begin
        return Get_Coord_2 (V);
    end e2;

    --  ------------------------------------------------------------------------

    function e1 return Vector is
        V : Vector;
    begin
        Set_Coords (V, e1_basis (1), e1_basis (2), e1_basis (3));
        return V;
    end e1;

    --  ----------------------------------------------------------------------------

    function e2 return Vector is
        V : Vector;
    begin
        Set_Coords (V, e2_basis (1), e2_basis (2), e2_basis (3));
        return V;
    end e2;

    --  ------------------------------------------------------------------------

    function e3 return Vector is
        V : Vector;
    begin
        Set_Coords (V, e3_basis (1), e3_basis (2), e3_basis (3));
        return V;
    end e3;

    --  ------------------------------------------------------------------------

    function Get_Coords (BV : Bivector) return Array_3D is
    begin
        return (BV.C1_e1e2, BV.C2_e2e3, BV.C3_e3e1);
    end Get_Coords;

    --  ------------------------------------------------------------------------

    function Left_Contraction (MV1, MV2 : Multivector) return Multivector is
        Value  : E2GA.Coords_Continuous_Array (1 .. 8) := (others => 0.0);
    begin
        if (MV2.Grade_Usage and 1) /= 0 and then
          (MV1.Grade_Usage and 1) /= 0 then
            Value (1) := MV1.Coordinates (1) * MV2.Coordinates (1);
        end if;

        if (MV2.Grade_Usage and 2) /= 0 then
            if (MV1.Grade_Usage and 1) /= 0 then
                Value (2) := MV1.Coordinates (1) * MV2.Coordinates (2);
                Value (3) := MV1.Coordinates (1) * MV2.Coordinates (3);
                Value (4) := MV1.Coordinates (1) * MV2.Coordinates (4);
            end if;
            if (MV1.Grade_Usage and 2) /= 0 then
                Value (1) := Value (1) + MV1.Coordinates (2) * MV2.Coordinates (2) +
                  MV1.Coordinates (3) * MV2.Coordinates (3) +
                  MV1.Coordinates (4) * MV2.Coordinates (4);
            end if;
        end if;
        if (MV2.Grade_Usage and 4) /= 0 then
            if (MV1.Grade_Usage and 1) /= 0 then
                Value (5) := MV1.Coordinates (1) * MV2.Coordinates (5);
                Value (6) := MV1.Coordinates (1) * MV2.Coordinates (6);
                Value (7) := MV1.Coordinates (1) * MV2.Coordinates (7);
            end if;
            if (MV1.Grade_Usage and 2) /= 0 then
                Value (2) := Value (2) - MV1.Coordinates (3) * MV2.Coordinates (5)
                  + MV1.Coordinates (4) * MV2.Coordinates (7);
                Value (3) := Value (3) - MV1.Coordinates (4) * MV2.Coordinates (6)
                  + MV1.Coordinates (2) * MV2.Coordinates (5);
            end if;
            if (MV1.Grade_Usage and 4) /= 0 then
                Value (1) := Value (1) - MV1.Coordinates (5) * MV2.Coordinates (5)
                  - MV1.Coordinates (6) * MV2.Coordinates (6)
                  - MV1.Coordinates (7) * MV2.Coordinates (7);
            end if;
        end if;
        if (MV2.Grade_Usage and 8) /= 0 then
            if (MV1.Grade_Usage and 1) /= 0 then
                Value (8) := MV1.Coordinates (1) * MV2.Coordinates (8);
            end if;
            if (MV1.Grade_Usage and 2) /= 0 then
                Value (5) := Value (5) + MV1.Coordinates (4) * MV2.Coordinates (8);
                Value (6) := Value (6) + MV1.Coordinates (2) * MV2.Coordinates (8);
                Value (7) := Value (7) + MV1.Coordinates (3) * MV2.Coordinates (8);
            end if;
            if (MV1.Grade_Usage and 4) /= 0 then
                Value (1) := Value (1) - MV1.Coordinates (6) * MV2.Coordinates (8)
                  - MV1.Coordinates (7) * MV2.Coordinates (8)
                  - MV1.Coordinates (5) * MV2.Coordinates (8);
            end if;
        end if;
        if (MV1.Grade_Usage and 8) /= 0 then
            Value (1) := Value (1) - MV1.Coordinates (8) * MV2.Coordinates (8);
        end if;
        return (8, MV1.Grade_Usage, Value);
    end Left_Contraction;

    --  ------------------------------------------------------------------------

    function Left_Contraction (V : Vector; BV : Bivector) return Vector is
        BC  : GA_Maths.Array_3D := Get_Coords (BV);
        VC  : GA_Maths.Array_3D := Get_Coords (V);
        LC  : Vector;
    begin
        Set_Coords (LC, -VC (2) * BC (1) + VC (3) * BC (3),
                    VC (1) * BC (1) - VC (3) * BC (2),
                    -VC (1) * BC (3) + VC (2) * BC (2));
        Return LC;
    end Left_Contraction;

    --  ------------------------------------------------------------------------

    function Left_Contraction (V1 : Vector; V2 : Vector) return Scalar is
    begin
        Return Scalar (Dot_Product (V1, V2));
    end Left_Contraction;

    --  ------------------------------------------------------------------------

    function Norm_E2 (V : Vector) return Scalar is
        Coords : Array_3D := Get_Coords (V);
    begin
        return Scalar (Coords (1) * Coords (1) + Coords (2) * Coords (2) +
                  Coords (3) * Coords (3));
    end Norm_E2;

    --  ---------------------------------------------------------------------------

    function Norm_E2 (BV : Bivector) return Scalar is
    begin
        return Scalar (BV.C1_e1e2 * BV.C1_e1e2 + BV.C2_e2e3 * BV.C2_e2e3 +
                       BV.C3_e3e1 * BV.C3_e3e1);
    end Norm_E2;

    --  ----------------------------------------------------------------------------

    function Norm_E2 (MV : E2GA.Multivector) return Scalar is
        Value  : float := 0.0;
    begin
        if (MV.Grade_Usage and 1) /= 0 then
            Value := MV.Coordinates (1) * MV.Coordinates (1);
        end if;
        if (MV.Grade_Usage and 2) /= 0 then
            Value := Value + MV.Coordinates (2) * MV.Coordinates (2) +
              MV.Coordinates (3) * MV.Coordinates (3);
        end if;
        if (MV.Grade_Usage and 4) /= 0 then
            Value := Value + MV.Coordinates (4) * MV.Coordinates (4) +
              MV.Coordinates (5) * MV.Coordinates (5) +
              MV.Coordinates (6) * MV.Coordinates (6);
        end if;
        if (MV.Grade_Usage and 8) /= 0 then
            Value := Value + MV.Coordinates (9) * MV.Coordinates (9);
        end if;
        return Scalar (Value);
    end Norm_E2;

    --  ----------------------------------------------------------------------------

    function Norm_E2 (R : Rotor) return Scalar is
    begin
        return Scalar (R.C2_e1e2 * R.C2_e1e2 + R.C3_e2e3 * R.C3_e2e3 +
                    R.C4_e3e1 * R.C4_e3e1);
    end Norm_E2;

    --  ----------------------------------------------------------------------------

    function Norm_E2 (TV : Trivector) return Scalar is
    begin
        return Scalar (TV * TV);
    end Norm_E2;

    --  ---------------------------------------------------------------------------

    function Outer_Product (V1, V2 : Vector) return Bivector is
        use Float_Array_Package;
        use GA_Maths;
        VC1  : array_3D := Get_Coords (V1);
        VC2  : array_3D := Get_Coords (V2);
        Result : Bivector;
    begin
        Set_Bivector (Result, VC1 (1) * VC2 (2) - VC1 (2) * VC2 (1),
                              VC1 (2) * VC2 (3) - VC1 (3) * VC2 (2),
                             -VC1 (1) * VC2 (3) + VC1 (3) * VC2 (1));
        return Result;
    end Outer_Product;

    --  ------------------------------------------------------------------------

    function Scalar_Product (V1, V2 : Vector) return Scalar is
    begin
        return  Scalar (Dot_Product (V1, V2));
    end Scalar_Product;

    --  ------------------------------------------------------------------------

    procedure Set_Bivector (BV : out Bivector; C1, C2, C3 : float) is
    begin
        BV.C1_e1e2 := C1;
        BV.C2_e2e3 := C2;
        BV.C3_e3e1 := C3;
    end Set_Bivector;

    --  ------------------------------------------------------------------------

    procedure Set_Rotor (X : out Rotor; BV : Bivector) is
    begin
        X := (1.0, BV.C1_e1e2, BV.C2_e2e3, BV.C3_e3e1);
    end Set_Rotor;

    --  ------------------------------------------------------------------------

    procedure Set_Rotor (X : out Rotor; MV : Multivector) is
    begin
        X := (MV.Coordinates (1),
              MV.Coordinates (2), MV.Coordinates (3), MV.Coordinates (4));
    end Set_Rotor;

    --  ------------------------------------------------------------------------

    procedure Set_Rotor_Scalar (X : out Rotor; Scalar : float) is
    begin
        X.C1_Scalar := Scalar;
    end Set_Rotor_Scalar;
    --  ------------------------------------------------------------------------

    procedure Set_Rotor (X : out Rotor; C_Scalar, C2, C3, C4 : float) is
    begin
        X := (C_Scalar, C2, C3, C4);
    end Set_Rotor;

    --  ------------------------------------------------------------------------

    function Unit_e (X : Rotor) return Rotor is
        e2        : float;
        Ie        : float;
        New_Rotor : Rotor;
    begin
        e2 := X.C1_Scalar * X.C1_Scalar + X.C2_e1e2 * X.C2_e1e2 +
          X.C3_e2e3 * X.C3_e2e3 + X.C4_e3e1 * X.C4_e3e1;
        Ie := 1.0 / (Float_Functions.Sqrt (e2));

        Set_Rotor (New_Rotor, X.C1_Scalar * Ie, X.C2_e1e2 * Ie, X.C3_e2e3 * Ie,
                   X.C4_e3e1 * Ie);
        return New_Rotor;
    end Unit_E;

    --  ----------------------------------------------------------------------------

    function Unit_e (X : Vector) return Vector is
        e2         : float;
        Ie         : float;
        New_Vector : Vector;
    begin
        e2 := Dot_Product (X, X);
        Ie := 1.0 / Float_Functions.Sqrt (e2);

        Set_Coords (New_Vector, Get_Coord_1 (X) * Ie,
                    Get_Coord_2 (X) * Ie,  Get_Coord_3 (X) * Ie);
        return New_Vector;
    end Unit_E;

    --  ----------------------------------------------------------------------------

end E3GA;
