
with GA_Maths; use GA_Maths;
with E2GA;

package E3GA is

    --  Multivector types
    type G2_Type is (MVT_None, MVT_E1_T, MVT_E2_T, MVT_E3_T, MVT_Scalar,
                     MVT_Vector_2D, MVT_Vector, MVT_Bivector, MVT_Trivector,
                     MVT_Rotor, MVT_E1_CT, MVT_E2_CT, MVT_E3_CT,
                     MVT_I3_CT, MVT_I3I_CT, MVT_MV, MVT_Last);
    --  Outermorphism types
    type OM_Type is (OMT_None, OMT_OM, OMT_Last);
--    type Rotor_Coordinates_Type is (Rotor_Scalar_e1e2_e2e3_e3e1);

    type Bivector is private;
    type Multivector is private;
    type Rotor is private;
    type Trivector is private;
--    type Vector is implemented in GA_Maths
--    type Vector_2D is in GA_Maths;

    --  Joinable grade definitions
    Grade_0 : constant integer := 1;
    Grade_1 : constant integer := 2;
    Grade_2 : constant integer := 4;
    Grade_3 : constant integer := 8;

    --  ------------------------------------------------------------------------

    function "*" (Weight : float; BV : Bivector) return Bivector;

    function e1 (V : Vector_2D) return float;
    function e2 (V : Vector_2D) return float;

    function e1 return Vector;
    function e2 return Vector;
    function e3 return Vector;

    function Get_Coords (BV : Bivector) return Array_3D;
    function Left_Contraction (MV1, MV2 : Multivector) return Multivector;
    function Left_Contraction (V : Vector; BV : Bivector) return Vector;
    function Left_Contraction (V1 : Vector; V2 : Vector) return Scalar;
    function Outer_Product (V1, V2 : Vector) return Bivector;

    function Norm_E2 (BV : Bivector) return Scalar;
    function Norm_E2 (V : Vector) return Scalar;
    function Norm_E2 (MV : E2GA.Multivector) return Scalar;
    function Norm_E2 (R : Rotor) return Scalar;
    function Norm_E2 (TV : Trivector) return Scalar;
    function Scalar_Product (V1, V2 : Vector) return Scalar;
    procedure Set_Bivector (BV : out Bivector; C1, C2, C3 : float);
    procedure Set_Rotor (X : out Rotor; C_Scalar, C2, C3, C4 : float);
    procedure Set_Rotor (X : out Rotor; MV : Multivector);
    procedure Set_Rotor (X : out Rotor; BV : Bivector);
    procedure Set_Rotor_Scalar (X : out Rotor; Scalar : float);
    function Unit_e (X : Rotor) return Rotor;
    function Unit_e (X : Vector) return Vector;
private

    type Bivector is record
        C1_e1e2     : float := 0.0;
        C2_e2e3     : float := 0.0;
        C3_e3e1     : float := 0.0;
    end record;
--  /// enum for the coordinates of rotor
--  	enum __rotor_coordinates__ {rotor_scalar_e1e2_e2e3_e3e1};
    type Rotor is record
--        Coords_Type : Rotor_Coordinates_Type := Rotor_Scalar_e1e2_e2e3_e3e1;
        C1_Scalar   : float := 0.0;
        C2_e1e2     : float := 0.0;
        C3_e2e3     : float := 0.0;
        C4_e3e1     : float := 0.0;
    end record;

    type Multivector is new E2GA.Multivector (8);

    type Trivector is new float;  -- e1e2e3

end E3GA;
