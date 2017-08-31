
with GA_Maths; use GA_Maths;

package E2GA is

    type Bit_Map is new integer range 0 .. 2 ** 30;
    type Bit_Map_Array is array (integer range <>) of Bit_Map;
    type Bivector is record
        Grade_Usage : GA_Maths.Unsigned_Integer := 2; --  m_gu
        e1e2_Coord  : float; -- Coordinate of e1^e2.
    end record;

    type Coords_4 is record
        Coord_1 : float;
        Coord_2 : float;
        Coord_3 : float;
        Coord_4 : float;
    end record;

    type Coords3_Array is array (integer range <>) of Vector;
    type Coords_Continuous_Array is array (integer range <>) of float;
    type E2_Bit_Map is new integer range 0 .. 4;

    type E2_Type is record
        Coord_1 : float;
        Coord_2 : float;
    end record;

    --  Multivector types
    type G2_Type is (MVT_None, MVT_E1_T, MVT_E2_T, MVT_Scalar, MVT_Vector,
                     MVT_Bivector, MVT_Rotor, MVT_E1_CT, MVT_E2_CT,
                     MVT_I2_CT, MVT_I2I_CT, MVT_MV, MVT_Last);

    --  Joinable grade definitions
    Grade_0 : constant integer := 1;
    Grade_1 : constant integer := 2;
    Grade_2 : constant integer := 4;

    --  Outermorphism types
    type OM_Type is (OMT_None, OMT_OM, OMT_Last);

    type Rotor is record
        M_C1 : float := 0.0;
        M_C2 : float := 0.0;
    end record;

    type Trivector is record
        Grade_Usage    : GA_Maths.Unsigned_Integer; --  m_gu
        e1e2_Coord_1   : float; -- Coordinate of e1^e2.
        e2e3_Coord_2   : float; -- Coordinate of e2^e3.
        e1e2e3_Coord_3 : float; -- Coordinate of e1^e2^e3.
    end record;

    --  The outer product of P vectors is called a grade P multivector or a P-vector
    --  In E2, MV = u^v = det(u,v)e1^e2. This "bivector" is the only MV in E2.
    type Multivector (Size : integer) is record
        Grade_Usage : GA_Maths.Unsigned_Integer; --  m_gu
        Coordinates : Coords_Continuous_Array (1 .. Size);   --  m_c[4]
    end record;

    MV_Space_Dimension : constant integer := 2;
    MV_Metric_Euclidean : constant boolean := True;
    --  MV_Grade_Size is a lookup table for the number of coordinates
    --  in the grade part of a general multivector
    MV_Grade_Size : constant array (1 .. 3) of integer := (1, 2, 1);
    --  MV_Size is a lookup table for the number of coordinates based on a grade usage bitmap
    MV_Size : constant array (1 .. 8) of integer := (0, 1, 2, 3, 1, 2, 3, 4);
    MV_Basis_Elements : array (1 .. 4, 1 .. 3) of integer :=
                          ((-1, 999, 999), (0, -1, 999), (1, -1, 999), (0, 1, -1));
    MV_Basis_Element_Sign_By_Index : constant array (1 .. 4) of integer := (1, 1, 1, 1);
    MV_Basis_Element_Sign_By_Bit_Map : constant array (Bit_Map range 1 .. 4) of integer := (1, 1, 1, 1);
    --  MV_Basis_Element_Index_By_Bit_Map contains the order of basis elements in the general multivector
    --  Use it to answer: 'at what index do I find basis element [x] (x = basis vector bitmap)?'
    MV_Basis_Element_Index_By_Bit_Map : constant array (Bit_Map range 1 .. 4) of integer := (0, 1, 2, 3);
    --  TMV_Basis_Element_Bit_Map_By_Index contains the indices of basis elements in the general multivector
    --  Use it to answer: 'what basis element do I find at index [x]'?
    MV_Basis_Element_Bit_Map_By_Index : constant array (1 .. 4) of integer := (0, 1, 2, 3);
    MV_Basis_Element_Grade_By_Bit_Map : constant array (Bit_Map range 1 .. 4) of integer := (0, 1, 1, 2);

    function Construct_Vector (Coord : float) return GA_Maths.Vector_2D;
    function Construct_Vector (Coord_1,  Coord_2 : float) return GA_Maths.Vector_2D;
    function Dual (MV : Multivector) return Multivector;
    function e1 return GA_Maths.Vector_2D;
    function e2 return GA_Maths.Vector_2D;
    function Norm_E (MV : Multivector) return Scalar;
    function Norm_E2 (BV : Bivector) return Scalar;
    function Norm_E2 (V2 : GA_Maths.Vector_2D) return Scalar;
    function Norm_E2 (MV : Multivector) return Scalar;
    function Geometric_Product (MV1, MV2 : Multivector) return Multivector;
    function Left_Contraction (V1, V2 : Vector_2D) return Scalar;
    function Left_Contraction (V1, V2 : Vector) return Scalar;
    function Left_Contraction (V : Vector_2D; BV : Bivector) return Vector_2D;
    function Outer_Product (V1, V2 : Vector_2D) return Bivector;
    function Scalar_Product (V1, V2 : GA_Maths.Vector_2D) return Scalar;
    function Set_Bivector (V1, V2 : Vector_2D) return Bivector;
    function Set_Rotor (E1_E2 : float) return Rotor;
    function Unit_E (V : Vector_2D) return Vector_2D;

    function Largest_Basis_Blade (Map : Bit_Map) return float;
--      function Largest_Coordinate return float;

end E2GA;
