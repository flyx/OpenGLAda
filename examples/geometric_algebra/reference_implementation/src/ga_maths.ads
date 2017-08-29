
with Interfaces;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Real_Arrays;

package GA_Maths is
    package Float_Array_Package is new Ada.Numerics.Generic_Real_Arrays (float);
    package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (Float);

    type Unsigned_Integer is mod 2 ** 32;
--      type Vector_Type is (Vector_e1_e2, Vector_e1_e2_e3);
    type Vector_Unsigned_3D is array (1 .. 3) of Interfaces.Unsigned_32;

    subtype Basis_Index is Integer range 1 .. 2;
    subtype Float_Matrix is Float_Array_Package.Real_Matrix;

    type Scalar is new float;
--      type Scalar_Coords is (Scalar_Scalar);
--      type Scalar is record
--      --    Coord_Type : Scalar_Coords := Scalar_Scalar;
--          M_C1       : float;
--      end record;
    type Vector_Unsigned is record
        C1_e1   : Interfaces.Unsigned_32;
        C2_e2   : Interfaces.Unsigned_32;
        C3_e3   : Interfaces.Unsigned_32;
    end record;

    --  Vector corresponds to e3ga.vector coordinate storage float m_c[3]
    type Vector is private;

    --  Vector_2D corresponds to e2ga.vector coordinate storage float m_c[2]
    type Vector_2D is private;

    type Array_2D is array (1 .. 2) of float;
    type Array_3D is array (1 .. 3) of float;
    type Array_4f is array (1 .. 4) of float;
    type Coord4_Array is  Array (1 .. 4) of float;

    function "+" (V1, V2 : Vector_2D) return Vector_2D;
    function "*" (Weight : float; V : Vector_2D) return Vector_2D;
    function "*" (V1, V2 : Vector_2D) return Vector_2D;

    function "+" (V1, V2 : Vector) return Vector;
    function "*" (Weight : float; V : Vector) return Vector;

    function Canonical_Reordering_Sign (Map_A, Map_B : integer) return float;
    function Get_Coord_1 (V : Vector) return float;
    function Get_Coord_2 (V : Vector) return float;
    function Get_Coord_3 (V : Vector) return float;
    function Get_Coords (V : Vector_2D) return Array_2D;
    function Get_Coords (V : Vector) return Array_3D;
    function Get_Coord_1 (V : Vector_2D) return float;
    function Get_Coord_2 (V : Vector_2D) return float;
    function Get_Unsigned_Coords (V : Vector) return Vector_Unsigned;
    function Dot_Product (V1, V2 : Vector_2D) return float;
    function Dot_Product (V1, V2 : Vector) return float;

    procedure Set_Coords (V : out Vector_2D; C1, C2 : float);
    procedure Set_Coords (V : out Vector; C1, C2, C3 : float);
    function To_Unsigned (V : Vector) return Vector_Unsigned;

private
    --  Vector_2D corresponds to e2ga.vector coordinate storage float m_c[2]
    type Vector_2D is record
    --    Vec_type : Vector_Type := Vector_e1_e2;
        C1_e1    : float;
        C2_e2    : float;
    end record;

    --  Vector corresponds to e3ga.vector coordinate storage float m_c[3]
    type Vector is record
   --     Vec_Type : Vector_Type := Vector_e1_e2_e3;
        C1_e1    : float;
        C2_e2    : float;
        C3_e3    : float;
    end record;

end GA_Maths;
