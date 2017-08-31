with E2GA;
with Multivector_Type_Base;

package Multivector_Analysis is
    Flag_Invalid : constant boolean := false;
    type Flags_Type is record
        Valid : boolean := Flag_Invalid;
        Dual  : boolean := false;
    end record;
    type Model is (Vector_Space, Homogenous, Conformal);
    type Flats is (Point, Line, Plane);
    type Rounds is (Point_Pair, Circle, Sphere);
    type Multivector_Type is (Vector, Bivector, Trivector);
    type Versor_Type is (Even, Odd, Rotor);

    function Default_Epsilon return float;
    procedure Analyze (MV : E2GA.Multivector; Flags : Flags_Type := (Flag_Invalid, false);
                       Epsilon : float := Default_Epsilon);
    function Get_M_Flags return Flags_Type;
    function Get_Multivector_Type (MV : E2GA.Multivector; Epsilon : float)
                                   return Multivector_Type_Base.M_Type_Type;
    function Get_M_Points (Index : Integer) return integer;
    function Get_M_Scalors (Index : Integer) return integer;
    function Get_M_Vectors (Index : Integer) return integer;
    function Num_Points return integer;
    function Num_Vectors return integer;
    function Num_Scalars return integer;
    function Num_Type_Levels return integer;
    procedure Set_M_Flags (Valid, Dual : boolean);
    procedure Set_M_Type (Index : Integer; Value : Multivector_Type_Base.M_Type_Type);
    procedure Set_M_Multivector_Type (Current_MV_Type : Multivector_Type_Base.Type_Base);

end Multivector_Analysis;
