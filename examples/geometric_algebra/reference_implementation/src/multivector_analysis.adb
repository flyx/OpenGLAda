
package body Multivector_Analysis is

    Default_Epsilon_Value : float := 10.0 ** (-5);
    Number_Of_Points      : integer := 3;
    Number_Of_Vectors     : integer := 3;
    Number_Of_Scalars     : integer := 3;
    Number_Of_Type_Levels : integer := 3;

    M_Flags   : Flags_Type := (Flag_Invalid, false);
    M_Epsilon : Float := Default_Epsilon_Value;
    M_Points  : array (1 .. Number_Of_Points) of integer;
    M_Scalors : array (1 .. Number_Of_Scalars) of integer;
    --   Intended use of M_Type:
    --   m_type[0] = model
    --   m_type[1] = multivector type (c3ga_type : BLADE, c3ga_type : VERSOR,
    --               c3ga_type :  : MULTIVECTOR, )
    --   m_type[2] = class (round, flat, free, etc)
    --   m_type[3] = grade / class dependent
    M_Type             : array (1 .. Number_Of_Type_Levels) of
                                Multivector_Type_Base.M_Type_Type;
    M_Vectors          : array (1 .. Number_Of_Vectors) of integer;
    M_Multivector_Type : Multivector_Type_Base.Type_Base; --  m_mvType

 --  --------------------------------------------------------------------------

    procedure Analyze (MV : E2GA.Multivector; Flags : Flags_Type := (Flag_Invalid, false);
                       Epsilon : float := Default_Epsilon) is
    begin
        M_Flags := Flags;
        M_Epsilon := Epsilon;
    end Analyze;

 --  --------------------------------------------------------------------------

    function Default_Epsilon return float is
    begin
        return Default_Epsilon_Value;
    end Default_Epsilon;

 --  ---------------------------------------------------------------------------
    function Get_M_Flags return Flags_Type is
    begin
        return M_Flags;
    end Get_M_Flags;

 --  --------------------------------------------------------------------------

    function Get_Multivector_Type (MV : E2GA.Multivector; Epsilon : float)
                                   return Multivector_Type_Base.M_Type_Type is
    begin
        return M_Type (2);
    end Get_Multivector_Type;

 --  --------------------------------------------------------------------------

    function Get_M_Points (Index : Integer) return integer is
    begin
        return M_Points (Index);
    end Get_M_Points;

 --  --------------------------------------------------------------------------

    function Get_M_Scalors (Index : Integer) return integer is
    begin
        return M_Scalors (Index);
    end Get_M_Scalors;

 --  --------------------------------------------------------------------------

    function Get_M_Vectors (Index : Integer) return integer is
    begin
        return M_Vectors (Index);
    end Get_M_Vectors;

 --  --------------------------------------------------------------------------

    function Num_Points return integer is
    begin
        return Number_Of_Points;
    end Num_Points;

 --  --------------------------------------------------------------------------

    function Num_Vectors return integer is
    begin
        return Number_Of_Vectors;
    end Num_Vectors;

 --  --------------------------------------------------------------------------

    function Num_Scalars return integer is
    begin
        return Number_Of_Scalars;
    end Num_Scalars;

 --  --------------------------------------------------------------------------

    function Num_Type_Levels return integer is
    begin
        return Number_Of_Type_Levels;
    end Num_Type_Levels;

 --  --------------------------------------------------------------------------

    procedure Set_M_Flags (Valid, Dual : boolean) is
    begin
        M_Flags := (Valid, Dual);
    end Set_M_Flags;

 --  --------------------------------------------------------------------------

    procedure Set_M_Type (Index : Integer; Value : Multivector_Type_Base.M_Type_Type) is
    begin
        M_Type (Index) := Value;
    end Set_M_Type;

 --  --------------------------------------------------------------------------

    procedure Set_M_Multivector_Type (Current_MV_Type : Multivector_Type_Base.Type_Base) is
    begin
        M_Multivector_Type := Current_MV_Type;
    end Set_M_Multivector_Type;

 --  --------------------------------------------------------------------------

    procedure Set_M_Points (Index : Integer; Value : integer) is
    begin
        M_Points (Index) := Value;
    end Set_M_Points;

 --  --------------------------------------------------------------------------

    procedure Set_M_Scalors (Index : Integer; Value : integer) is
    begin
        M_Scalors (Index) := Value;
    end Set_M_Scalors;

 --  --------------------------------------------------------------------------

    procedure Set_M_Vectors (Index : Integer; Value : integer) is
    begin
        M_Vectors (Index) := Value;
    end Set_M_Vectors;

 --  --------------------------------------------------------------------------

end Multivector_Analysis;
