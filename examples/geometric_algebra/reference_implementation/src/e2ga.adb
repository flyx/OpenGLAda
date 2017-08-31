
with Interfaces;

with Multivector_Analysis;
with Multivector_Type_Base;

package body E2GA is

    e1_basis : GA_Maths.Array_2D := (1.0, 0.0);
    e2_basis : GA_Maths.Array_2D := (0.0, 1.0);

    --  Construct_Vector corresponds to e2ga._vector
    function Construct_Vector (Coord : float) return GA_Maths.Vector_2D is
        use GA_Maths;
        theVector : Vector_2D;
    begin
        Set_Coords (theVector, Coord, 0.0);
	return theVector;
    end Construct_Vector;

--  ----------------------------------------------------------------------------

    function Construct_Vector (Coord_1, Coord_2 : float) return GA_Maths.Vector_2D is
        use GA_Maths;
        theVector : Vector_2D;
    begin
        Set_Coords (theVector, Coord_1, Coord_2);
	return theVector;
    end Construct_Vector;

--  -------------------------------------------------------------------------

    function Basis_Vector_Name (Index : Basis_Index) return String is
    begin
        case Index is
            when 1 => return "e1";
            when 2 => return "e2";
        end case;
    end Basis_Vector_Name;

--  ----------------------------------------------------------------------------

    function Dual (MV : Multivector) return Multivector is
        Coords : Coords_Continuous_Array (1 .. 4) := (0.0, 0.0, 0.0, 0.0);
    begin
        if (MV.Grade_Usage and 1) /= 0 then
            Coords (4) := -MV.Coordinates (1);
        end if;
        if (MV.Grade_Usage and 2) /= 0 then
            Coords (2) := MV.Coordinates (3);
            Coords (3) := -MV.Coordinates (2);
        end if;
        if (MV.Grade_Usage and 4) /= 0 then
            Coords (1) := MV.Coordinates (4);
        end if;
        return (MV.Size, MV.Grade_Usage, Coords);
    end Dual;

--  ----------------------------------------------------------------------------

    function e1 return GA_Maths.Vector_2D is
        use GA_Maths;
        theVector : Vector_2D;
    begin
        Set_Coords (theVector, e1_basis (1), e1_basis (2));
        return theVector;
    end e1;

--  ----------------------------------------------------------------------------

    function e2 return GA_Maths.Vector_2D is
        use GA_Maths;
        theVector : Vector_2D;
    begin
        Set_Coords (theVector, e2_basis (1), e2_basis (2));
        return theVector;
    end e2;

--  ----------------------------------------------------------------------------

    function  Get_MV_Type (X : Multivector; Epsilon : float)
                           return Multivector_Type_Base.M_Type_Type is
    begin
        return Multivector_Analysis.Get_Multivector_Type (X, Epsilon);
    end Get_MV_Type;

--  ----------------------------------------------------------------------------

    function Norm_E2 (BV : Bivector) return Scalar is
    begin
        return Scalar (BV.e1e2_Coord * BV.e1e2_Coord);
    end Norm_E2;

--  ----------------------------------------------------------------------------

    function Norm_E2 (V2 : GA_Maths.Vector_2D) return Scalar is
        V21 : float := GA_Maths.Get_Coord_1 (V2);
        V22 : float := GA_Maths.Get_Coord_2 (V2);
    begin
        return Scalar (V21 * V21 + V22 * V22);
    end Norm_E2;

--  ----------------------------------------------------------------------------

    function Norm_E (MV : Multivector) return Scalar is
        Value : float := 0.0;
    begin
        if (MV.Grade_Usage and 1) /= 0 then
            Value := MV.Coordinates (1) * MV.Coordinates (1);
        end if;
        if (MV.Grade_Usage and 2) /= 0 then
            Value := Value + MV.Coordinates (2) * MV.Coordinates (2) +
                     MV.Coordinates (3) * MV.Coordinates (3);
        end if;
        if (MV.Grade_Usage and 4) /= 0 then
            Value := Value + MV.Coordinates (4) * MV.Coordinates (4);
        end if;

        return Scalar (Value);
    end Norm_E;

--  ----------------------------------------------------------------------------

    function Norm_E2 (MV : Multivector) return Scalar is
        Value : float := 0.0;
    begin
        if (MV.Grade_Usage and 1) /= 0 then
            Value := MV.Coordinates (1) * MV.Coordinates (1);
        end if;
        if (MV.Grade_Usage and 2) /= 0 then
            Value := Value + MV.Coordinates (2) * MV.Coordinates (2) +
                     MV.Coordinates (3) * MV.Coordinates (3);
        end if;
        if (MV.Grade_Usage and 4) /= 0 then
            Value := Value + MV.Coordinates (4) * MV.Coordinates (4);
        end if;

        return Scalar (Value);
    end Norm_E2;

--  ----------------------------------------------------------------------------

    function Largest_Basis_Blade (Map : Bit_Map) return float is
    begin
        return 0.0;
    end Largest_Basis_Blade;

--  ----------------------------------------------------------------------------
    --  Compress is not intended for external use. It compresses the coordinates stored in this
--      function Compress (A : Array_4f; epsilon : float := 0.0;
--                         Grade_Use : integer := 0) return Multivector is
--          CC          : Coord4_Array;
--          C_Grade_Use : Unsigned_Integer;
--      begin
--          return (C_Grade_Use, CC);
--      end Compress;

--  ------------------------------------------------------------------------

--      function  Compress (Num_Blades : integer; Maps : Bit_Map_Array;
--                          Coordinates : Coords_Continuous_Array)
--                          return Multivector is
--          A : Array_4f := (0.0, 0.0, 0.0, 0.0);
--      begin
--          for i in 0 .. Num_Blades loop
--             A (MV_Basis_Element_Index_By_Bit_Map (Maps (i))) := Coordinates (i) *
--             float (MV_Basis_Element_Sign_By_Bit_Map (Maps (i)));
--          end loop;
--          return Compress (A);
--      end Compress;

    --  ------------------------------------------------------------------------
    --  Expand is not for external use. It decompresses the coordinates stored in this
    function  Expand (MV : Multivector; Nulls : boolean := True)
                             return Array_3D is
--          C_M_C : float;
--          Null_Float : float;
    begin
        return (MV.Coordinates (1), MV.Coordinates (2), MV.Coordinates (3));
    end Expand;

    --  ------------------------------------------------------------------------

    function Left_Contraction (V1, V2 : Vector_2D) return Scalar is
    begin
        return Scalar (Dot_Product (V1, V2));
    end Left_Contraction;

    --  ------------------------------------------------------------------------

    function Left_Contraction (V : Vector_2D; BV : Bivector) return Vector_2D is
        use GA_Maths;
        Coord_Array : array (1 .. 2) of float := (0.0, 0.0);
        VC1  : float := Get_Coord_1 (V);
        VC2  : float := Get_Coord_2 (V);
        Vout : Vector_2D;
    begin
        if (BV.Grade_Usage and 1) /= 0 then
                Coord_Array (1) := VC1 * BV.e1e2_Coord;
        end if;
        if (BV.Grade_Usage and 2) /= 0 then
                Coord_Array (2) :=  VC2 * BV.e1e2_Coord;
        end if;
        Set_Coords (Vout, Coord_Array (1), Coord_Array (2));
        return Vout;
    end Left_Contraction;

    --  ------------------------------------------------------------------------

    function Left_Contraction (V1, V2 : Vector) return Scalar is
    begin
        return Scalar (Dot_Product (V1, V2));
    end Left_Contraction;

    --  ------------------------------------------------------------------------

    function Geometric_Product (MV1, MV2 : Multivector) return Multivector is
        use Interfaces;
        Coords : Coord4_Array := (0.0, 0.0, 0.0, 0.0);
        MV1_Expand : Array_3D := Expand (MV1, True);
        MV2_Expand : Array_3D := Expand (MV2, True);
    begin
        --  m_gu: Grade_Usage
        --  m_c[4]: Coords
        --  Expand decompresses the coordinates stored in a multivector
        if (MV1.Grade_Usage and Unsigned_Integer (1)) /= 0 then
            Coords (1) := Coords (1) + MV1_Expand (1) * MV2_Expand (1);
        end if;
        if (MV1.Grade_Usage and Unsigned_Integer (2)) /= 0 then
            Coords (2) := Coords (2) + MV1_Expand (2) * MV2_Expand (1);
            Coords (3) := Coords (3) + MV1_Expand (2) * MV2_Expand (1);
        end if;

        return MV2;
    end Geometric_Product;

    --  ------------------------------------------------------------------------

    function BV_Geometric_Product (MV1, MV2 : Multivector) return Multivector is
    begin

        return MV2;
    end BV_Geometric_Product;

    --  ------------------------------------------------------------------------

    function Outer_Product (V1, V2 : Vector_2D) return Bivector is
        --  The outer product basis in 2D is the coordinate of e1^e2.
        use Float_Array_Package;
        use GA_Maths;
        V11  : float := Get_Coord_1 (V1);
        V12  : float := Get_Coord_2 (V1);
        V21  : float := Get_Coord_1 (V2);
        V22  : float := Get_Coord_2 (V2);
    begin
        return (2, Determinant (((V11, V21), (V12, V22))));
    end Outer_Product;

    --  ------------------------------------------------------------------------
--      underscore constructor from general multivector:
--  	inline rotor _rotor(const mv &arg1) {
--  		return rotor(arg1, 0);
--  	}
--  	from const specialization class:
--  	inline const rotor &_rotor(const rotor &arg1) {
--  		return arg1;
--  	}
--  	from non-const specialization class:
--  	inline rotor &_rotor(rotor &arg1) {
--  		return arg1;
--  	}
--  	 from Float:
--  	inline rotor _rotor(rotor::Float arg1) {
--  		return rotor(arg1, 0); // 0 = filler; can have any value,
--                   it simply prevents implicit conversion from mv -> vector
--  	}

    function Scalar_Product (V1, V2 : GA_Maths.Vector_2D) return Scalar is
    begin
        return  Scalar (Dot_Product (V1, V2));
    end Scalar_Product;

--  ----------------------------------------------------------------------------

    function Set_Bivector (V1, V2 : Vector_2D) return Bivector is
    begin
        return  Outer_Product (V1, V2);
    end Set_Bivector;

--  ----------------------------------------------------------------------------

    function Set_Rotor (E1_E2 : float) return Rotor is
    begin
        return  (E1_E2, 0.0);
    end Set_Rotor;

--  ----------------------------------------------------------------------------

    function Unit_E (V : Vector_2D) return Vector_2D is
        E2_Value   : float;
        IE_Value   : float;
        use GA_Maths;
        V1    : float := Get_Coord_1 (V);
        V2    : float := Get_Coord_2 (V);
        e21    : float := Get_Coord_1 (e2);
        Result : Vector_2D;
    begin
        E2_Value := V1 *  V1 + V2 *  V2;
        IE_Value := 1.0 / Float_Functions.Sqrt (e21);
        Set_Coords (Result, V1 * IE_Value, V2 * IE_Value);
        return  Result;
    end Unit_E;

--  ----------------------------------------------------------------------------

end E2GA;
