
with Interfaces;

package body GA_Maths is

    function "+" (V1, V2 : Vector_2D) return Vector_2D is
    begin
        return (V1.C1_e1 + V2.C1_e1, V1.C2_e2 + V2.C2_e2);
    end "+";

    --  ------------------------------------------------------------------------

    function "+" (V1, V2 : Vector) return Vector is
    begin
        return (V1.C1_e1 + V2.C1_e1,
                V1.C2_e2 + V2.C2_e2,
                V1.C3_e3 + V2.C3_e3);
    end "+";

    --  ------------------------------------------------------------------------

    function "*" (Weight : float; V : Vector_2D) return Vector_2D is
    begin
        return (Weight * V.C1_e1, Weight * V.C2_e2);
    end "*";

    --  ------------------------------------------------------------------------

    function "*" (V1, V2 : Vector_2D) return Vector_2D is
    begin
        return (V1.C1_e1 * V2.C1_e1, V1.C2_e2 * V2.C2_e2);
    end "*";

    --  ------------------------------------------------------------------------

    function "*" (Weight : float; V : Vector) return Vector is
    begin
        return (Weight * V.C1_e1, Weight * V.C2_e2, Weight * V.C3_e3);
    end "*";

    --  ------------------------------------------------------------------------

    function Bit_Count (Value : Interfaces.Unsigned_32) return Natural is
    begin
        return 0;
    end Bit_Count;

    --  ------------------------------------------------------------------------

    function Canonical_Reordering_Sign (Map_A, Map_B : integer) return float is
        use Interfaces;
        A     : Unsigned_32 := Shift_Right (Unsigned_32 (Map_A), 1);
        B     : Unsigned_32 := Unsigned_32 (Map_B);
        Swaps : Natural := 0;
    begin
        while A /= 0 loop
            Swaps := Swaps + Bit_Count (A and B);
            A := Shift_Right (Unsigned_32 (A), 1);
        end loop;

        if Swaps mod 2 = 0 then  -- an even number of swaps
            return 1.0;
        else  -- an odd number of swaps
            return -1.0;
        end if;
    end Canonical_Reordering_Sign;

    --  ------------------------------------------------------------------------

    function Dot_Product (V1, V2 : Vector_2D) return float is
    begin
        return V1.C1_e1 * V2.C1_e1 + V1.C2_e2 * V2.C2_e2;
    end Dot_Product;

    --  ------------------------------------------------------------------------

    function Dot_Product (V1, V2 : Vector) return float is
    begin
        return V1.C1_e1 * V2.C1_e1 + V1.C2_e2 * V2.C2_e2 +
               V1.C3_e3 * V2.C3_e3;
    end Dot_Product;

    --  ------------------------------------------------------------------------

    function Get_Coords (V : Vector_2D) return Array_2D is
    begin
        return (V.C1_e1, V.C2_e2);
    end Get_Coords;

    --  ------------------------------------------------------------------------

    function Get_Coords (V : Vector) return Array_3D is
    begin
        return (V.C1_e1, V.C2_e2, V.C3_e3);
    end Get_Coords;

    --  ------------------------------------------------------------------------

    function Get_Coord_1 (V : Vector_2D) return float is
    begin
        return V.C1_e1;
    end Get_Coord_1;

    --  ------------------------------------------------------------------------

    function Get_Coord_2 (V : Vector_2D) return float is
    begin
        return V.C2_e2;
    end Get_Coord_2;

    --  ------------------------------------------------------------------------

    function Get_Coord_1 (V : Vector) return float is
    begin
        return V.C1_e1;
    end Get_Coord_1;

    --  ------------------------------------------------------------------------

    function Get_Coord_2 (V : Vector) return float is
    begin
        return V.C2_e2;
    end Get_Coord_2;

    --  ------------------------------------------------------------------------

    function Get_Coord_3 (V : Vector) return float is
    begin
        return V.C3_e3;
    end Get_Coord_3;

    --  ------------------------------------------------------------------------

    function Get_Unsigned_Coords (V : Vector) return Vector_Unsigned is
    begin
        return To_Unsigned (V);
    end Get_Unsigned_Coords;

    --  ------------------------------------------------------------------------

    procedure Set_Coords (V : out Vector_2D; C1, C2 : float) is
    begin
        V := (C1, C2);
    end Set_Coords;

    --  ------------------------------------------------------------------------

    procedure Set_Coords (V : out Vector; C1, C2, C3 : float) is
    begin
        V := (C1, C2, C3);
    end Set_Coords;

    --  ------------------------------------------------------------------------

    function To_Unsigned (V : Vector) return Vector_Unsigned is
        use Interfaces;
    begin
        return (Unsigned_32 (V.C1_e1), Unsigned_32 (V.C2_e2),
                Unsigned_32 (V.C3_e3));
    end To_Unsigned;

    --  ------------------------------------------------------------------------

end GA_Maths;
