
package Multivector_Type_Base is

    type M_Type_Type is (Invalid_Type, Unused_Type, Zero, Vector_Space_Model, Blade,
                         Bivector, Trivector, Even_Versor);
    type Object_Type is (Multivector, Versor, Blade);
    type Parity is (None, Even, Odd);
    type Type_Base is private;

    function Get_Type_Base return Type_Base;
    procedure Set_Type_Base (Zero : boolean; Object : Object_Type;
                             Grade : integer; Grade_Usage : integer;
                             Par : Parity := None);
    procedure Set_M_Type (M_Type : Object_Type);
private
    type Type_Base is record
        M_Zero        : boolean := False; -- True if multivector is zero
        M_Type        : Object_Type := Multivector;
        M_Top_Grade   : integer := -1;  --  Top grade occupied by the multivector
        M_Grade_Usage : integer := 0;   --  Bit map indicating which grades are present
        M_Parity      : Parity := None;
    end record;

    type Flag is (Valid);

end Multivector_Type_Base;
