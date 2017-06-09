
package body Multivector_Type_Base is

    Current_Type_Base : Type_Base;

    function Get_Type_Base return Type_Base is
    begin
        return Current_Type_Base;
    end Get_Type_Base;

--  ----------------------------------------------------------------------------

    procedure Set_M_Type (M_Type : Object_Type) is
    begin
       Current_Type_Base.M_Type := M_Type;
    end Set_M_Type;

--  ----------------------------------------------------------------------------

    procedure Set_Type_Base (Zero : boolean; Object : Object_Type;
                             Grade : integer; Grade_Usage : integer;
                             Par : Parity := None) is
    begin
       Current_Type_Base.M_Zero := Zero;
       Current_Type_Base.M_Type := Object;
       Current_Type_Base.M_Top_Grade := Grade;
       Current_Type_Base.M_Grade_Usage := Grade_Usage;
       Current_Type_Base.M_Parity := Par;
    end Set_Type_Base;

--  ----------------------------------------------------------------------------

end Multivector_Type_Base;
