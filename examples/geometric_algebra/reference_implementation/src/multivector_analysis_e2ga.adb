
with Multivector_Analysis; use Multivector_Analysis;
with Multivector_Type_Base; use Multivector_Type_Base;

package body Multivector_Analysis_E2GA is

    procedure Analyze (MV : in out E2GA.Multivector; Flags : Flags_Type := (Flag_Invalid, false);
                       Epsilon : float := Default_Epsilon) is
        Current_Flags : Flags_Type := Get_M_Flags;
    begin
        for Index in 0 .. Num_Type_Levels loop
            Set_M_Type (Index, Invalid_Type);
        end loop;
        Set_M_Type (1, Vector_Space_Model);
        Set_M_Type (3, Unused_Type);
        if Flags.Dual then
            Set_M_Flags (Flags.Valid, Current_Flags.Dual xor Flags.Dual);
            MV := E2GA.Dual (MV);
        end if;
        Set_M_Multivector_Type (MV_Index, Get_Multivector_Type (MV, Epsilon));
    end Analyze;

end Multivector_Analysis_E2GA;
