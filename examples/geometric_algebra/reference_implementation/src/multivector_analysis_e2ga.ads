
with E2GA;
with Multivector_Analysis;

package Multivector_Analysis_E2GA is

    procedure Analyze (MV : in out E2GA.Multivector; Flags : FlagsType := (Flag_Invalid, false);
                       Epsilon : float := Multivector_Analysis.Default_Epsilon);

end Multivector_Analysis_E2GA;
