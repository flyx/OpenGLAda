
with Interfaces;

with E2GA;

package body E3GA_Utilities is

--  ----------------------------------------------------------------------------

    function Rotor_Vector_To_Vector (V_From, V_To : GA_Maths.Vector) return E3GA.Rotor is
        use Interfaces;
        use GA_Maths;
        use GA_Maths.Float_Functions;
        use E3GA;
        V1     : Vector_Unsigned := To_Unsigned (V_From);
        V2     : Vector_Unsigned := To_Unsigned (V_To);
        C1     : float;
        S      : float;
        w0     : Vector;
        w1     : Vector;
        w2     : Vector;
        N2     : Scalar;
        Result : Rotor;
    begin
        Set_Coords (w0, 0.0, 0.0, 0.0);
        Set_Coords (w1, 0.0, 0.0, 0.0);
        Set_Coords (w2, 0.0, 0.0, 0.0);
      if float (Scalar_Product (V_From, V_To)) < -0.9 then
         C1 := GA_Maths.Get_Coord_1 (Left_Contraction (V_From, Outer_Product (V_From, V_To)));
         Set_Coords (w0, C1, 0.0, 0.0);
         N2 := Norm_E2 (w0);
      end if;

        if N2 = 0.0 then
            C1 := GA_Maths.Get_Coord_1 (Left_Contraction (V_From, Outer_Product (V_From, e1)));
            Set_Coords (w1, C1, 0.0, 0.0);
            C1 := GA_Maths.Get_Coord_1 (Left_Contraction (V_From, Outer_Product (V_From, e2)));
            Set_Coords (w2, C1, 0.0, 0.0);
            if Norm_E2 (w1) > Norm_E2 (w2) then
                Set_Rotor (Result, Outer_Product (V_From, Unit_e (w1)));
            else
                Set_Rotor (Result, Outer_Product (V_From, Unit_e (w2)));
            end if;
        else
           --  Replace V1 with -V1 and additional 180 degree rotation.
            S := Sqrt (2.0 * float (1.0 - Left_Contraction (V_To, V_From)));
            Set_Rotor (Result, (1.0 - Dot_Product (V_To, V_From)) / S *
                      Outer_Product (V_From, Unit_e (w0)));
        end if;
        return Result;
    end Rotor_Vector_To_Vector;

end E3GA_Utilities;
