
with GA_Draw;

package body E2GA_Draw is

    procedure Draw (Render_Program : GL.Objects.Programs.Program;
                    aVector : GA_Maths.Vector_2D; Colour : GL.Types.Colors.Color;
                    Scale : float := 1.0) is
    begin
        --  MV_Analysis (MV) declares A as a variable of class mvAnalysis constructed from v1
        GA_Draw.Draw_Vector (Render_Program, aVector, aVector, Colour);
    end Draw;

--  ----------------------------------------------------------------------------

    procedure Draw (Render_Program : GL.Objects.Programs.Program;
                         V2, V3, V4 : GA_Maths.Vector_2D;
                         Colour : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
                         Scale : float := 1.0) is
    begin
        GA_Draw.Draw_Quad (Render_Program, V2, V3, V4, Colour);
    end Draw;

--  ----------------------------------------------------------------------------

    procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   BV : E2GA.Bivector;
                   Colour : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
                   Scale : float := 1.0) is
    begin
        GA_Draw.Draw_Bivector (Render_Program, BV, Colour, Scale);
    end Draw;

--  --------------------------------------------------------------------------------

end E2GA_Draw;
