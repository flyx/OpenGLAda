
with GL.Types.Colors;
with GL.Objects.Programs;

with E2GA;
with GA_Maths;

package E2GA_Draw is
    --  procedure Draw (MV : E2GA.Multivector, Method : Integer := 0;
    --                  Palet : Integer'access := null);
    --  Method is dependent on what 'X' represents.
    --  It is forwarded to drawVector, drawBivector, drawTrivector.
    --  Currently, 'method' is some integer in range [0, n), where each
    --  integer indicates a different way of drawing the multivector.
    --  Palet can be used to specify foreground, background and outline color.
    --  Uses g_drawState for some extra flags or allow DrawState to be
    --  passed as an argument (and also integrate 'Palet')
    procedure Draw (Render_Program : GL.Objects.Programs.Program;
                    aVector : GA_Maths.Vector_2D; Colour : GL.Types.Colors.Color;
                    Scale : float := 1.0);
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   V2, V3, V4 : GA_Maths.Vector_2D;
                   Colour : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
                   Scale : float := 1.0);
   procedure Draw (Render_Program : GL.Objects.Programs.Program;
                   BV : E2GA.Bivector;
                   Colour : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
                   Scale : float := 1.0);

end E2GA_Draw;
