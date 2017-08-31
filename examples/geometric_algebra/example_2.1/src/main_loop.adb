
with Ada.Numerics;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders.Lists;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with GA_Draw;
with E2GA;
with E2GA_Draw;
with GA_Maths;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
    subtype tVec4f is Singles.Vector4;

    Red     : Colors.Color := (1.0, 0.0, 0.0, 1.0);
    Green   : Colors.Color := (0.0, 1.0, 0.0, 1.0);
    Blue    : Colors.Color := (0.0, 0.0, 1.0, 1.0);

    Rendering_Program       : GL.Objects.Programs.Program;
    Vertices_Array_Object   : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

--      Quad_Vertices : GA_Maths.Array_2D (1 .. 4) :=
--                           ((-0.2, -0.2),
--                            (-0.2, 0.2),
--
--                            (0.2, 0.2),
--                            (0.2, -0.2));

--      function To_GL (V : GA_Maths.GA_Maths.Vector_2D) return GL.Types.GA_Maths.Vector_2D;

    --  ----------------------------------------------------------------------------

--      procedure Generate_Bivector (A : float; B : out E2GA_Basic.Bivector;
--                                   V1, V2, V1_Plus_V2 : out GA_Maths.GA_Maths.Vector_2D) is
--      --  Vector_Space : e2ga;
--          use Maths.Single_Math_Functions;
--          use GA_Maths;
--          use GA_Maths.Float_Functions;
--          e1           : GA_Maths.Vector_2D := E2GA_Basic.e (1);
--          e2           : GA_Maths.Vector_2D := E2GA_Basic.e (2);
--      begin
--          V1 := e1;
--          V2 := Cos (A) * e1 + Sin (A) * e2;
--          V1_Plus_V2 := V1 + V2;
--          B := E2GA_Basic.Set_Bivector (V1, V2);
--      end Generate_Bivector;

    --  ----------------------------------------------------------------------------

    procedure Render_Bivectors (Draw_Parallelogram : boolean := True) is
        use GL.Objects.Buffers;

        use Maths.Single_Math_Functions;
        use GA_Maths;
        use GA_Maths.Float_Functions;

        Back_Colour : constant Colors.Color := (0.6, 0.6, 0.6, 1.0);

	--  How many bivectors? what spacing between them?:
	EntryWidth      : constant float := 2.75;
	EntryHeight     : constant float := 3.5;
	Num_Bivector_X  : constant integer := 6;
	Num_Bivector_Y  : constant integer := 4;
        Two_Pi          : constant float := 2.0 * Ada.Numerics.Pi;
        A               : float := 0.01;
        B               : E2GA.Bivector;
        Step            : float := Two_Pi / float (Num_Bivector_X * Num_Bivector_Y);
        V1              : GA_Maths.Vector_2D := E2GA.e1;
        V2              : GA_Maths.Vector_2D;
        V1_Plus_V2      : GA_Maths.Vector_2D;

    begin
        Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

        GL.Objects.Programs.Use_Program (Rendering_Program);

        V1 := E2GA.e1;
        while A <= Two_Pi loop
            V2 := Cos (A) * E2GA.e1 + Sin (A) * E2GA.e2;
            E2GA_Draw.Draw (Rendering_Program, V1, Red);
            E2GA_Draw.Draw (Rendering_Program, V2, Green);
            if Draw_Parallelogram then
                GL.Toggles.Disable (GL.Toggles.Cull_Face);
                V1_Plus_V2 := V1 + V2;
                --  Draw Quad with vertices: origin -> V1 -> V1+V2 -> V2
                E2GA_Draw.Draw (Rendering_Program, V1, V1 + V2, V2);
            else
                B := E2GA.Outer_Product (V1, V2);
                E2GA_Draw.Draw (Rendering_Program, B);
            end if;
            A := A + Step;
        end loop;

    exception
        when anError :  others =>
            Put_Line ("An exception occurred in Render_Bivectors.");
            raise;
    end Render_Bivectors;

    --  ----------------------------------------------------------------------------

    procedure Setup_Graphic is
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
        use Program_Loader;
    begin
        Rendering_Program := Program_Loader.Program_From
          ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
            Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));
         GL.Toggles.Enable (GL.Toggles.Depth_Test);
         GL.Toggles.Enable (GL.Toggles.Cull_Face);
        --          GL.Culling.Set_Front_Face (GL.Types.Clockwise);
        --          GL.Culling.Set_Cull_Face (GL.Culling.Back);
        --          GL.Buffers.Set_Depth_Function (GL.Types.Less);
        GA_Draw.Set_Point_Size (0.005);
        Vertices_Array_Object.Initialize_Id;
        Vertices_Array_Object.Bind;

    end Setup_Graphic;

    --  ----------------------------------------------------------------------------

    function To_GL (V : GA_Maths.Vector_2D) return GL.Types.Singles.Vector2 is
        Coord1 : float := GA_Maths.Get_Coord_1 (V);
        Coord2 : float := GA_Maths.Get_Coord_2 (V);
    begin
        return (Single (Coord1), Single (Coord2));
    end;

    --  ----------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup_Graphic;
    while Running loop
        Render_Bivectors;
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and then
          not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and then not Main_Window.Should_Close;
    end loop;
end Main_Loop;
