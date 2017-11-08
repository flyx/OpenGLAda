
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Culling;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Rendering_Program : GL.Objects.Programs.Program;
    Vertex_Array      : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

    --  ------------------------------------------------------------------------

    procedure Render_Triangle (Current_Time : Glfw.Seconds) is
        use Maths.Single_Math_Functions;
        Now         : constant Single := Single (Current_Time);
        Back_Colour : constant GL.Types.Colors.Color :=
          (0.5 * (1.0 + Sin (Now)), 0.5 * (1.0 + Cos (Now)), 0.0, 1.0);
        Offset      : constant GL.Types.Singles.Vector4 :=
          (0.5 * Sin (Now), 0.5 * Cos (Now), 0.0, 0.0);
    begin
        Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

        GL.Objects.Programs.Use_Program (Rendering_Program);

        GL.Attributes.Set_Single (0, Offset);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 3);
    exception
        when others => Put_Line ("Exception in Render_Triangle");
            raise;
    end Render_Triangle;

    --  ------------------------------------------------------------------------

    procedure Setup_Graphic is
        use Program_Loader;
        use GL.Objects.Shaders;
    begin
        Rendering_Program := Program_From
          ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Toggles.Enable (GL.Toggles.Cull_Face);
        GL.Culling.Set_Front_Face (GL.Types.Clockwise);
        GL.Culling.Set_Cull_Face (GL.Culling.Back);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);

        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        Utilities.Show_Shader_Program_Data (Rendering_Program);
    end Setup_Graphic;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running  : Boolean := True;
begin
    Setup_Graphic;
    while Running loop
        Render_Triangle (Glfw.Time);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and then not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and then not Main_Window.Should_Close;
    end loop;

end Main_Loop;
