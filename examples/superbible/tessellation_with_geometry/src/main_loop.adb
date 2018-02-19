
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Rendering_Program : GL.Objects.Programs.Program;
    Vertex_Array      : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

    --  ------------------------------------------------------------------------

    procedure Render_Tessellation_Geometry is
        Back_Colour : constant GL.Types.Colors.Color := (0.0, 0.75, 0.0, 1.0);
    begin
        Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

        GL.Objects.Programs.Use_Program (Rendering_Program);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Patches, 0, 3);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Render_Tessellation_Geometry.");
            raise;
    end Render_Tessellation_Geometry;

    --  ------------------------------------------------------------------------

    procedure Setup_Graphic is
        use GL.Objects.Shaders;
    begin
        Rendering_Program := Program_Loader.Program_From
          ((Program_Loader.Src ("src/shaders/vertex_shader.glsl",
           Vertex_Shader),
           Program_Loader.Src ("src/shaders/geometry_shader.glsl",
             Geometry_Shader),
           Program_Loader.Src ("src/shaders/tessellation_control_shader.glsl",
             Tess_Control_Shader),
           Program_Loader.Src ("src/shaders/tessellation_evaluation_shader.glsl",
             Tess_Evaluation_Shader),
           Program_Loader.Src ("src/shaders/fragment_shader.glsl",
             Fragment_Shader)));

        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        GL.Rasterization.Set_Polygon_Mode (GL.Rasterization.Line);
        GL.Rasterization.Set_Point_Size (5.0);

        Utilities.Show_Shader_Program_Data (Rendering_Program);
    end Setup_Graphic;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup_Graphic;
    while Running loop
        Render_Tessellation_Geometry;
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and then not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) =
               Glfw.Input.Pressed);
        Running := Running and then not Main_Window.Should_Close;
    end loop;
end Main_Loop;
