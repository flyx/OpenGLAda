
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Errors;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    package Math_Functions is new
      Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);
    subtype tVec4f is GL.Types.Singles.Vector4;

    Rendering_Program : GL.Objects.Programs.Program;
    Vertex_Array      : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

    --  ------------------------------------------------------------------------

    procedure Render_Tesselation is
        Back_Colour : constant GL.Types.Colors.Color := (0.0, 0.75, 0.0, 1.0);
    begin
        GL.Buffers.Clear ((True, False, False, True));
        GL.Buffers.Set_Color_Clear_Value (Back_Colour);

        GL.Objects.Programs.Use_Program (Rendering_Program);
        GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Patches, 0, 3);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Render_Tesselation.");
            raise;
    end Render_Tesselation;

    --  ------------------------------------------------------------------------

    procedure Setup_Graphic is
        use GL.Objects.Shaders;
    begin
        Rendering_Program := Program_Loader.Program_From
          ((Program_Loader.Src ("src/shaders/vertex_shader.glsl",
           Vertex_Shader),
           Program_Loader.Src ("src/shaders/tesselation_control_shader.glsl",
             Tess_Control_Shader),
           Program_Loader.Src ("src/shaders/tesselation_evaluation_shader.glsl",
             Tess_Evaluation_Shader),
           Program_Loader.Src ("src/shaders/fragment_shader.glsl",
             Fragment_Shader)));

        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        GL.Rasterization.Set_Polygon_Mode (GL.Rasterization.Line);

        Utilities.Show_Shader_Program_Data (Rendering_Program);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup_Graphic.");
            raise;
    end Setup_Graphic;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup_Graphic;
    while Running loop
        Render_Tesselation;
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) =
               Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;

exception
    when others =>
        Put_Line ("An exceptiom occurred in Main_Loop.");
        raise;
end Main_Loop;
