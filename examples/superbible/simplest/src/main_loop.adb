
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Toggles;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Rendering_Program  : GL.Objects.Programs.Program;
    Vertex_Array       :  GL.Objects.Vertex_Arrays.Vertex_Array_Object;

    procedure Render_Dot (Current_Time : Glfw.Seconds) is
        use GL.Types;
        use Maths.Single_Math_Functions;

        Back_Colour : constant GL.Types.Colors.Color :=
          (0.5 * (1.0 + Sin (Single (Current_Time))),
           0.5 * (1.0 + Cos (Single (Current_Time))), 0.0, 1.0);
    begin
        Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

        GL.Objects.Programs.Use_Program (Rendering_Program);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Render_Dot.");
            raise;
    end Render_Dot;

--  ----------------------------------------------------------------------------

    procedure Setup_Graphic is
        use Program_Loader;
        use GL.Objects.Shaders;
    begin
        Rendering_Program := Program_From (
          (Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader))
        );
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        -- Point size is set in the vertex shader
        GL.Toggles.Enable (GL.Toggles.Vertex_Program_Point_Size);
        Utilities.Show_Shader_Program_Data (Rendering_Program);
    end Setup_Graphic;

--  ----------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup_Graphic;
    while Running loop
        Render_Dot (Glfw.Time);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and not
            (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;
exception
    when Program_Loader.Shader_Loading_Error =>
        -- message was already written to stdout
        null;
end Main_Loop;
