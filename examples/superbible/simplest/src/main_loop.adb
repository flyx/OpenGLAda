
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Toggles;
with GL.Errors;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Rasterization;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;



    --  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
    package Math_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);

    Rendering_Program  : GL.Objects.Programs.Program;
    Vertex_Array       :  GL.Objects.Vertex_Arrays.Vertex_Array_Object;

    procedure Render_Dot (Current_Time : Glfw.Seconds) is
        use GL.Types;
        use Math_Functions;

        Back_Colour : GL.Types.Colors.Color :=
          (0.5 * (1.0 + Sin (Single (Current_Time))),
           0.5 * (1.0 + Cos (Single (Current_Time))), 0.0, 1.0);
    begin
        GL.Buffers.Clear ((True, False, False, True));
        GL.Buffers.Set_Color_Clear_Value (Back_Colour);

        GL.Objects.Programs.Use_Program (Rendering_Program);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);

    exception
        when anError : Constraint_Error =>
            Put ("Render returned constraint error: ");
            Put_Line (Exception_Information (anError));

        when anError : GL.Errors.Invalid_Operation_Error =>
            Put_Line ("Render returned an invalid operation error: ");
            Put_Line (Exception_Information (anError));

        when anError :  others =>
            Put_Line ("An exceptiom occurred in Render.");
            Put_Line (Exception_Information (anError));
    end Render_Dot;

    procedure Setup_Graphic is
        use Program_Loader;
        use GL.Objects.Shaders;
    begin
        Rendering_Program := Program_From (
          (Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader))
        );
        GL.Toggles.Enable (gl.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (gl.Types.Less);
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        -- Point size is set in the vertex shader
        gl.Toggles.Enable (gl.Toggles.Vertex_Program_Point_Size);
        Utilities.Show_Shader_Program_Data (Rendering_Program);
    end Setup_Graphic;

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup_Graphic;
    while Running loop
        Render_Dot (Glfw.Time);
        glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        glfw.Input.Poll_Events;
        Running := Running and not
            (Main_Window.Key_State (glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;
exception
    when Program_Loader.Shader_Loading_Error =>
        -- message was already written to stdout
        null;
end Main_Loop;
