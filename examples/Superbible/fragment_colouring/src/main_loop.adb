
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Culling;
with GL.Errors;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is

    package Math_Functions is new Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);
    subtype tVec4f is GL.Types.Singles.Vector4;

    Rendering_Program : GL.Objects.Programs.Program;
    Vertex_Array      : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

    --  ------------------------------------------------------------------------

    procedure Render_Triangle (Current_Time : Glfw.Seconds) is
        use GL.Types;
        use Math_Functions;
        Now           : Single := Single (Current_Time);
        Back_Colour   : GL.Types.Colors.Color := (0.5 * (1.0 + Sin (Now)),
                                                  0.5 * (1.0 + Cos (Now)), 0.0, 1.0);
        colour        : tVec4f  := (0.8 * (1.0 + Sin (Now)),
                                    0.4 * (1.0 + Cos (Now)), 0.4, 1.0);
        Offset        : tVec4f := (0.5 * Sin (Now),
                                   0.5 * Cos (Now), 0.0, 0.0);
    begin
        GL.Buffers.Clear ((True, False, False, True));
        GL.Buffers.Set_Color_Clear_Value (Back_Colour);

        GL.Objects.Programs.Use_Program (Rendering_Program);

        GL.Attributes.Set_Single (0, offset);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 3);

    exception
        when anError :  others =>
            Put_Line ("An exceptiom occurred in Render_Triangle.");
            raise;
    end Render_Triangle;

    --  ------------------------------------------------------------------------

    procedure Startup is
    begin
        Rendering_Program := Program_Loader.Program_From
          ((Program_Loader.Src ("src/shaders/vertex_shader1.glsl", GL.Objects.Shaders.Vertex_Shader),
           Program_Loader.Src ("src/shaders/fragment_shader.glsl", GL.Objects.Shaders.Fragment_Shader)));
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Toggles.Enable (GL.Toggles.Cull_Face);
        GL.Culling.Set_Front_Face (GL.Types.Clockwise);
        GL.Culling.Set_Cull_Face (GL.Culling.Back);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        Utilities.Show_Shader_Program_Data (Rendering_Program);

    exception
        when anError :  others =>
            Put_Line ("An exceptiom occurred in Startup.");
            raise;
    end Startup;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running    : Boolean := True;
begin
    Startup;
    while Running loop
        Render_Triangle (Glfw.Time);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and not (Main_Window.Key_State (Glfw.Input.Keys.Escape) =
                                      Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;

exception
        when anError :  others =>
            Put_Line ("An exceptiom occurred in Main_Loop.");
            raise;
end Main_Loop;
