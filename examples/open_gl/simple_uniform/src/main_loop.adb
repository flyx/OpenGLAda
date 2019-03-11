
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Uniforms;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;
with Vertex_Data;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Rendering_Program     : GL.Objects.Programs.Program;
    Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer         : GL.Objects.Buffers.Buffer;
    Position_Location     : GL.Attributes.Attribute;
    Colour_Location       : GL.Uniforms.Uniform;

    --  ------------------------------------------------------------------------

    procedure Render is
        use GL.Types;
        use Maths.Single_Math_Functions;
        Now  : constant Glfw.Seconds := Glfw.Time;
    begin
        Rendering_Program.Use_Program;
        GL.Uniforms.Set_Single (Colour_Location,
                             (1.0 + Sin (4.0 * Single (Now))) / 2.0, 0.0, 0.0);
        GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Triangles, 0, 3);
    exception
        when others =>
            Put_Line ("An exception occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Setup_Graphic is
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
        use GL.Types;
        use Program_Loader;
    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data.Vertices, Static_Draw);

        Rendering_Program := Program_From
          ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));

        Position_Location := GL.Objects.Programs.Attrib_Location
          (Rendering_Program, "position");
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Position_Location, 2, GL.Types.Single_Type, False, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (Position_Location);
        Colour_Location := GL.Objects.Programs.Uniform_Location
          (Rendering_Program, "triangle_colour");

        Rendering_Program.Use_Program;  -- Needed to set uniform values
        GL.Uniforms.Set_Single (Colour_Location, 1.0, 0.0, 0.0);

        Utilities.Show_Shader_Program_Data (Rendering_Program);
    exception
        when others =>
            Put_Line ("An exception occurred in Setup_Graphic.");
            raise;
    end Setup_Graphic;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup_Graphic;
    while Running loop
        Render;
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;
exception
    when others =>
        Put_Line ("An exception occurred in Main_Loop.");
        raise;
end Main_Loop;
