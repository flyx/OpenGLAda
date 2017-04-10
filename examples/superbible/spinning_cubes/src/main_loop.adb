
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Culling;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;
with Vertex_Data;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Green                  : GL.Types.Colors.Color := (0.0, 0.6, 0.0, 1.0);
    Grey                   : GL.Types.Colors.Color := (0.8, 0.8, 0.8, 1.0);
    Window_Width           : Glfw.Size := 520;
    Window_Height          : Glfw.Size := 520;
    Viewport_Width         : GL.Types.Size := 500;
    Viewport_Height        : GL.Types.Size := 500;
    Position_Buffer        : GL.Objects.Buffers.Buffer;
    Rendering_Program      : GL.Objects.Programs.Program;
    Vertex_Array           : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Projection_Location    : GL.Uniforms.Uniform;
    Model_View_Location    : GL.Uniforms.Uniform;

    Projection_Matrix      : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
    --  Projection_Matrix_Stack : GL.Fixed.Matrix.Matrix_Stack := GL.Fixed.Matrix.Projection;
    --  Projection_Matrix_Array : Maths.pSingles_Algebra.Matrix4_Array (1 .. 1);

    Set_Up_Error           : Exception;

    --  ------------------------------------------------------------------------

    procedure Render_Cubes (Window : in out Glfw.Windows.Window) is
        use GL.Types;
        use GL.Types.Singles;
        use Maths.Single_Math_Functions;

        Model_View_Matrix : GL.Types.Singles.Matrix4;
        Current_Time      : Single;
        Time_Factor       : Single;
    begin
        Window.Set_Size (Window_Width, Window_Height);
        GL.Window.Set_Viewport (10, 10, GL.Types.Int (Viewport_Width),
                                GL.Types.Int (Viewport_Height));
        Utilities.Clear_Background_Colour_And_Depth (Grey);
        for count in 0 .. 10 loop
            Current_Time := Single (Glfw.Time);
            Time_Factor := Single (count) + 0.3 * Current_Time;
            Model_View_Matrix :=
              Maths.Rotation_Matrix (Maths.Degree (45.0 * Current_Time),
                                     (0.0, 1.0, 0.0)) *
                Maths.Rotation_Matrix (Maths.Degree (21.0 * Current_Time),
                                       (1.0, 0.0, 0.0));
            Model_View_Matrix := Maths.Translation_Matrix ((0.0, 0.0, -6.0)) * Model_View_Matrix;
            Model_View_Matrix :=
              Maths.Translation_Matrix ((2.0 * Sin (2.3 * Time_Factor),
                                        2.0 * Cos (1.7 * Time_Factor),
                                        2.0 * Sin (1.1 * Time_Factor)
                                        * Cos (1.5 * Time_Factor)))
              * Model_View_Matrix;

            GL.Objects.Programs.Use_Program (Rendering_Program);
            GL.Uniforms.Set_Single (Projection_Location, Transpose (Projection_Matrix));
            GL.Uniforms.Set_Single (Model_View_Location, Transpose (Model_View_Matrix));

            GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 36);
        end loop;

exception
        when  others =>
            Put_Line ("An exception occurred in Render_Cubes.");
            raise;
    end Render_Cubes;

    --  ------------------------------------------------------------------------

    procedure Set_Up (Window : in out Glfw.Windows.Window) is
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
        use GL;
        use GL.Types;
        use GL.Types.Singles;
        use Program_Loader;

    begin
        Window.Set_Size (Window_Width, Window_Height);
        GL.Window.Set_Viewport (10, 10, Viewport_Width, Viewport_Height);
        Utilities.Clear_Background_Colour_And_Depth (Grey);

        Rendering_Program :=
          Program_From ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
                         Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));
        Utilities.Show_Shader_Program_Data (Rendering_Program);

        --  Get locations of shader programs matrix uniforms
        Model_View_Location :=
          GL.Objects.Programs.Uniform_Location (Rendering_Program, "mv_matrix");
        Projection_Location :=
          GL.Objects.Programs.Uniform_Location (Rendering_Program, "projection_matrix");

        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        Position_Buffer.Initialize_Id;
        ARRAY_BUFFER.Bind (Position_Buffer);
        Utilities.Load_Vertex_Buffer (ARRAY_BUFFER, Vertex_Data.Vertices, Static_Draw);

        GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                                 Kind   => GL.Types.Single_Type,
                                                 Stride => 0, Offset => 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        Maths.Init_Perspective_Transform (Maths.Degree (50.0),
                              Single (Viewport_Width), Single (Viewport_Height),
                              0.1, 1000.0,  Projection_Matrix);

        GL.Toggles.Enable (GL.Toggles.Cull_Face);
        GL.Culling.Set_Front_Face (GL.Types.Counter_Clockwise);
        --  Depth_Function specifies the function used to compare each
        --  incoming pixel depth value with the depth value present in
        --  the depth buffer.
        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.LEqual);

    exception
        when others =>
            Put_Line ("An exception occurred in Set_Up.");
            raise Set_Up_Error;
    end Set_Up;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running      : Boolean := True;
begin
    Set_Up (Main_Window);
    while Running loop
        Render_Cubes (Main_Window);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and not (Main_Window.Key_State (Glfw.Input.Keys.Escape) =
                                      Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;

    Vertex_Array.Delete_Id;
    Rendering_Program.Delete_Id;
    Position_Buffer.Delete_Id;

exception
    when others =>
        Put_Line ("An exception occurred in Main_Loop.");
        raise;
end Main_Loop;
