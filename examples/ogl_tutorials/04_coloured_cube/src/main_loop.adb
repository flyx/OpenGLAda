
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Cube_Data;
with Maths;
with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Dark_Blue                : constant Colors.Color := (0.0, 0.0, 0.4, 1.0);

    Vertices_Array_Object    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer            : GL.Objects.Buffers.Buffer;
    Colour_Buffer            : GL.Objects.Buffers.Buffer;
    Render_Program           : GL.Objects.Programs.Program;
    MVP_Matrix_ID            : GL.Uniforms.Uniform;
    MVP_Matrix               : GL.Types.Singles.Matrix4;

    --  ------------------------------------------------------------------------

    procedure Set_MVP_Matrix (Window : in out Glfw.Windows.Window;
                              Render_Program : GL.Objects.Programs.Program);

    --  ------------------------------------------------------------------------

    procedure Render (Window : in out Glfw.Windows.Window) is
        use GL.Objects.Buffers;
        Window_Width  : Glfw.Size;
        Window_Height : Glfw.Size;
    begin
        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                                GL.Types.Int (Window_Height));
        Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);

        GL.Objects.Programs.Use_Program (Render_Program);
        Set_MVP_Matrix (Window, Render_Program);
        GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);

        --  First attribute buffer : vertices
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (0, 3, Single_Type, False, 0, 0);

        --  Second attribute buffer : Colours
        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        Array_Buffer.Bind (Colour_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (1, 3, Single_Type, False, 0, 0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangles,
                                              First => 0,
                                              Count => 12 * 3);

        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        GL.Attributes.Disable_Vertex_Attrib_Array (1);
    exception
        when others =>
            Put_Line ("An exception occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Set_MVP_Matrix (Window : in out Glfw.Windows.Window;
                              Render_Program : GL.Objects.Programs.Program) is
        use GL.Types.Singles;
        use Maths;
        --  Camera position, Look_At and Up are world coordinates.
        Camera_Position   : constant Vector3 := (4.0, 3.0, -3.0);
        Look_At           : constant Vector3 := (0.0, 0.0, 0.0);
        Up                : constant Vector3 := (0.0, 1.0, 0.0);
        --  The Model_Matrix operates in world coordinates.
        Model_Matrix      : constant Matrix4 := Singles.Identity4;
        --  The Projection_Matrix projects the camera view in camera coordinates
        --  onto the camera view's Near plane
        Projection_Matrix : Matrix4;
        --  The View_Matrix transforms the world_cordinates of the world view
        --  into view (camera) coordinates.
        View_Matrix       : Matrix4;
        Window_Width      : Glfw.Size;
        Window_Height     : Glfw.Size;
    begin
        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "MVP_Matrix");

        Init_Lookat_Transform (Camera_Position, Look_At, Up, View_Matrix);
        Init_Perspective_Transform (60.0, Single (Window_Width),
                                          Single (Window_Height),
                                    0.1, 100.0, Projection_Matrix);
        --  The View_Matrix transforms world_cordinates to view (camera) coordinates.
        --  The Projection_Matrix transforms view (camera) coordinates.
        MVP_Matrix := Projection_Matrix * View_Matrix * Model_Matrix;
    exception
        when others =>
            Put_Line ("An exception occurred in Set_MVP_Matrix.");
            raise;
    end Set_MVP_Matrix;

    --  ------------------------------------------------------------------------

    procedure Setup (Window : in out Glfw.Windows.Window) is
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
    begin
        Window.Set_Input_Toggle (Glfw.Input.Sticky_Keys, True);

        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        --  Accept fragment if it closer to the camera than the former one.
        GL.Buffers.Set_Depth_Function (GL.Types.Less);
        Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);

        Vertices_Array_Object.Initialize_Id;
        Vertices_Array_Object.Bind;

        Render_Program := Program_Loader.Program_From
          ((Program_Loader.Src ("src/shaders/MVP_Vertex_Shader.glsl",
           Vertex_Shader),
           Program_Loader.Src ("src/shaders/Colour_Fragment_Shader.glsl",
             Fragment_Shader)));
        Utilities.Show_Shader_Program_Data (Render_Program);

        Set_MVP_Matrix (Window, Render_Program);

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Cube_Data.Vertex_Data, Static_Draw);

        Colour_Buffer.Initialize_Id;
        Array_Buffer.Bind (Colour_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Cube_Data.Colour_Data, Static_Draw);

    exception
        when others =>
            Put_Line ("An exception occurred in Setup.");
            raise;
    end Setup;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup (Main_Window);
    while Running loop
        Render (Main_Window);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and then
          not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and then not Main_Window.Should_Close;
    end loop;

exception
    when others =>
        Put_Line ("An exception occurred in Main_Loop.");
        raise;
end Main_Loop;
