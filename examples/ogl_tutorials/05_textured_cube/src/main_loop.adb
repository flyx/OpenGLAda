
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input.Keys;
with Glfw.Windows;
with Glfw.Windows.Context;

with Cube_Data;
with Maths;
with Program_Loader;
with Load_DDS;
with Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Dark_Blue                : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 1.0);

    Vertices_Array_Object    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer            : GL.Objects.Buffers.Buffer;
    UVs_Buffer               : GL.Objects.Buffers.Buffer;
    Render_Program           : GL.Objects.Programs.Program;
    MVP_Matrix_ID            : GL.Uniforms.Uniform;
    Texture_ID               : GL.Uniforms.Uniform;
    Cube_Texture             : GL.Objects.Textures.Texture;
    MVP_Matrix               : GL.Types.Singles.Matrix4;

    --  ------------------------------------------------------------------------

    procedure Set_MVP_Matrix (Window : in out Glfw.Windows.Window;
                              Render_Program : GL.Objects.Programs.Program);

    --  ------------------------------------------------------------------------

    procedure Render is
        use GL.Objects.Buffers;

    begin
        Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);

        GL.Objects.Programs.Use_Program (Render_Program);
        GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);

        GL.Objects.Textures.Set_Active_Unit (0);
        GL.Objects.Textures.Targets.Texture_2D.Bind (Cube_Texture);
        --  Set myTextureSampler sampler to use Texture Unit 0
        GL.Uniforms.Set_Int (Texture_ID, 0);

        --  First attribute buffer : vertices
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (0, 3, Single_Type, False, 0, 0);

        --  Second attribute buffer : UVs
        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        Array_Buffer.Bind (UVs_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (1, 2, Single_Type, False, 0, 0);

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
          (Render_Program, "MVP_5");

        Init_Lookat_Transform (Camera_Position, Look_At, Up, View_Matrix);
        Init_Perspective_Transform (45.0, Single (Window_Width),
                                          Single (Window_Height),
                                    0.1, 100.0, Projection_Matrix);
       MVP_Matrix :=  Projection_Matrix * View_Matrix * Model_Matrix;
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
        Utilities.Clear_Background_Colour (Dark_Blue);

        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);

        Vertices_Array_Object.Initialize_Id;
        Vertices_Array_Object.Bind;

        Render_Program := Program_Loader.Program_From
          ((Program_Loader.Src ("src/shaders/Transform_Vertex_Shader.glsl",
           Vertex_Shader),
           Program_Loader.Src ("src/shaders/Texture_Fragment_Shader.glsl",
             Fragment_Shader)));
        Utilities.Show_Shader_Program_Data (Render_Program);

        Set_MVP_Matrix (Window, Render_Program);

        Load_DDS ("src/textures/uvtemplate.DDS", Cube_Texture);
        Texture_ID := GL.Objects.Programs.Uniform_Location
                      (Render_Program, "myTextureSampler");

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Cube_Data.Vertex_Data, Static_Draw);

        UVs_Buffer.Initialize_Id;
        Array_Buffer.Bind (UVs_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Cube_Data.UV_Data, Static_Draw);

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
        Render;
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
