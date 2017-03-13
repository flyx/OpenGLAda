
with Interfaces.C;
with Interfaces.C.Pointers;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Errors;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Toggles;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with Controls;
with Cube_Data;
with Maths;
with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    package pVertex_Pointers is new Interfaces.C.Pointers
      (GL.Types.UInt, GL.Types.Single, Cube_Data.tElement_Array, 0.0);

    procedure Load_Vertex_Buffer is new GL.Objects.Buffers.Load_To_Buffer
      (pVertex_Pointers);

    Dark_Blue                : GL.Types.Colors.Color := (0.0, 0.0, 0.4, 1.0);
    White                    : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);

    Vertices_Array_Object    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer            : GL.Objects.Buffers.Buffer;
    Colour_Buffer               : GL.Objects.Buffers.Buffer;
    Render_Program           : GL.Objects.Programs.Program;
    MVP_Matrix_ID            : GL.Uniforms.Uniform;
    MVP_Matrix               : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
    UV_Texture               : GL.Objects.Textures.Texture;

    --  ------------------------------------------------------------------------

    procedure Render (Window : in out Glfw.Windows.Window) is
        use GL.Types;
        use GL.Objects.Buffers;
        use Maths;

        Window_Width                  : Glfw.Size;
        Window_Height                 : Glfw.Size;
    begin
        Utilities.Clear_Background_Colour (Dark_Blue);
        Glfw.Windows.Get_Size (Window'Access, Window_Width, Window_Height);
        GL.Window.Set_Viewport (0, 0, Int (Window_Width), Int (Window_Height));

        GL.Objects.Programs.Use_Program (Render_Program);
        GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);

        --  First attribute buffer : vertices
        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

        --  Second attribute buffer : UVs
        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        Array_Buffer.Bind (Colour_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 0, 0);

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

    procedure Setup (Window : in out Glfw.Windows.Window) is
        use Interfaces.C;
        use GL.Types;
        use GL.Types.Singles;
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
        use GL.Objects.Textures.Targets;
        Window_Width                  : Glfw.Size;
        Window_Height                 : Glfw.Size;
        Camera_Position               : Singles.Vector3 := (4.0, 3.0, 3.0);
        Target                        : Singles.Vector3 := (0.0, 0.0, 0.0);
        Up                            : Singles.Vector3 := (0.0, 1.0, 0.0);
        Model_Matrix                  : Singles.Matrix4 := Singles.Identity4;
        View_Matrix                   : Singles.Matrix4;
        Projection_Matrix             : Singles.Matrix4;
    begin
        Window.Set_Input_Toggle (Glfw.Input.Sticky_Keys, True);
        Window.Get_Size (Window_Width, Window_Height);
--          Window.Set_Cursor_Pos (Glfw.Input.Mouse.Coordinate (Window_Width) / 2.0,
--                                 Glfw.Input.Mouse.Coordinate (Window_Height) / 2.0);
        Utilities.Clear_Background_Colour (Dark_Blue);

        GL.Toggles.Enable (GL.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (GL.Types.Less);

        Vertices_Array_Object.Initialize_Id;
        Vertices_Array_Object.Bind;

        Render_Program := Program_Loader.Program_From
          ((Program_Loader.Src ("src/shaders/Transform_Vertex_Shader.glsl",
           Vertex_Shader),
           Program_Loader.Src ("src/shaders/Colour_Fragment_Shader.glsl",
             Fragment_Shader)));
        Utilities.Show_Shader_Program_Data (Render_Program);

        MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location (Render_Program, "MVP");

        Maths.Init_Lookat_Transform (Camera_Position, Target, Up, View_Matrix);
        Maths.Init_Perspective_Transform (45.0, Single (Window_Width),
                                          Single (Window_Height), 0.1, 100.0,
                                          Projection_Matrix);
        MVP_Matrix := Projection_Matrix * View_Matrix * Model_Matrix;

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Load_Vertex_Buffer (Array_Buffer, Cube_Data.Vertex_Data, Static_Draw);

        Colour_Buffer.Initialize_Id;
        Array_Buffer.Bind (Colour_Buffer);
        Load_Vertex_Buffer (Array_Buffer, Cube_Data.Colour_Data, Static_Draw);
        Put_Line ("Setup; Vertex and UVs Buffers loaded.");

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
        Delay (1.0);
        Render (Main_Window);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and then
          not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and then not Main_Window.Should_Close;
    end loop;

    Vertex_Buffer.Delete_Id;
    Colour_Buffer.Delete_Id;
    Render_Program.Delete_Id;

exception
    when others =>
        Put_Line ("An exception occurred in Main_Loop.");
        raise;
end Main_Loop;
