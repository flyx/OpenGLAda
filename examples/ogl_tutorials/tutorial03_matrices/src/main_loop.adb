
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;
with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is

    procedure Load_Vertex_Buffer is new GL.Objects.Buffers.Load_To_Buffer
      (Vertex_Data.pVertex_Pointers);

    Dark_Blue           : GL.Types.Colors.Color := (0.0, 0.0, 0.4, 1.0);
    Element_Buffer_Data : Array (0 .. 2) of GL.Types.UShort := (0, 1, 2);
    Vertex_Array        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer       : GL.Objects.Buffers.Buffer;
    Render_Program      : GL.Objects.Programs.Program;
    MVP_Location        : GL.Uniforms.Uniform;
    MVP_Matrix          : Maths.tMatrix_4f := GL.Types.Singles.Identity4;

    --  ------------------------------------------------------------------------

    procedure Render (Current_Time : Glfw.Seconds) is
        use GL.Types;
        use GL.Objects.Buffers;
    begin
        Utilities.Clear_Background_Colour (Dark_Blue);
        GL.Objects.Programs.Use_Program (Render_Program);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        Array_Buffer.Bind (Vertex_Buffer);

        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
        GL.Uniforms.Set_Single (MVP_Location, MVP_Matrix);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 3);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when others =>
            Put_Line ("An exception occurred in Render.");raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Set_MVP_Matrix (Render_Program : GL.Objects.Programs.Program) is
        use GL.Types;
        use Maths;
        View_Width        : constant Single := 1024.0;
        View_Height       : constant Single := 768.0;
        Camera_Position   : tVector_3f := (4.0, 3.0, 3.0);
        Look_At           : tVector_3f := (0.0, 0.0, 0.0);
        Up                : tVector_3f := (0.0, 1.0, 0.0);
        Model_Matrix      : Maths.tMatrix_4f := Singles.Identity4;
        Projection_Matrix : Maths.tMatrix_4f := Singles.Identity4;
        View_Matrix       : Maths.tMatrix_4f := Singles.Identity4;
    begin
        MVP_Location := GL.Objects.Programs.Uniform_Location
          (Render_Program, "MVP");

        Init_Perspective_Transform (45.0, View_Width, View_Height, 0.1, 100.0, Projection_Matrix);
        Init_Lookat_Transform (Camera_Position, Look_At, Up, View_Matrix);
        MVP_Matrix := Projection_Matrix * View_Matrix * Model_Matrix;

    exception
        when others =>
            Put_Line ("An exception occurred in Set_MVP_Matrix.");
            raise;
    end Set_MVP_Matrix;

    --  ------------------------------------------------------------------------

    procedure Setup is
        use GL.Types;
        use GL.Objects.Buffers;

        use GL.Objects.Shaders;
        use Program_Loader;
    begin
        Utilities.Clear_Background_Colour (Dark_Blue);

        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        Render_Program := Program_From
          ((Src ("src/shaders/simple_transform_vertex.glsl", Vertex_Shader),
           Src ("src/shaders/single_colour_fragment.glsl", Fragment_Shader)));
            Set_MVP_Matrix (Render_Program);
            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);
            Load_Vertex_Buffer (Array_Buffer, Vertex_Data.Vertex_Buffer_Data,
                                Static_Draw);
    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup.");
    end Setup;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup;
    while Running loop
        Render (Glfw.Time);
        Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        Glfw.Input.Poll_Events;
        Running := Running and not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;
exception
    when others =>
        Put_Line ("An exceptiom occurred in Main_Loop.");
        raise;
end Main_Loop;
