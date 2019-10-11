
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Types;
with GL.Types.Colors;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;
with Vertex_Data;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Shader_Program        : GL.Objects.Programs.Program;
    Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer         : GL.Objects.Buffers.Buffer;
    Elements_Buffer       : GL.Objects.Buffers.Buffer;
    Position_Location     : GL.Attributes.Attribute;
    Colour_Location       : GL.Attributes.Attribute;

    --  ------------------------------------------------------------------------

    procedure Render is
        use GL.Types;
        Back_Colour   : constant Colors.Color := (0.0, 0.6, 0.6, 1.0);
        Stride        : constant Int := 5 * 4;
        Skip          : constant Int := 3 * 4;
    begin
        Utilities.Clear_Background_Colour (Back_Colour);

        Shader_Program.Use_Program;
        GL.Attributes.Enable_Vertex_Attrib_Array (Position_Location);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Position_Location, 2, Single_Type, False, Stride, 0);

        GL.Attributes.Enable_Vertex_Attrib_Array (Colour_Location);

        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Colour_Location, 3, Single_Type, False, Stride, Skip);

        GL.Objects.Buffers.Draw_Elements (GL.Types.Triangles, 6, UInt_Type);
    exception
        when others =>
            Put_Line ("An exception occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Setup_Graphic is
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
        use Program_Loader;
    begin
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Utilities.Load_Vector5_Buffer (Array_Buffer, Vertex_Data.Vertices, Static_Draw);

        Elements_Buffer.Initialize_Id;
        Element_Array_Buffer.Bind (Elements_Buffer);
        Vertex_Data.Load_Element_Buffer (Element_Array_Buffer, Vertex_Data.Elements, Static_Draw);

        Shader_Program := Program_From
          ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));
        Put ("Shader_Program.Info_Log: ");
        Put_Line (Shader_Program.Info_Log);
        Put_Line ("Shader_Program.Active_Attributes:" &
                    GL.Types.Size'Image (Shader_Program.Active_Attributes));
        Put_Line ("Shader_Program.Active_Uniforms:" &
                    GL.Types.Size'Image (Shader_Program.Active_Uniforms));

        Position_Location := GL.Objects.Programs.Attrib_Location
          (Shader_Program, "position");
        Colour_Location := GL.Objects.Programs.Attrib_Location
          (Shader_Program, "colour");

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
