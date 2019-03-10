
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

with Maths;
with My_Buffers;

    --  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    Rendering_Program     : GL.Objects.Programs.Program;
    Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer         : My_Buffers.tBuffer;
    Element_Buffer        : My_Buffers.tBuffer;
    Textures              : My_Buffers.tTexture_List (0 .. 1);
    Fade_Factor           : GL.Types.Single := 1.0;
    Fade_Factor_Location  : GL.Uniforms.Uniform;
    Texture_Location      : array (0 .. 1) of GL.Uniforms.Uniform;
    Position_Location     : GL.Attributes.Attribute;

    --  ------------------------------------------------------------------------

    procedure Render is
        use GL.Types;
        use GL.Objects.Textures.Targets;
        use Maths.Single_Math_Functions;

        Back_Colour   : constant Colors.Color := (1.0, 1.0, 1.0, 1.0);
        Stride_Bytes  : constant Size := 8;
    begin
        Utilities.Clear_Background_Colour_And_Depth (Back_Colour);
        Fade_Factor := 0.5 * (1.0 + Sin (Single (Glfw.Time)));

        GL.Objects.Programs.Use_Program (Rendering_Program);

        GL.Uniforms.Set_Single (Fade_Factor_Location, Fade_Factor);

        GL.Objects.Textures.Set_Active_Unit (0);
        Texture_2D.Bind (Textures (0));
        GL.Uniforms.Set_Int (Texture_Location (0), 0);

        GL.Objects.Textures.Set_Active_Unit (1);
        Texture_2D.Bind (Textures (1));
        GL.Uniforms.Set_Int (Texture_Location (1), 1);

        GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Position_Location, 2, GL.Types.Single_Type, False, Stride_Bytes, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (Position_Location);

        GL.Objects.Buffers.Element_Array_Buffer.Bind (Element_Buffer);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangle_Strip, 0, 4);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Setup_Graphic is
        use Ada.Strings.Unbounded;
        use GL.Objects.Shaders;
        use Program_Loader;
        Images  : constant My_Buffers.tImage_Sources (1 .. 2) :=
                    (To_Unbounded_String ("src/hello1.tga"),
                     To_Unbounded_String ("src/hello2.tga"));
    begin
        Rendering_Program := Program_From
          ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        My_Buffers.Setup_Buffers (Vertex_Buffer, Element_Buffer);
        My_Buffers.Setup_Textures (Textures, Images);

        Fade_Factor_Location :=
          GL.Objects.Programs.Uniform_Location (Rendering_Program, "fade_factor");
        Texture_Location (0) :=
          GL.Objects.Programs.Uniform_Location (Rendering_Program, "textures[0]");
        Texture_Location (1) :=
          GL.Objects.Programs.Uniform_Location (Rendering_Program, "textures[1]");
        Position_Location :=
          GL.Objects.Programs.Attrib_Location (Rendering_Program, "position");

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
    when Program_Loader.Shader_Loading_Error =>
        --  message has already been written to stdout
        raise;
    when others =>
            Put_Line ("An exceptiom occurred in Main_Loop.");
            raise;
end Main_Loop;
