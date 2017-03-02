
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Errors;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;
with Vertex_Data;

with My_Buffers;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
    package Math_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);

    --  ------------------------------------------------------------------------

    Rendering_Program       : GL.Objects.Programs.Program;
    Vertex_Array            : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Vertex_Buffer           : My_Buffers.tBuffer;
    Element_Buffer          : My_Buffers.tBuffer;
    Texture                 : GL.Objects.Textures.Texture;
    Colour_Location         : GL.Attributes.Attribute;
    Image_Location          : GL.Uniforms.Uniform;
    Position_Location       : GL.Attributes.Attribute;
    Texture_Coords_Location : GL.Attributes.Attribute;

    --  ------------------------------------------------------------------------

    procedure Render is
        use GL.Types;
        use GL.Objects.Textures.Targets;

        Back_Colour   : Colors.Color := (0.0, 0.7, 0.7, 0.0);
        Border_Colour : GL.Types.Colors.Color := (1.0, 0.0, 0.0, 1.0);
        Single_Size   : GL.Types.Size := GL.Types.Single'Size;
        Stride        : GL.Types.Size := 7;
        Skip_Vertex   : GL.Types.Size := 2;
        Skip_To_Texture_Coords : GL.Types.Size := 5;
    begin
        Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

        Rendering_Program.Use_Program;
        GL.Objects.Textures.Set_Active_Unit (0);
        Texture_2D.Bind (Texture);
        --  Texture_2D.Set_Border_Color (Border_Colour);
        GL.Uniforms.Set_Int (Image_Location, 0);

        GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Position_Location, 2, GL.Types.Single_Type, Stride, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (Position_Location);

        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Colour_Location, 3, GL.Types.Single_Type, Stride, Skip_Vertex);
        GL.Attributes.Enable_Vertex_Attrib_Array (Colour_Location);

        GL.Attributes.Set_Vertex_Attrib_Pointer
          (Texture_Coords_Location, 2, GL.Types.Single_Type, Stride, Skip_To_Texture_Coords);
        GL.Attributes.Enable_Vertex_Attrib_Array (Texture_Coords_Location);

        GL.Objects.Buffers.Element_Array_Buffer.Bind (Element_Buffer);
        GL.Objects.Buffers.Draw_Elements (Triangles, 6, GL.Types.UInt_Type);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Setup_Graphic is
        use Program_Loader;
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
        use GL.Objects.Textures;
    begin
        Rendering_Program := Program_From
          ((Src ("src/shaders/vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/fragment_shader.glsl", Fragment_Shader)));
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        My_Buffers.Setup_Buffers (Vertex_Buffer, Element_Buffer);
        My_Buffers.Setup_Textures (Texture, "src/cat.png");

        Position_Location :=
          GL.Objects.Programs.Attrib_Location (Rendering_Program, "position");
        Colour_Location :=
          GL.Objects.Programs.Attrib_Location (Rendering_Program, "colour");
        Texture_Coords_Location :=
          GL.Objects.Programs.Attrib_Location (Rendering_Program, "image_coords");
        Image_Location :=
          GL.Objects.Programs.Uniform_Location (Rendering_Program, "image");

        Utilities.Show_Shader_Program_Data (Rendering_Program);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup_Graphic.");
            raise;
    end Setup_Graphic;

    --  --------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup_Graphic;
    while Running loop
        Render;
        glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        glfw.Input.Poll_Events;
        Running := Running and not
          (Main_Window.Key_State (glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;
exception
    when Program_Loader.Shader_Loading_Error =>
        --  message has been written to stdout
        raise;
    when anError :  others =>
        Put_Line ("An exceptiom occurred in Main_Loop.");
        raise;
end Main_Loop;
