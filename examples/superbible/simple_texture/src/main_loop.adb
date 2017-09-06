
with Interfaces;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Toggles;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Textures.With_2D_Loader;
with GL.Pixels;
with GL.Rasterization;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   type Data is array (GL.Types.Int range <>) of GL.Types.Single;

    Rendering_Program  : GL.Objects.Programs.Program;
    Vertex_Array       : GL.Objects.Vertex_Arrays;
    theTexture         : GL.Objects.Textures.Texture;

--  ------------------------------------------------------------------------

   procedure Load_Subimage_2D   (Object : in out GL_Object;
                  Level, X_Offset, Y_Offset : GL.Types.Int;
                  Width, Height : GL.Types.Size;
                  Format     : GL.Pixels.Data_Format; Data_Type : GL.Pixels.Data_Type;
                  Image_Data : access GL.Objects.Textures.Image_Source) is
   begin
      GL_Load_Subimage_2D (Object, Level, X_Offset, Y_Offset,
                           Width, Height, Format, Data_Type, Image_Data);
      Raise_Exception_On_OpenGL_Error;
   end Load_Subimage_2D;

--  ------------------------------------------------------------------------

    procedure Generate_Texture (Texture_Data : in out Data; Width, Height : GL.Types.Int) is
      use GL.Types;
      use Interfaces;
      Col_32     : Unsigned_32;
      Row_32     : Unsigned_32;
      Width_32   : constant Unsigned_32 := Unsigned_32 (Width);
      Height_32  : constant Unsigned_32 := Unsigned_32 (Height);
    begin
        for Col in 1 .. Height loop
            Col_32 := Unsigned_32 (Col);
            for Row in 1 .. Width loop
                Row_32 := Unsigned_32 (Row);
                Texture_Data ((Col * Width + Row) * 4) :=
                  Single ((Row_32 and Col_32) and 16#FF#) / 255.0;
                Texture_Data ((Col * Width + Row) * 4 + 1) :=
                  Single ((Row_32 or Col_32) and 16#FF#) / 255.0;
                Texture_Data ((Col * Width + Row) * 4 + 2) :=
                  Single ((Row_32 xor Col_32) and 16#FF#) / 255.0;
                Texture_Data ((Col * Width + Row) * 4 + 3) := 1.0;
            end loop;
        end loop;
    end Generate_Texture;

--  ------------------------------------------------------------------------

    procedure Render is
        use GL.Types;
        use Maths.Single_Math_Functions;

        Back_Colour : GL.Types.Colors.Color :=
          (0.5 * (1.0 + Sin (Single (';Current_Time))),
           0.5 * (1.0 + Cos (Single (Current_Time))), 0.0, 1.0);
    begin
        Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

        GL.Objects.Programs.Use_Program (Rendering_Program);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);

    exception
        when anError :  others =>
            Put_Line ("An exceptiom occurred in Render.");
            raise;
    end Render;

--  ----------------------------------------------------------------------------

    procedure Setup is
        use GL.Objects.Shaders;
        use GL.Objects.Textures.Targets;
        use GL.Types;
        use Program_Loader;
        Width        : constant Int := 256;
        Height       : constant Int := 256;
        MIP_Levels   : constant Int := 8;
        Texture_Data : Data (1 .. Width * Height * 4);
    begin
      theTexture.Initialize_Id;
      Texture_2D.Bind (theTexture);
      Texture_2D.Storage (MIP_Levels, GL.Pixels.RGBA32F, Width, Height);
      Generate_Texture (Texture_Data, Width, Height);

        Rendering_Program := Program_From (
          (Src ("src/shaders/st_vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/st_fragment_shader.glsl", Fragment_Shader))
        );
        GL.Toggles.Enable (gl.Toggles.Depth_Test);
        GL.Buffers.Set_Depth_Function (gl.Types.Less);
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
        -- Point size is set in the vertex shader
        gl.Toggles.Enable (gl.Toggles.Vertex_Program_Point_Size);
        Utilities.Show_Shader_Program_Data (Rendering_Program);
    exception
        when anError :  others =>
            Put_Line ("An exceptiom occurred in Setup.");
            raise;
    end Setup;

--  ----------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup;
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
        -- message was already written to stdout
        null;
end Main_Loop;
