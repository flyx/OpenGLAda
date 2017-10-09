
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input;
with Glfw.Input.Keys;
with GL.Window;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with FT.OGL;
with FT.Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Render_Program        : GL.Objects.Programs.Program;
   Texture_ID            : GL.Uniforms.Uniform;
   Projection_Matrix_ID  : GL.Uniforms.Uniform;
   Colour_ID             : GL.Uniforms.Uniform;
   Projection_Matrix     : GL.Types.Singles.Matrix4;

   Background      : constant GL.Types.Colors.Color := (0.4, 0.6, 0.6, 1.0);
   Text_Colour     : constant GL.Types.Colors.Basic_Color := (0.5, 0.2, 0.6);
   Font_File_1     : String := "../fonts/NotoSerif-Regular.ttf";

   --  ------------------------------------------------------------------------

   procedure Render_The_Text (Text   : String; X, Y, Scale : GL.Types.Single;
                              Colour : GL.Types.Colors.Basic_Color);

   --  ------------------------------------------------------------------------

   procedure Render (Window  : in out Glfw.Windows.Window) is
      use GL.Types;
      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;
      Pos_X           : GL.Types.Single := 5.0;
      Pos_Y           : GL.Types.Single := 50.0;
      Scale_1         : GL.Types.Single := 0.4;
      Scale_2         : GL.Types.Single := 0.6;
   begin
      Window.Get_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Background_Colour_And_Depth (Background);
      Maths.Init_Orthographic_Transform (Single (Window_Height), 0.0, 0.0,
                                         Single (Window_Width), 0.1, -100.0,
                                         Projection_Matrix);
      Render_The_Text ("The Quick Brown Fox jumps over the zoo's Lazy Dog.",
                       Pos_X, Pos_Y, Scale_1, Text_Colour);
      Render_The_Text ("1234567890 !@#$%^&*()_+=,./?;':""{}[]\|~`",
                       Pos_X + 20.0, Pos_Y + 150.0, Scale_2, Text_Colour);
   end Render;

   --  ------------------------------------------------------------------------

   procedure Render_The_Text (Text   : String; X, Y, Scale : GL.Types.Single;
                              Colour : GL.Types.Colors.Basic_Color) is
   begin
     FT.OGL.Render_Text (Render_Program, Text, X, Y, Scale, Colour,
                         Texture_ID, Projection_Matrix_ID, Colour_ID,
                         Projection_Matrix);
   end Render_The_Text;

   --  ------------------------------------------------------------------------

   procedure Setup  (Window  : in out Glfw.Windows.Window) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types;
      use Program_Loader;

      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;
   begin
      Window.Get_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));

      GL.Toggles.Enable (GL.Toggles.Cull_Face);

      Render_Program := Program_From
          ((Src ("src/shaders/text_vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/text_fragment_shader.glsl", Fragment_Shader)));
      Use_Program (Render_Program);

      Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "projection_matrix");
      Texture_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "text_sampler");
      Colour_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "text_colour");

      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      FT.OGL.Initialize_Font_Data (Font_File_1);
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
      Running := Running and then not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and then not Main_Window.Should_Close;
   end loop;

   Render_Program.Delete_Id;
exception
   when anError :  others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
