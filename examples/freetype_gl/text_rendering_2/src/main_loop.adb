
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

with GL.Blending;
with GL.Objects.Programs;
with GL.Objects.Shaders;
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

with Text_Management;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Render_Text_Program   : GL.Objects.Programs.Program;
   Dimensions_ID         : GL.Uniforms.Uniform;
   Texture_ID            : GL.Uniforms.Uniform;
   MVP_Matrix_ID         : GL.Uniforms.Uniform;
   Colour_ID             : GL.Uniforms.Uniform;
   MVP_Matrix            : GL.Types.Singles.Matrix4;

   Background      : constant GL.Types.Colors.Color := (0.4, 0.6, 0.6, 1.0);
   Text_Colour     : constant GL.Types.Colors.Color := (0.5, 0.2, 0.6, 1.0);
   Font_File_1     : constant String := "../fonts/NotoSerif-Regular.ttf";

   --  ------------------------------------------------------------------------

   procedure Render (Window  : in out Glfw.Windows.Window) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;
      Pos_X           : constant GL.Types.Single := 10.0;
      Pos_Y           : constant GL.Types.Single := 150.0;
      Scale_1         : constant GL.Types.Single := 0.4;
      Scale_2         : constant GL.Types.Single := 0.1;
      Text_List       : Text_Management.Text_Array (1 .. 2);
   begin
      Window.Get_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Maths.Init_Orthographic_Transform (Single (Window_Height), 0.0, 0.0,
                                         Single (Window_Width), 0.1, -100.0,
                                         MVP_Matrix);
      Text_Management.Render_Text (Render_Text_Program, "Hello", 100.0, Pos_Y + 250.0,
                                   Scale_1, Text_Colour, Texture_ID, MVP_Matrix_ID,
                                   Dimensions_ID, Colour_ID, MVP_Matrix);
      Text_List (1) :=
         (To_Unbounded_String ("1234567890 !@#$%^&*()_+=,./?;':""{}[]\|~`"),
                       Pos_X + 20.0, Pos_Y + 150.0, Scale_1, Text_Colour);
      Text_List (2) :=
        (To_Unbounded_String ("The Quick Brown Fox jumps over the zoo's Lazy Dog."),
         Pos_X, Pos_Y, Scale_1, Text_Colour);
      Text_Management.Render_Text (Render_Text_Program, Text_List, Texture_ID,
                                   MVP_Matrix_ID, Dimensions_ID, Colour_ID, MVP_Matrix);
      Text_Management.Render_Text (Render_Text_Program, "Hello again!", 150.0, 10.0,
                                   Scale_1, Text_Colour, Texture_ID, MVP_Matrix_ID,
                                   Dimensions_ID, Colour_ID, MVP_Matrix);
   end Render;

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

      GL.Blending.Set_Blend_Func (Src_Factor => GL.Blending.Src_Alpha,
                                  Dst_Factor => GL.Blending.One_Minus_Src_Alpha);

      Render_Text_Program := Program_From
          ((Src ("src/shaders/text_vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/text_fragment_shader.glsl", Fragment_Shader)));
      Use_Program (Render_Text_Program);

      MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
          (Render_Text_Program, "mvp_matrix");
      Texture_ID := GL.Objects.Programs.Uniform_Location
          (Render_Text_Program, "text_sampler");
      Colour_ID := GL.Objects.Programs.Uniform_Location
        (Render_Text_Program, "text_colour");
      Dimensions_ID := GL.Objects.Programs.Uniform_Location
        (Render_Text_Program, "dimensions");

      Text_Management.Setup (Font_File_1);
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup (Main_Window);
   Utilities.Clear_Background_Colour_And_Depth (Background);
   while Running loop
      Delay (2.0);
      Render (Main_Window);
      GL.Flush;
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and then not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and then not Main_Window.Should_Close;
   end loop;
   Render_Text_Program.Delete_Id;

exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
