

with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
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

with Program_Loader;
with Utilities;

with Feedback;
with Transform_Feedback_API;
with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;
   use Ada.Strings.Unbounded;

   type Varyings_Array_1 is new Transform_Feedback_API.Varyings_Array (1 .. 1);
   type Varyings_Array_2 is new Transform_Feedback_API.Varyings_Array (1 .. 2);

   Vertex_Array      : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer     : GL.Objects.Buffers.Buffer;
   Model_Matrix      : constant Singles.Matrix4 := GL.Types.Singles.Identity4;
   View_Matrix       : Singles.Matrix4;
   Projection_Matrix : Singles.Matrix4;
   MVP_Matrix        : Singles.Matrix4;
   Render_Program    : GL.Objects.Programs.Program;
   Update_Program    : GL.Objects.Programs.Program;
   Varyings          : constant Varyings_Array_2 :=
     (To_Unbounded_String ("position_out"),
      To_Unbounded_String ("velocity_out"));
   Varyings_2          : constant Varyings_Array_1 :=
     (Varyings_Array_1'First => To_Unbounded_String ("world_space_position"));

   --  ------------------------------------------------------------------------

   procedure Render is
      use GL.Objects.Buffers;
      Dark_Blue : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 1.0);
   begin
      Utilities.Clear_Background_Colour (Dark_Blue);
      GL.Objects.Programs.Use_Program (Render_Program);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      Array_Buffer.Bind (Vertex_Buffer);

      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 3);
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when  others =>
         Put_Line ("An exceptiom occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup is
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use Program_Loader;

      Model_Matrix_ID             : GL.Uniforms.Uniform;
      Projection_Matrix_ID        : GL.Uniforms.Uniform;
      Triangle_Count_ID           : GL.Uniforms.Uniform;
      Time_Step_ID                : GL.Uniforms.Uniform;
      Render_Model_Matrix_ID      : GL.Uniforms.Uniform;
      Render_Projection_Matrix_ID : GL.Uniforms.Uniform;
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer,
                                    Vertex_Data.Vertex_Buffer_Data, Static_Draw);

      Render_Program := Program_From
        ((Src ("src/shaders/render_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/blue_fragment_shader.glsl", Fragment_Shader)));

      Update_Program := Program_From
        ((Src ("src/shaders/update_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/white_fragment_shader.glsl", Fragment_Shader)));

      Feedback.Transform_Feedback_Varyings (Update_Program, 2,
                                            Transform_Feedback_API.Varyings_Array (Varyings),
                                            Transform_Feedback_API.GL_Interleaved_Attribs);

      Feedback.Transform_Feedback_Varyings (Update_Program, 2,
                                            Transform_Feedback_API.Varyings_Array (Varyings_2),
                                            Transform_Feedback_API.GL_Interleaved_Attribs);
      Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "model_matrix");
      Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "projection_matrix");
      Triangle_Count_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "triangle_count");
      Time_Step_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "time_step");
      Render_Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "model_matrix");
      Render_Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "projection_matrix");

      GL.Uniforms.Set_Single (Model_Matrix_ID, Model_Matrix);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup;
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
      Put_Line ("An exceptiom occurred in Main_Loop.");
      raise;
end Main_Loop;
