

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

with Feedback;
with Transform_Feedback_API;
with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is

   Vertex_Array   : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer  : GL.Objects.Buffers.Buffer;
   Render_Program : GL.Objects.Programs.Program;
   Update_Program : GL.Objects.Programs.Program;
   Varyings       : constant Transform_Feedback_API.Varyings_Array (1 .. 2) :=
     ("position_out", "velocity_out");


   --  ------------------------------------------------------------------------

   procedure Render is
      use GL.Types;
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

      Feedback.Transform_Feedback_Varyings (Update_Program, 2, Varyings,
                                            Transform_Feedback_API.GL_Interleaved_Attribs);

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
