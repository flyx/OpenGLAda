

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
with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is

   Render_Program              : GL.Objects.Programs.Program;
   Vertex_Array                : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer               : GL.Objects.Buffers.Buffer;
   Element_Buffer              : GL.Objects.Buffers.Buffer;

   Render_Model_Matrix_ID      : GL.Uniforms.Uniform;
   Render_Projection_Matrix_ID : GL.Uniforms.Uniform;

   --  ------------------------------------------------------------------------

   procedure Render is
      use GL.Types;
      use GL.Objects.Buffers;
      Dark_Blue : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 1.0);
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
      Aspect            : Single;
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
      Render_Program := Program_From
        ((Src ("src/shaders/primitive_restart_vs.glsl", Vertex_Shader),
         Src ("src/shaders/primitive_restart_fs.glsl", Fragment_Shader)));

      GL.Objects.Programs.Use_Program (Render_Program);
      Render_Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "model_matrix");
      Render_Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "projection_matrix");

      Element_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Element_Buffer);
      Utilities.Load_Element_Buffer (Element_Array_Buffer,
                                     Vertex_Data.Vertex_Indices,
                                     Static_Draw);

      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer,
                                    Vertex_Data.Vertex_Positions, Static_Draw);

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
