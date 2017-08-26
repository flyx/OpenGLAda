
with Ada.Exceptions; use Ada.Exceptions;
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

with Maths;
with Program_Loader;
with Utilities;

with Texture_Manager;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   use GL.Types;

   Rendering_Program     : GL.Objects.Programs.Program;
   Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer         : Texture_Manager.V_Buffer;
   Char_Texture          : GL.Objects.Textures.Texture;
   Colour_Location       : GL.Uniforms.Uniform;
   Projection_Location   : GL.Uniforms.Uniform;
   Texture_Location      : GL.Uniforms.Uniform;
   Position_Location     : GL.Attributes.Attribute;
   Projection_Matrix     : Singles.Matrix4 := Singles.Identity4;

   --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
   Vertex_Data           : GL.Types.Singles.Vector4_Array (1 .. 6);

   Back_Colour   : constant GL.Types.Colors.Color := (0.3, 0.6, 0.6, 1.0);

   --  ------------------------------------------------------------------------

   procedure Render is
      use GL.Types;
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      Num_Vertices  : Int := 12;

   begin
      Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

      GL.Objects.Programs.Use_Program (Rendering_Program);

      GL.Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (Char_Texture);
      GL.Uniforms.Set_Int (Texture_Location, 0);

      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, Num_Vertices);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup is
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use GL.Objects.Textures;
      use Program_Loader;
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Static_Draw);

      Rendering_Program := Program_From
          ((Src ("src/shaders/gl1_vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/gl1_fragment_shader.glsl", Fragment_Shader)));

      Texture_Manager.Setup_Graphic (Vertex_Buffer, Vertex_Data, Char_Texture,
                                     -400.0, 10.0, 1.0 / 512.0);

      Projection_Location :=
          GL.Objects.Programs.Uniform_Location (Rendering_Program, "projection_matrix");
      Texture_Location :=
          GL.Objects.Programs.Uniform_Location (Rendering_Program, "bitmap_image");
      Colour_Location := GL.Objects.Programs.Uniform_Location
          (Rendering_Program, "text_colour");
      Position_Location :=
          GL.Objects.Programs.Attrib_Location (Rendering_Program, "vertices");
--        Maths.Init_Orthographic_Transform (0.0, 800.0, 0.0, 600.0, -1.0, 100.0, Projection_Matrix);
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
