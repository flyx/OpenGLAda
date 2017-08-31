
with System;
with System.Address_Image;

with Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with FT_Glyphs;
with FT_Interface;
with FT_Utilities;

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
   View_Matrix           : Singles.Matrix4 := Singles.Identity4;

   Back_Colour   : constant GL.Types.Colors.Color := (0.3, 0.6, 0.6, 1.0);

   --  ------------------------------------------------------------------------

   procedure Render (Window : in out Glfw.Windows.Window)  is
      use GL.Types;
      use GL.Types.Colors;
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      Num_Vertices   : GL.Types.Int := 2 * 3; -- Two triangles
      Num_Components : GL.Types.Int := 4;     -- Coords vector size;
      Text_Colour    : constant Basic_Color := (0.5, 0.2, 0.6);
   begin
      Utilities.Clear_Background_Colour_And_Depth (Back_Colour);
      Vertex_Array.Bind;

      GL.Objects.Programs.Use_Program (Rendering_Program);

      GL.Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (Char_Texture);
      GL.Uniforms.Set_Int (Texture_Location, 0);
      GL.Uniforms.Set_Single (Colour_Location, Text_Colour (R), Text_Colour (G), Text_Colour (B));
      GL.Uniforms.Set_Single (Projection_Location, Projection_Matrix);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => Num_Components,
                                               Kind   => Single_Type,
                                               Stride => 0, Offset => 0);

      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, Num_Vertices);
      GL.Attributes.Disable_Vertex_Attrib_Array (0);
   exception
      when others =>
         Put_Line ("An exceptiom occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window;
                    Test_Character : Character) is
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use GL.Objects.Textures;
      use Program_Loader;
      Window_Width   : Glfw.Size;
      Window_Height  : Glfw.Size;
   begin
      Window.Get_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      --  Blending allows a fragment colour's alpha value to control the resulting
      --  colour which will be transparent for all the glyph's background colours and
      --  non-transparent for the actual character pixels.
      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha, GL.Blending.One_Minus_Src_Alpha);

      Maths.Init_Orthographic_Transform (Single (Window_Height), 0.0, 0.0,
                        Single (Window_Width), 0.1, -100.0, Projection_Matrix);

      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Rendering_Program := Program_From
          ((Src ("/Ada_Source/OpenGLAda/examples/freetype_gl/single_character/src/shaders/gl1_vertex_shader.glsl", Vertex_Shader),
           Src ("/Ada_Source/OpenGLAda/examples/freetype_gl/single_character/src/shaders/gl1_fragment_shader.glsl", Fragment_Shader)));
      --  Character position must be within window bounds.
      Texture_Manager.Setup_Graphic (Vertex_Buffer, Char_Texture, 50.0, 50.0, 4.0, Test_Character);

      Projection_Location :=
          GL.Objects.Programs.Uniform_Location (Rendering_Program, "projection_matrix");
      Texture_Location :=
          GL.Objects.Programs.Uniform_Location (Rendering_Program, "bitmap_image");
      Colour_Location := GL.Objects.Programs.Uniform_Location
          (Rendering_Program, "text_colour");
      Position_Location :=
          GL.Objects.Programs.Attrib_Location (Rendering_Program, "vertices");
   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
   Test_Character : Character := 'x';
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("No character entered, so dispalying default character.");
   else
      Test_Character := Ada.Command_Line.Argument (1) (1);
   end if;

   Setup (Main_Window, Test_Character);
   while Running loop
      Render (Main_Window);
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
