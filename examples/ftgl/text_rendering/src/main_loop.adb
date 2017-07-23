
with Interfaces.C;
with System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Objects.Vertex_Arrays;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with FTGL.Fonts;

with Maths;
with Program_Loader;
with Utilities;
with FTGL_Interface;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   package Bitmap_Font_Package is new FTGL_Interface.Setup (FTGL.Fonts.Bitmap_Font);
   package Buffer_Font_Package is new FTGL_Interface.Setup (FTGL.Fonts.Buffer_Font);
   package Pixmap_Font_Package is new FTGL_Interface.Setup (FTGL.Fonts.Pixmap_Font);
   package Polygon_Font_Package is new FTGL_Interface.Setup (FTGL.Fonts.Polygon_Font);

   Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer         : GL.Objects.Buffers.Buffer;
   Render_Program        : GL.Objects.Programs.Program;
   Texture_ID            : GL.Uniforms.Uniform;
   Bitmap_Font_Data      : FTGL_Interface.Character_Data;
   Buffer_Font_Data      : FTGL_Interface.Character_Data;
   Pixmap_Font_Data      : FTGL_Interface.Character_Data;
   Polygon_Font_Data     : FTGL_Interface.Character_Data;
   Colour_ID             : GL.Uniforms.Uniform;
   Projection_Matrix     : GL.Types.Singles.Matrix4;
   Projection_Matrix_ID  : GL.Uniforms.Uniform;

   Background      : constant GL.Types.Colors.Color := (0.9, 0.9, 0.9, 1.0);
   Text_Colour     : constant GL.Types.Colors.Basic_Color := (0.2, 0.4, 0.0);

   --  ------------------------------------------------------------------------

    procedure Load_Vertex_Sub_Buffer is new
      GL.Objects.Buffers.Set_Sub_Data (GL.Types.Singles.Vector4_Pointers);

   procedure Render_The_Text (Text   : String; X, Y, Scale : GL.Types.Single;
                              Colour : GL.Types.Colors.Basic_Color);

   --  ------------------------------------------------------------------------

   procedure Render is
      use GL.Types;
      use GL.Objects.Buffers;
   begin
      Utilities.Clear_Background_Colour (Background);
      Render_The_Text ("Some sample text.", 25.0, 25.0, 1.0, Text_Colour);

   exception
      when  others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Render_The_Text (Text   : String; X, Y, Scale : GL.Types.Single;
                              Colour : GL.Types.Colors.Basic_Color) is
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      use GL.Types.Colors;
      use GL.Types;
      use FTGL_Interface;

      Char_Index    : Integer;
      Char_S        : String (1 .. 1);
      X_Orig        : Single := X;
      Y_Orig        : Single := Y;
      X_Pos         : Single;
      Y_Pos         : Single;
      --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
      Vertex_Data   : Singles.Vector4_Array (1 .. 6);
   begin
      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));
      GL.Objects.Textures.Set_Active_Unit (0);
      Vertex_Array.Bind;

      for index in Text'Range loop
         Char_S := Text (index .. index);
         Char_Index := Character'Pos (Text (index));
         X_Pos := X_Orig + Char_Bearing_X (Bitmap_Font_Data) * Scale;
         Y_Pos := Y_Orig - (Char_Width (Bitmap_Font_Data) - Char_Bearing_Y (Bitmap_Font_Data)) * Scale;
         Vertex_Data := ((X_Pos, Y_Pos + Char_Height (Bitmap_Font_Data), 0.0, 0.0),
                         (X_Pos, Y_Pos,                    0.0, 1.0),
                         (X_Pos + Char_Width (Bitmap_Font_Data), Y_Pos,  1.0, 1.0),
                         (X_Pos, Y_Pos + Char_Height (Bitmap_Font_Data), 0.0, 0.0),
                         (X_Pos + Char_Width (Bitmap_Font_Data), Y_Pos,  1.0, 1.0),
                         (X_Pos + Char_Width (Bitmap_Font_Data),
                          Y_Pos + Char_Height (Bitmap_Font_Data),        1.0, 0.0));

         Texture_2D.Bind (Char_Texture (Bitmap_Font_Data));
         Array_Buffer.Bind (Vertex_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Dynamic_Draw);

         Load_Vertex_Sub_Buffer (Array_Buffer, 0, Vertex_Data);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 1, Single_Type, 0, 0);

         GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 6);
         X_Orig := X_Orig + Char_Advance (Bitmap_Font_Data) * Scale;
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in Render_The_Text.");
         raise;
   end Render_The_Text;

   --      --  ------------------------------------------------------------------------

   procedure Setup  (Window  : in out Glfw.Windows.Window) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types;
      use Program_Loader;

      Font_Bitmap     : FTGL.Fonts.Bitmap_Font;
      Font_Buffer     : FTGL.Fonts.Buffer_Font;
      Font_Pixmap     : FTGL.Fonts.Pixmap_Font;
      Font_Polygon    : FTGL.Fonts.Polygon_Font;
      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;
   begin
--        Bitmap_Font_Package.Setup_Font (Font_Bitmap, Bitmap_Font_Data,
--                                        "/System/Library/Fonts/Helvetica.dfont");
--        Pixmap_Font_Package.Setup_Font (Font_Pixmap, Pixmap_Font_Data,
--                                        "/System/Library/Fonts/Helvetica.dfont");
--        Polygon_Font_Package.Setup_Font (Font_Polygon, Polygon_Font_Data,
--                                        "/System/Library/Fonts/Helvetica.dfont");
      Buffer_Font_Package.Setup_Font (Font_Buffer, Buffer_Font_Data,
                                      "/System/Library/Fonts/Helvetica.dfont");

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);

      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                  GL.Blending.One_Minus_Src_Alpha);

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

      Maths.Init_Orthographic_Transform (0.0, Single (Window_Width),
                                         0.0, Single (Window_Height),
                                         -100.0, 100.0, Projection_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);

   exception
      when others =>
         Put_Line ("An exception occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup (Main_Window);
   while Running loop
      Render;
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and then not
          (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and then not Main_Window.Should_Close;
   end loop;
   Put_Line ("Finished.");

   Vertex_Buffer.Delete_Id;
   Vertex_Array.Delete_Id;
   Render_Program.Delete_Id;
exception
   when anError : FTGL.FTGL_Error =>
      Put_Line ("Main_Loop returned an FTGL error: ");
      raise;

   when anError :  others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
