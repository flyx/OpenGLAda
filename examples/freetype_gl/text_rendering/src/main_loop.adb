
with Interfaces.C;
with Interfaces.C.Strings;
with System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with FT.Glyphs;
with FT.Interfac;
with FT.Utilities;

with Texture_Manager;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer         : GL.Objects.Buffers.Buffer;
   Render_Program        : GL.Objects.Programs.Program;
   Texture_ID            : GL.Uniforms.Uniform;
   Colour_ID             : GL.Uniforms.Uniform;
   aTexture              : GL.Objects.Textures.Texture;
   Projection_Matrix     : GL.Types.Singles.Matrix4;
   Projection_Matrix_ID  : GL.Uniforms.Uniform;

   Background      : constant GL.Types.Colors.Color := (0.4, 0.6, 0.6, 1.0);
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
      Utilities.Clear_Background_Colour_And_Depth (Background);
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
      use Texture_Manager;

      Char          : Character;
      Char_Data     : Texture_Manager.Character_Record;
      X_Orig        : Single := X;
      Y_Orig        : Single := Y;
      X_Pos         : Single;
      Y_Pos         : Single;
      Char_Width    : Single;
      Height        : Single;
      --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
      Vertex_Data   : Singles.Vector4_Array (1 .. 6);
   begin
      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));
      GL.Objects.Textures.Set_Active_Unit (0);
      GL.Uniforms.Set_Int (Texture_ID, 0);  --  Added

      Vertex_Array.Bind;

      for index in Text'Range loop
         Char := Text (index);
         Char_Data := Data (Index);
         X_Pos := X_Orig + Left (Char_Data) * Scale;
         Y_Pos := Y_Orig - (Rows (Char_Data) - Top (Char_Data)) * Scale;
         Char_Width := Width (Char_Data);
         Height := Rows (Char_Data);
         Vertex_Data := ((X_Pos, Y_Pos + Height,         0.0, 0.0),
                         (X_Pos, Y_Pos,                  0.0, 1.0),
                         (X_Pos + Char_Width, Y_Pos,          1.0, 1.0),
                         (X_Pos, Y_Pos + Height,         0.0, 0.0),
                         (X_Pos + Char_Width, Y_Pos,          1.0, 1.0),
                         (X_Pos + Char_Width, Y_Pos + Height, 1.0, 0.0));

         GL.Attributes.Enable_Vertex_Attrib_Array (0);  --  Added
         Texture_2D.Bind (Char_Texture (Char_Data));
         Array_Buffer.Bind (Vertex_Buffer);
         Load_Vertex_Sub_Buffer (Array_Buffer, 0, Vertex_Data);

         GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 6);
         GL.Attributes.Disable_Vertex_Attrib_Array (0);  --  Added
         --  Bitshift by 6 to get value in pixels (2^6 = 64
         --  (divide amount of 1/64th pixels by 64 to get amount of pixels))
         X_Orig := X_Orig + Single (Advance_X (Char_Data)) / 64.0 * Scale;
--           Put_Line ("X origin: " & Single'Image (X_Orig));
      end loop;

   exception
      when  others =>
         Put_Line ("An exception occurred in Render_The_Text.");
         raise;
   end Render_The_Text;

   --  ------------------------------------------------------------------------

   procedure Setup  (Window  : in out Glfw.Windows.Window) is
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types;
      use Program_Loader;

      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;
      Pos_X           : GL.Types.Single := 10.0;
      Pos_Y           : GL.Types.Single := 10.0;
      Cache           : String := "Hello";
   begin
--        FTGL_Interface.Setup_Font (Font_Bitmap, Texture_Map,
--                                        "/System/Library/Fonts/Helvetica.dfont");
--        FTGL_Interface.Setup_Font (Font_Pixmap, Pixmap_Font_Data,
--                                        "/System/Library/Fonts/Helvetica.dfont");
--        FTGL_Interface.Setup_Font (Font_Polygon, Polygon_Font_Data,
--                                        "/System/Library/Fonts/Helvetica.dfont");
--        FTGL_Interface.Setup_Font (Font_Buffer, Buffer_Font_Data,
--                                        "/System/Library/Fonts/Helvetica.dfont");

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

      Vertex_Buffer.Initialize_Id;
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);

      Texture_Manager.Setup_Graphic (Vertex_Buffer,
                                     aTexture, Pos_X, Pos_Y, 1.0, Cache);

      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      GL.Objects.Buffers.Allocate (GL.Objects.Buffers.Array_Buffer,
                 Single'Size / 8 * 6 * 4,  GL.Objects.Buffers.Dynamic_Draw);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Index => 0, Count  => 4,
                                               Kind => Single_Type, Stride => 4,
                                               Offset => 0);
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
--     when anError : FTGL.FTGL_Error =>
--        Put_Line ("Main_Loop returned an FTGL error: ");
--        raise;

   when anError :  others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
