
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
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Raster;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with FTGL.Fonts;

with Maths;
with Program_Loader;
with Utilities;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   theFont               : FTGL.Fonts.Bitmap_Font;

   Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer         : GL.Objects.Buffers.Buffer;
   Render_Program        : GL.Objects.Programs.Program;
   Texture_ID            : GL.Uniforms.Uniform;
   Font_Texture          : GL.Objects.Textures.Texture;
   Colour_ID             : GL.Uniforms.Uniform;
   Projection_Matrix     : GL.Types.Singles.Matrix4;
   Projection_Matrix_ID  : GL.Uniforms.Uniform;

   --  ------------------------------------------------------------------------

   procedure Load_Buffer is new
       GL.Objects.Buffers.Load_To_Buffer (GL.Types.Single_Pointers);

   procedure Render_The_Text (Text   : String; X, Y, Scale : GL.Types.Single;
                              Colour : GL.Types.Colors.Basic_Color);

   --  ------------------------------------------------------------------------

   procedure Generate_Texture is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use GL.Pixels;

      Char_Map_List   : FTGL.Fonts.Charset_List := theFont.Get_Char_Map_List;
      Char_Map        : FTGL.Charset := Char_Map_List (5);
      Face_Size       : GL.Types.Int := GL.Types.Int (theFont.Get_Font_Face_Size);
      Buffer          : String (128 .. 255);
      aChar           : String := "Z";
      Width           : GL.Types.Single;
      Image_Address   : GL.Objects.Textures.Image_Source;
      Image_Columns : GL.Types.Size := 256;
      Image_Rows    : GL.Types.Size := 256;

   begin
      for i in Buffer'Range loop
         Buffer (i) := Character'Val (i);
      end loop;
      Width := theFont.Advance_Width (aChar);
      Image_Address := Image_Source (Char_Map'Address);

      Font_Texture.Initialize;
      Texture_2D.Bind (Font_Texture);
      Texture_2D.Load_From_Data  (0, RGB, Face_Size, Face_Size, Red,
                                  Unsigned_Byte, Image_Address);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);

   exception
      when anError : FTGL.FTGL_Error =>
      Put_Line ("Generate_Texture returned an FTGL error: ");
         raise;
      when  others =>
         Put_Line ("An exception occurred in Generate_Texture.");
         raise;
   end Generate_Texture;

   --  ------------------------------------------------------------------------

   procedure Render is
      use GL.Types;
      use GL.Objects.Buffers;
      Background      : constant GL.Types.Colors.Color := (0.2, 0.3, 0.3, 1.0);
      Text_Colour     : constant GL.Types.Colors.Basic_Color := (0.5, 0.8, 0.2);
   begin
      --        Utilities.Clear_Background_Colour (Background);
      --          GL.Objects.Programs.Use_Program (Render_Program);
      --          Put_Line ("Rendering text.");
      Render_The_Text ("Some sample text.", 25.0, 25.0, 1.0, Text_Colour);

   exception
      when anError : FTGL.FTGL_Error =>
      Put_Line ("Render returned an FTGL error: ");
         raise;
      when  others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Render_The_Text (Text   : String; X, Y, Scale : GL.Types.Single;
                              Colour : GL.Types.Colors.Basic_Color) is
      use GL.Objects.Buffers;
      use GL.Types.Colors;
      use GL.Types;
      --  2D quad requires 6 vertices of 4 floats
      Vertex_Data        : Single_Array (1 .. 6 * 4);
   begin
      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));

      Vertex_Data := (0.0, -theFont.Descender, -1.5);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 1, Single_Type, 0, 0);

      Array_Buffer.Bind (Vertex_Buffer);

      Load_Buffer (Array_Buffer, Vertex_Data, Dynamic_Draw);

   exception
      when anError : FTGL.FTGL_Error =>
      Put_Line ("Render_The_Text returned an FTGL error: ");
         raise;
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

      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;
      Face_Size       : UInt := 72;
      Font_File       : String := "/System/Library/Fonts/Helvetica.dfont";
   begin
      theFont.Initialize;
      theFont.Load (Font_File);
      if not theFont.Loaded then
         Put_Line (Font_File & " failed to load.");
         raise FTGL.FTGL_Error;
      end if;
      theFont.Set_Font_Face_Size (Face_Size);

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);

      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha, GL.Blending.One_Minus_Src_Alpha);

      Render_Program := Program_From
          ((Src ("src/shaders/text_vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/text_fragment_shader.glsl", Fragment_Shader)));
      Use_Program (Render_Program);

      Generate_Texture;

      Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "projection_matrix");
      Texture_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "texture_sampler");
      Colour_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "texture_colour");

      Maths.Init_Orthographic_Transform (0.0, Single (Window_Width),
                                         0.0, Single (Window_Height),
                                         -100.0, 100.0, Projection_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);

   exception
      when anError : FTGL.FTGL_Error =>
         Put_Line ("Setup returned an FTGL error: ");
         raise;
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
