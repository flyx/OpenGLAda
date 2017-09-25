
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
with GL.Window;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with FT.Interfac;
with FT.Utilities;

with Texture_Manager;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer         : GL.Objects.Buffers.Buffer;
   Render_Program        : GL.Objects.Programs.Program;
   Texture_ID            : GL.Uniforms.Uniform;
   Extended_Ascii_List   : FT.Interfac.Character_Data_Vector (0 .. 255);
   Projection_Matrix     : GL.Types.Singles.Matrix4;
   Projection_Matrix_ID  : GL.Uniforms.Uniform;
   Colour_ID             : GL.Uniforms.Uniform;

   Background      : constant GL.Types.Colors.Color := (0.4, 0.6, 0.6, 1.0);
   Text_Colour     : constant GL.Types.Colors.Basic_Color := (0.5, 0.2, 0.6);
   Font_File_1     : String := "/Library/Fonts/Arial.ttf";
   Font_File_2     : String := "/System/Library/Fonts/Helvetica.dfont";

   --  ------------------------------------------------------------------------

   procedure Render_The_Text (Text   : String; X, Y, Scale : GL.Types.Single;
                              Colour : GL.Types.Colors.Basic_Color);
   procedure Setup_Buffer;

   --  ------------------------------------------------------------------------

   procedure Render (Window  : in out Glfw.Windows.Window) is
      use GL.Types;
      use GL.Objects.Buffers;
      Window_Width    : Glfw.Size;
      Window_Height   : Glfw.Size;
      Pos_X           : GL.Types.Single := 5.0;
      Pos_Y           : GL.Types.Single := 50.0;
      Scale           : GL.Types.Single := 0.5;
   begin
      Window.Get_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Utilities.Clear_Background_Colour_And_Depth (Background);
      Maths.Init_Orthographic_Transform (Single (Window_Height), 0.0, 0.0,
                                         Single (Window_Width), 0.1, -100.0,
                                         Projection_Matrix);
      Render_The_Text ("The Quick Brown Fox jumps over the Lazy Dog.", Pos_X, Pos_Y, Scale, Text_Colour);
      Render_The_Text ("1234567890 !@#$%^&*()_+=,./?;':""{}[]\|~`", Pos_X, 200.0, 0.8, Text_Colour);

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
      use FT.Interfac;
      use Texture_Manager;

      Num_Triangles  : Int := 2;
      Num_Vertices   : GL.Types.Int := Num_Triangles * 3; -- Two triangles
      Num_Components : GL.Types.Int := 4;                 -- Coords vector size;
      Stride         : Int := 0;

      Char           : Character;
      Char_Data      : FT.Interfac.Character_Record;
      Char_Texture   : GL.Objects.Textures.Texture;
      X_Orig         : Single := X;
      Y_Orig         : constant Single := Y;
      X_Pos          : Single;
      Y_Pos          : Single;
      Char_Width     : Single;
      Height         : Single;
      --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
      Vertex_Data    : Singles.Vector4_Array (1 .. Num_Vertices);
   begin
      GL.Objects.Programs.Use_Program (Render_Program);

      for index in Text'Range loop
         Char := Text (index);
         Char_Data := Extended_Ascii_List (Character'Pos (Char));
         X_Pos := X_Orig + Single (Left (Char_Data)) * Scale;
         Y_Pos := Y_Orig - Single (Rows (Char_Data) - Top (Char_Data)) * Scale;
         Char_Width := Single (Width (Char_Data)) * Scale;
         Height := Single (Rows (Char_Data)) * Scale;

         Vertex_Data := ((X_Pos, Y_Pos + Height,             0.0, 0.0),
                         (X_Pos, Y_Pos,                      0.0, 1.0),
                         (X_Pos + Char_Width, Y_Pos,         1.0, 1.0),

                         (X_Pos, Y_Pos + Height,              0.0, 0.0),
                         (X_Pos + Char_Width, Y_Pos,          1.0, 1.0),
                         (X_Pos + Char_Width, Y_Pos + Height, 1.0, 0.0));

         Vertex_Array.Bind;
         Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Dynamic_Draw);

         Char_Texture := Character_Texture (Char_Data);
         if not GL.Objects.Textures.Is_Texture  (Char_Texture.Raw_Id) then
            Put_Line ("Render_The_Text, aTexture is invalid.");
         end if;

         GL.Objects.Textures.Set_Active_Unit (0);
         Texture_2D.Bind (Char_Texture);
         GL.Uniforms.Set_Int (Texture_ID, 0);
         GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));
         GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         Array_Buffer.Bind (Vertex_Buffer);
         GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => Num_Components,
                                                  Kind   => GL.Types.Single_Type,
                                                  Stride => Stride, Offset => 0);

         GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, Num_Vertices);
         GL.Attributes.Disable_Vertex_Attrib_Array (0);
         --  Bitshift by 6 to get value in pixels (2^6 = 64
         --  (divide amount of 1/64th pixels by 64 to get amount of pixels))
         X_Orig := X_Orig + Single (Advance_X (Char_Data)) / 64.0 * Scale;
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
   begin
      Window.Get_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));

      --  Blending allows a fragment colour's alpha value to control the resulting
      --  colour which will be transparent for all the glyph's background colours and
      --  non-transparent for the actual character pixels.

      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                  GL.Blending.One_Minus_Src_Alpha);
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

      Texture_Manager.Setup_Graphic (Font_File_2, Vertex_Buffer,
                                     Extended_Ascii_List, 10.0, 10.0);
      Setup_Buffer;
   exception
      when others =>
         Put_Line ("An exception occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   procedure Setup_Buffer is
      use GL.Objects.Buffers;
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);

   end Setup_Buffer;

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
