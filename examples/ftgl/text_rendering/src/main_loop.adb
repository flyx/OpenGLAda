
with Interfaces.C;
with System;

with Ada.Containers.Ordered_Maps;
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
   type Character_Data is record
      Texture_ID        : GL.Objects.Textures.Texture;
      Width             : GL.Types.Single;
      Height            : GL.Types.Single;
      Advance           : GL.Types.Single;
      Ascend            : GL.Types.Single;
      Descend           : GL.Types.Single;
      Bearing_X         : GL.Types.Single;
      Bearing_Y         : GL.Types.Single;
      Valid             : Boolean := False;
   end record;

   package Characters_Package is new Ada.Containers.Ordered_Maps (Character, Character_Data);
   type Chars_Map_Type is new Characters_Package.Map with null record;
   type Chars_Cursor is new Characters_Package.Cursor;

   theFont               : FTGL.Fonts.Bitmap_Font;
   Chars_Map             : Chars_Map_Type;
   Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer         : GL.Objects.Buffers.Buffer;
   Render_Program        : GL.Objects.Programs.Program;
   Texture_ID            : GL.Uniforms.Uniform;
   Font_Texture          : GL.Objects.Textures.Texture;
   Colour_ID             : GL.Uniforms.Uniform;
   Projection_Matrix     : GL.Types.Singles.Matrix4;
   Projection_Matrix_ID  : GL.Uniforms.Uniform;

   --  ------------------------------------------------------------------------

    procedure Load_Vertex_Sub_Buffer is new
      GL.Objects.Buffers.Set_Sub_Data (GL.Types.Singles.Vector4_Pointers);

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

   procedure Load_Char_Vector (Scale : GL.Types.Single := 1.0) is
      use GL.Types;
      Char_Map_List   : FTGL.Fonts.Charset_List := theFont.Get_Char_Map_List;
      Char_Map        : FTGL.Charset := Char_Map_List (5);
      Char_Data       : Character_Data;
      Face_Size       : GL.Types.Int := GL.Types.Int (theFont.Get_Font_Face_Size);
      aChar           : Character;
      Char_S          : String := " ";  --  BBox array elements:
      --  1 lower  2 left  3 near
      --  4 upper  5 right 6 far
      BBox          : FTGL.Bounding_Box;
   begin
      for Index in 0 .. 128 loop
         aChar := Character'Val (Index);
         Char_S (1) := aChar;
         BBox := theFont.Bounds (Char_S);
         Char_Data.Ascend := theFont.Ascender;
         Char_Data.Descend := theFont.Descender;
         Char_Data.Advance := theFont.Advance_Width (Char_S);
         Char_Data.Height := (BBox (4) - BBox (1)) * Scale;
         Char_Data.Width  := (BBox (5) - BBox (2)) * Scale;
         Char_Data.Bearing_X := 0.5 * Char_Data.Width;
         Char_Data.Bearing_Y := Char_Data.Height - Char_Data.Descend;
         Chars_Map.Insert (aChar, Char_Data);
      end loop;

   exception
      when anError : FTGL.FTGL_Error =>
      Put_Line ("Generate_Texture returned an FTGL error: ");
         raise;
      when  others =>
         Put_Line ("An exception occurred in Load_Char_Vector.");
         raise;
   end Load_Char_Vector;

   --  ------------------------------------------------------------------------

   procedure Render is
      use GL.Types;
      use GL.Objects.Buffers;
      Background      : constant GL.Types.Colors.Color := (0.9, 0.9, 0.9, 1.0);
      Text_Colour     : constant GL.Types.Colors.Basic_Color := (0.2, 0.4, 0.0);
   begin
      Utilities.Clear_Background_Colour (Background);
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
      use GL.Objects.Textures.Targets;
      use GL.Types.Colors;
      use GL.Types;
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
--           X_Pos := X_Orig + Bearing_X * Scale;
--           Y_Pos := Y_Orig - (Width - Bearing_Y) * Scale;
--           Vertex_Data := ((X_Pos, Y_Pos + Height,         0.0, 0.0),
--                           (X_Pos, Y_Pos,                  0.0, 1.0),
--                           (X_Pos + Width, Y_Pos,          1.0, 1.0),
--                           (X_Pos, Y_Pos + Height,         0.0, 0.0),
--                           (X_Pos + Width, Y_Pos,          1.0, 1.0),
--                           (X_Pos + Width, Y_Pos + Height, 1.0, 0.0));

         Texture_2D.Bind (Font_Texture);
         Array_Buffer.Bind (Vertex_Buffer);
         Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Dynamic_Draw);

--       generic
--           with package Pointers is new Interfaces.C.Pointers (<>);
--           Get_Sub_Data (Target : in out Buffer_Target;
--                             Offset : Types.Size;
--                             Data   : in out Pointers.Element_Array);
         Load_Vertex_Sub_Buffer (Array_Buffer, 0, Vertex_Data);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 1, Single_Type, 0, 0);

         GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 6);
--           X_Orig := X_Orig + Advance * Scale;
      end loop;

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
