
with Interfaces.C;
with Interfaces.C.Strings;
with System;

with Ada.Containers.Vectors;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Buffers;
with GL.Errors;
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

with Maths;
with Program_Loader;
with Utilities;

with FT_Glyphs;
with FT_Image;
with FT_Interface;
with FT_Types;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
   type Character_Size is record
      Width     : GL.Types.Single;
      Rows      : GL.Types.Single;
   end record;

   type Character_Bearing is record
      Left      : GL.Types.Single;
      Top       : GL.Types.Single;
   end record;

   type Character_Record is record
      Texture   : GL.Objects.Textures.Texture;
      Size      : Character_Size;
      Bearing   : Character_Bearing;
      Advance_X : GL.Types.Int;
   end record;

   package Data_Vector_Package is new Ada.Containers.Vectors (Natural, Character_Record);
   type Character_Data_Vector is new Data_Vector_Package.Vector with null record;

   theLibrary            : FT_Types.FT_Library;
   Face_Ptr              : FT_Interface.FT_Face;
   Vertex_Array          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer         : GL.Objects.Buffers.Buffer;
   Render_Program        : GL.Objects.Programs.Program;
   Texture_ID            : GL.Uniforms.Uniform;
   Colour_ID             : GL.Uniforms.Uniform;
   Projection_Matrix     : GL.Types.Singles.Matrix4;
   Projection_Matrix_ID  : GL.Uniforms.Uniform;
   Character_Data        : Character_Data_Vector;

   Background      : constant GL.Types.Colors.Color := (0.4, 0.6, 0.6, 1.0);
   Text_Colour     : constant GL.Types.Colors.Basic_Color := (0.2, 0.4, 0.0);

   --  ------------------------------------------------------------------------

    procedure Load_Vertex_Sub_Buffer is new
      GL.Objects.Buffers.Set_Sub_Data (GL.Types.Singles.Vector4_Pointers);

   procedure Render_The_Text (Text   : String; X, Y, Scale : GL.Types.Single;
                              Colour : GL.Types.Colors.Basic_Color);
   procedure Setup_Font;
   procedure Setup_Texture;

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

      Char          : Character;
      Char_Data     : Character_Record;
      X_Orig        : Single := X;
      Y_Orig        : Single := Y;
      X_Pos         : Single;
      Y_Pos         : Single;
      Width         : Single;
      Height        : Single;
      --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
      Vertex_Data   : Singles.Vector4_Array (1 .. 6);
   begin
      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));
      GL.Objects.Textures.Set_Active_Unit (0);

      for index in Text'Range loop
         Put_Line ("Render_The_Text." & Single'Image (Y_Orig));
         Char := Text (index);
         Char_Data := Character_Data.Element (index);
         X_Pos := X_Orig + Char_Data.Bearing.Left * Scale;
         Y_Pos := Y_Orig - (Char_Data.Size.Rows - Char_Data.Bearing.Top) * Scale;
         Width := Char_Data.Size.Width;
         Height := Char_Data.Size.Rows;
         Vertex_Data := ((X_Pos, Y_Pos + Height,         0.0, 0.0),
                         (X_Pos, Y_Pos,                  0.0, 1.0),
                         (X_Pos + Width, Y_Pos,          1.0, 1.0),
                         (X_Pos, Y_Pos + Height,         0.0, 0.0),
                         (X_Pos + Width, Y_Pos,          1.0, 1.0),
                         (X_Pos + Width, Y_Pos + Height, 1.0, 0.0));

         Texture_2D.Bind (Char_Data.Texture);
         Array_Buffer.Bind (Vertex_Buffer);
         Load_Vertex_Sub_Buffer (Array_Buffer, 0, Vertex_Data);

         GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 6);
         --  Bitshift by 6 to get value in pixels (2^6 = 64
         --  (divide amount of 1/64th pixels by 64 to get amount of pixels))
         X_Orig := X_Orig + Single (Char_Data.Advance_X) / 64.0 * Scale;
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

      Setup_Font;
      Setup_Texture;
      FT_Interface.Done_Face (Face_Ptr);
      FT_Interface.Done_Library (theLibrary);

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

   procedure Setup_Font is
      use Interfaces.C;
      Font_File       : String := "/System/Library/Fonts/Helvetica.dfont";
   begin
     if FT_Interface.Init_FreeType (theLibrary) /= 0 then
         Put_Line ("The Freetype Library failed to load.");
         raise FT_Types.FT_Exception;
     end if;
      if FT_Interface.New_Face (theLibrary, Font_File, 0, Face_Ptr) /= 0 then
         Put_Line ("A face failed to load.");
         raise FT_Types.FT_Exception;
      end if;
      if FT_Interface.Set_Pixel_Sizes (Face_Ptr, 0, 48) /= 0 then
         Put_Line ("Unable to set pixel sizes.");
         raise FT_Types.FT_Exception;
      end if;
      GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);  --  Disable byte-alignment restriction
   exception
      when others =>
         Put_Line ("An exception occurred in Setup_Font.");
         raise;
   end Setup_Font;

   --  ------------------------------------------------------------------------

   procedure Setup_Texture is
      use Interfaces.C;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      aFace      : FT_Interface.FT_Face_Record := FT_Interface.Face (Face_Ptr);
      aTexture   : GL.Objects.Textures.Texture;
      Char_Data  : Character_Record;
   begin
      for Char in FT_Types.FT_ULong range 1 .. 128 loop
         if FT_Interface.Load_Character (Face_Ptr, Char, FT_Types.Load_Render) /= 0 then
            Put_Line ("A character failed to load.");
            raise FT_Types.FT_Exception;
         end if;

         Char_Data.Size.Width := FT_Glyphs.Get_Bitmap_Width (aFace.Glyph);
         Char_Data.Size.Rows := Single (FT_Glyphs.Get_Bitmap_Rows (aFace.Glyph));
         Char_Data.Bearing.Left := Single (FT_Glyphs.Get_Bitmap_Left (aFace.Glyph));
         Char_Data.Bearing.Top := Single (FT_Glyphs.Get_Bitmap_Top (aFace.Glyph));
         Char_Data.Advance_X := FT_Image.Vector_X (FT_Glyphs.Get_Glyph_Advance (aFace.Glyph));

         aTexture.Initialize_Id;
         Texture_2D.Bind (aTexture);
         Texture_2D.Load_From_Data (0, GL.Pixels.RGB,
                                    GL.Types.Int (Char_Data.Size.Width),
                                    GL.Types.Int (Char_Data.Size.Rows),
                                    GL.Pixels.RGB, GL.Pixels.Unsigned_Byte,
                                    FT_Glyphs.Get_Bitmap_Image (aFace.Glyph));
         Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
         Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
         Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
         Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge);

         Char_Data.Texture := aTexture;
         Character_Data.Append (Char_Data);
      end loop;
   exception
      when others =>
         Put_Line ("An exception occurred in Setup_Texture.");
         raise;
   end Setup_Texture;

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
