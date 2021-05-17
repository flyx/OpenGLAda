--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Unchecked_Deallocation;
with FT.Glyphs;
with GL.Attributes;
with GL.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Shaders;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Window;
with GL.Text.UTF8;

package body GL.Text is
   procedure Load_Vectors is new GL.Objects.Buffers.Load_To_Buffer
     (GL.Types.Singles.Vector2_Pointers);

   procedure Create (Object : in out Shader_Program_Reference) is
      Vertex_Shader :
        GL.Objects.Shaders.Shader (GL.Objects.Shaders.Vertex_Shader);
      Fragment_Shader :
        GL.Objects.Shaders.Shader (GL.Objects.Shaders.Fragment_Shader);
      Square : constant GL.Types.Singles.Vector2_Array :=
        ((0.0, 0.0), (1.0, 0.0), (0.0, 1.0), (1.0, 1.0));
      LF : constant Character := Character'Val (10);
   begin
      Object.Id.Initialize_Id;

      --  shader sources are included here so that the user does not need to
      --  handle additional resource files bundled with this library.
      Vertex_Shader.Initialize_Id;
      Vertex_Shader.Set_Source ("#version 410 core"                    & LF &
        "layout(location = 0) in vec2 vertex;"                         & LF &
        "uniform vec4 character_info;"                                 & LF &
        "uniform mat4 transformation;"                                 & LF &
        "out vec2 texture_coords;"                                     & LF &
        "void main() {"                                                & LF &
        "  vec2 translated = vec2("                                    &
              "character_info.z * vertex.x + character_info.x,"        &
              "character_info.w * vertex.y + character_info.y);"       & LF &
        "  gl_Position = transformation * vec4(translated, 0.0, 1.0);" & LF &
        "  texture_coords = vec2(vertex.x, 1.0 - vertex.y);"           & LF &
        "}");
      Vertex_Shader.Compile;
      if not Vertex_Shader.Compile_Status then
         raise Rendering_Error with "could not compile vertex shader:" &
           Character'Val (10) & Vertex_Shader.Info_Log;
      end if;
      Object.Id.Attach (Vertex_Shader);

      Fragment_Shader.Initialize_Id;
      Fragment_Shader.Set_Source ("#version 410 core"              & LF &
        "in vec2 texture_coords;"                                  & LF &
        "layout(location = 0) out float color;"                    & LF &
        "uniform sampler2D text_sampler;"                          & LF &
        "uniform vec4 text_color;"                                 & LF &
        "void main() {"                                            & LF &
        "  float alpha = texture(text_sampler, texture_coords).r;" & LF &
        "  color = alpha;"                                         & LF &
        "}");
      Fragment_Shader.Compile;
      if not Fragment_Shader.Compile_Status then
         raise Rendering_Error with "could not compile fragment shader: " &
           Character'Val (10) & Fragment_Shader.Info_Log;
      end if;
      Object.Id.Attach (Fragment_Shader);

      Object.Id.Link;
      if not Object.Id.Link_Status then
         raise Rendering_Error with "could not link program:" &
           Character'Val (10) & Object.Id.Info_Log;
      end if;
      GL.Objects.Shaders.Release_Shader_Compiler;

      Object.Square_Buffer.Initialize_Id;
      Object.Square_Array.Initialize_Id;
      Object.Square_Array.Bind;
      GL.Objects.Buffers.Array_Buffer.Bind (Object.Square_Buffer);
      Load_Vectors (GL.Objects.Buffers.Array_Buffer, Square,
                    GL.Objects.Buffers.Static_Draw);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (0, 2, GL.Types.Single_Type, False, 0, 0);

      Object.Info_Id := Object.Id.Uniform_Location ("character_info");
      Object.Texture_Id := Object.Id.Uniform_Location ("text_sampler");
      Object.Color_Id := Object.Id.Uniform_Location ("text_colour");
      Object.Transform_Id := Object.Id.Uniform_Location ("transformation");
   end Create;

   function Created (Object : Shader_Program_Reference) return Boolean is
   begin
      return Object.Id.Initialized;
   end Created;

   procedure Create (Object : in out Renderer_Reference;
                     Program : Shader_Program_Reference;
                     Face : FT.Faces.Face_Reference) is
   begin
      Finalize (Object);
      Object.Data := new Renderer_Data'(Face => Face, Refcount => 1,
                                        Program => Program,
                                        Characters => <>);
   end Create;

   procedure Create (Object : in out Renderer_Reference;
                     Program : Shader_Program_Reference;
                     Font_Path  : UTF_8_String;
                     Face_Index : FT.Faces.Face_Index_Type;
                     Size : Pixel_Size) is
      Lib : FT.Library_Reference;
   begin
      Finalize (Object);
      Lib.Init;
      Object.Data := new Renderer_Data;
      Object.Data.Program := Program;
      FT.Faces.New_Face (Lib, Font_Path, Face_Index, Object.Data.Face);
      Object.Data.Face.Set_Pixel_Sizes (0, FT.UInt (Size));
   end Create;

   function Created (Object : Renderer_Reference) return Boolean is
   begin
      return Object.Data /= null;
   end Created;


   function Character_Data (Object : Renderer_Reference;
                            Code_Point : UTF8.UTF8_Code_Point)
                            return Loaded_Characters.Cursor is
      use type FT.Position;
      use type FT.Faces.Char_Index_Type;
      use type UTF8.UTF8_Code_Point;
   begin
      return Ret : Loaded_Characters.Cursor :=
        Object.Data.Characters.Find (FT.ULong (Code_Point)) do
         if not Loaded_Characters.Has_Element (Ret) then
            declare
               Index : constant FT.Faces.Char_Index_Type :=
                 Object.Data.Face.Character_Index (FT.ULong (Code_Point));
            begin
               if Index = FT.Faces.Undefined_Character_Code then
                  if Code_Point = Character'Pos ('?') then
                     raise FT.FreeType_Exception with
                       "Font is missing character '?'";
                  else
                     Ret := Character_Data (Object, Character'Pos ('?'));
                     return;
                  end if;
               else
                  Object.Data.Face.Load_Glyph (Index, FT.Faces.Load_Render);
                  FT.Glyphs.Render_Glyph (Object.Data.Face.Glyph_Slot,
                                          FT.Faces.Render_Mode_Mono);
               end if;
            end;

            declare
               use GL.Objects.Textures.Targets;
               New_Data : Loaded_Character;
               Bitmap : constant FT.Bitmap_Record :=
                 FT.Glyphs.Bitmap (Object.Data.Face.Glyph_Slot);
               Inserted : Boolean;
               Top : constant Pixel_Difference := Pixel_Difference
                 (FT.Glyphs.Bitmap_Top (Object.Data.Face.Glyph_Slot));
               Height : constant Pixel_Difference :=
                 Pixel_Difference (Bitmap.Rows);
               Old_Alignment : constant GL.Pixels.Alignment :=
                 GL.Pixels.Unpack_Alignment;
            begin
               New_Data.Width := Pixel_Difference (Bitmap.Width);
               New_Data.Y_Min := Top - Height;
               New_Data.Y_Max := Top;
               New_Data.Advance := Pixel_Difference
                 (FT.Glyphs.Advance (Object.Data.Face.Glyph_Slot).X / 64);
               New_Data.Left := Pixel_Difference
                 (FT.Glyphs.Bitmap_Left (Object.Data.Face.Glyph_Slot));
               New_Data.Image.Initialize_Id;
               Texture_2D.Bind (New_Data.Image);
               Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
               Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
               Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
               Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
               GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);
               Texture_2D.Load_From_Data
                 (0, GL.Pixels.Red, GL.Types.Size (Bitmap.Width),
                  GL.Types.Size (Bitmap.Rows), GL.Pixels.Red,
                  GL.Pixels.Unsigned_Byte,
                  GL.Objects.Textures.Image_Source (Bitmap.Buffer));
               GL.Pixels.Set_Unpack_Alignment (Old_Alignment);
               Object.Data.Characters.Insert (FT.ULong (Code_Point),
                                              New_Data, Ret, Inserted);
            end;
         end if;
      end return;
   end Character_Data;

   procedure Calculate_Dimensions (Object : Renderer_Reference;
                                   Content : UTF_8_String;
                                   Width : out Pixel_Size;
                                   Y_Min : out Pixel_Difference;
                                   Y_Max : out Pixel_Size)
   is
      Char_Position : Integer := Content'First;
      Map_Position : Loaded_Characters.Cursor;
      Code_Point : UTF8.UTF8_Code_Point;
   begin
      Width := 0;
      Y_Min := 0;
      Y_Max := 0;
      while Char_Position <= Content'Last loop
         UTF8.Read (Content, Char_Position, Code_Point);
         Map_Position := Character_Data (Object, Code_Point);
         declare
            Char_Data : constant Loaded_Character :=
              Loaded_Characters.Element (Map_Position);
         begin
            Width := Width + Char_Data.Advance;
            Y_Min := Pixel_Difference'Min (Y_Min, Char_Data.Y_Min);
            Y_Max := Pixel_Difference'Max (Y_Max, Char_Data.Y_Max);
         end;
      end loop;
   end Calculate_Dimensions;

   function To_Texture (Object : Renderer_Reference; Content : UTF_8_String;
                        Text_Color : GL.Types.Colors.Color)
                        return GL.Objects.Textures.Texture is
      Width, Y_Min, Y_Max : Pixel_Difference;
   begin
      Object.Calculate_Dimensions (Content, Width, Y_Min, Y_Max);
      return Object.To_Texture (Content, Width, Y_Min, Y_Max, Text_Color);
   end To_Texture;

   function To_Texture (Object : Renderer_Reference; Content : UTF_8_String;
                        Width, Y_Min, Y_Max : Pixel_Difference;
                        Text_Color : GL.Types.Colors.Color)
                        return GL.Objects.Textures.Texture is
      use type GL.Types.Singles.Matrix4;
      use type GL.Types.Single;
      package Fb renames GL.Objects.Framebuffers;
      package Tx renames GL.Objects.Textures;
      package Va renames GL.Objects.Vertex_Arrays;
      FrameBuf : Fb.Framebuffer;
      Target : GL.Objects.Textures.Texture;
      Char_Position : Integer := Content'First;
      Map_Position : Loaded_Characters.Cursor;
      Code_Point : UTF8.UTF8_Code_Point;
      X_Offset : Pixel_Difference := 0;
      Height : constant Pixel_Difference := Y_Max - Y_Min;
      Transformation : constant GL.Types.Singles.Matrix4 :=
        ((1.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.0), (0.0, 0.0, 1.0, 0.0),
         (-1.0, -1.0, 0.0, 1.0)) *
          ((2.0 / GL.Types.Single (Width), 0.0, 0.0, 0.0),
           (0.0, 2.0 / GL.Types.Single (Height), 0.0, 0.0),
           (0.0, 0.0, 1.0, 0.0), (0.0, 0.0, 0.0, 1.0));
      Old_X, Old_Y : GL.Types.Int;
      Old_Width, Old_Height : GL.Types.Size;
   begin
      FrameBuf.Initialize_Id;
      Fb.Draw_Target.Bind (FrameBuf);
      Target.Initialize_Id;
      Tx.Targets.Texture_2D.Bind (Target);
      Tx.Targets.Texture_2D.Load_Empty_Texture
        (0, GL.Pixels.Red, GL.Types.Int (Width), GL.Types.Int (Height));
      Tx.Targets.Texture_2D.Set_Minifying_Filter (Tx.Nearest);
      Tx.Targets.Texture_2D.Set_Magnifying_Filter (Tx.Nearest);
      GL.Window.Get_Viewport (Old_X, Old_Y, Old_Width, Old_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Size (Width),
                              GL.Types.Size (Y_Max - Y_Min));
      Fb.Draw_Target.Attach_Texture (Fb.Color_Attachment_0, Target, 0);
      GL.Buffers.Set_Active_Buffer (GL.Buffers.Color_Attachment0);
      Tx.Set_Active_Unit (0);
      Object.Data.Program.Id.Use_Program;
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Uniforms.Set_Int (Object.Data.Program.Texture_Id, 0);
      GL.Uniforms.Set_Single (Object.Data.Program.Color_Id,
                              GL.Types.Single (Text_Color (GL.Types.Colors.R)),
                              GL.Types.Single (Text_Color (GL.Types.Colors.G)),
                              GL.Types.Single (Text_Color (GL.Types.Colors.B)),
                              GL.Types.Single (Text_Color (GL.Types.Colors.A)));
      GL.Uniforms.Set_Single (Object.Data.Program.Transform_Id, Transformation);
      Object.Data.Program.Square_Array.Bind;
      GL.Objects.Buffers.Array_Buffer.Bind (Object.Data.Program.Square_Buffer);
      GL.Buffers.Set_Color_Clear_Value ((0.0, 0.0, 0.0, 1.0));
      GL.Buffers.Clear ((Color => True, others => False));
      while Char_Position <= Content'Last loop
         UTF8.Read (Content, Char_Position, Code_Point);
         Map_Position := Character_Data (Object, Code_Point);
         declare
            Char_Data : constant Loaded_Character :=
              Loaded_Characters.Element (Map_Position);
         begin
            GL.Uniforms.Set_Single
              (Object.Data.Program.Info_Id,
               GL.Types.Single (X_Offset + Char_Data.Left),
               GL.Types.Single (Char_Data.Y_Min - Y_Min),
               GL.Types.Single (Char_Data.Width),
               GL.Types.Single (Char_Data.Y_Max - Char_Data.Y_Min));
            Tx.Targets.Texture_2D.Bind
              (Loaded_Characters.Element (Map_Position).Image);
            Va.Draw_Arrays (GL.Types.Triangle_Strip, 0, 4);
            X_Offset :=
              X_Offset + Loaded_Characters.Element (Map_Position).Advance;
         end;
      end loop;
      GL.Flush;
      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Window.Set_Viewport (Old_X, Old_Y, Old_Width, Old_Height);
      Fb.Draw_Target.Bind (Fb.Default_Framebuffer);

      return Target;
   end To_Texture;

   procedure Adjust (Object : in out Renderer_Reference) is
   begin
      if Object.Data /= null then
         Object.Data.Refcount := Object.Data.Refcount + 1;
      end if;
   end Adjust;

   procedure Finalize (Object : in out Renderer_Reference) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Renderer_Data, Pointer);
   begin
      if Object.Data /= null then
         Object.Data.Refcount := Object.Data.Refcount - 1;
         if Object.Data.Refcount = 0 then
            Free (Object.Data);
         end if;
      end if;
   end Finalize;

   function Hash (Value : FT.ULong) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type'Mod (Value);
   end Hash;
end GL.Text;
