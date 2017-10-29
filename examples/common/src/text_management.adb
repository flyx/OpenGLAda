
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures.Targets;
with GL.Text;
with GL.Toggles;

package body Text_Management is

   procedure Load_Vertex_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (GL.Types.Singles.Vector4_Pointers);

   Rendering_Program : GL.Text.Shader_Program_Reference;
   Renderer          : GL.Text.Renderer_Reference;
   Vertex_Array      : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer     : GL.Objects.Buffers.Buffer;

   --  ------------------------------------------------------------------------

procedure Load_Data (VAO  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
                     Data_Buffer : GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;

      Square : constant GL.Types.Singles.Vector4_Array
        := ((0.0, 0.0, 0.0, 0.0),
            (1.0, 0.0, 1.0, 0.0),
            (0.0, 1.0, 0.0, 1.0),
            (1.0, 1.0, 1.0, 1.0));
      T_Square : constant GL.Types.Singles.Vector4_Array
        := ((0.0, 0.0, 0.0, 0.0),
            (1.0, 0.0, 1.0, 0.0),
            (1.0, 1.0, 1.0, 1.0),

            (1.0, 1.0, 1.0, 1.0),
            (0.0, 1.0, 0.0, 1.0),
            (0.0, 0.0, 0.0, 0.0));
   begin
      VAO.Bind;
      Array_Buffer.Bind (Data_Buffer);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      Load_Vertex_Buffer (Array_Buffer, Square, Static_Draw);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 2, GL.Types.Single_Type, 4, 0);
--        GL.Attributes.Set_Vertex_Attrib_Pointer (Index => 0, Count => 2,
--                 Kind => GL.Types.Single_Type, Stride => 4, Offset => 0);
exception
   when others =>
      Put_Line ("An exception occurred in Texture_Management.Load_Data.");
      raise;
   end Load_Data;

   --  ------------------------------------------------------------------------

   procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                          Text   : String; X, Y, Scale : GL.Types.Single;
                          Colour : GL.Types.Colors.Color;
                          Texture_ID, Projection_Matrix_ID, Dimensions_ID,
                          Colour_ID : GL.Uniforms.Uniform;
                          Projection_Matrix : GL.Types.Singles.Matrix4) is
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      use GL.Text;
      use GL.Types.Colors;
      use GL.Types;

      Num_Triangles  : constant GL.Types.Int := 2;
      Num_Vertices   : constant GL.Types.Int := Num_Triangles * 3; -- Two triangles
      Num_Components : constant GL.Types.Int := 4;                 -- Coords vector size;
      Stride         : constant GL.Types.Int := 0;
      Blend_State    : constant GL.Toggles.Toggle_State :=
        GL.Toggles.State (GL.Toggles.Blend);
      Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
        GL.Blending.Blend_Func_Src_Alpha;
      One_Minus_Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
        GL.Blending.One_Minus_Src_Alpha;
      Height         : Single;
      Width          : Pixel_Difference;
      Y_Min, Y_Max   : Pixel_Difference;
      --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
--        Vertex_Data    : Singles.Vector4_Array (1 .. Num_Vertices);
      Text_Image     : GL.Objects.Textures.Texture;
   begin
      --  Blending allows a fragment colour's alpha value to control the resulting
      --  colour which will be transparent for all the glyph's background colours and
      --  non-transparent for the actual character pixels.
      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                  GL.Blending.One_Minus_Src_Alpha);
      GL.Objects.Programs.Use_Program (Render_Program);

      Renderer.Calculate_Dimensions (Text, Width, Y_Min, Y_Max);
      Height := Single (Y_Max - Y_Min) * Scale;

      GL.Uniforms.Set_Int (Texture_ID, 0);
      GL.Uniforms.Set_Single (Dimensions_ID, Single (Width), Height);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);
      GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));

      Load_Data (Vertex_Array, Vertex_Buffer);
      Text_Image := Renderer.To_Texture (Text, Width, Y_Min, Y_Max, Colour);
      GL.Objects.Textures.Set_Active_Unit (0);
      GL.Objects.Textures.Targets.Texture_2D.Bind (Text_Image);
--        Width  := Width * Pixel_Difference (Scale);

--        Vertex_Data := ((X, Y + Height,                   0.0, 0.0),
--                        (X, Y + Single (Y_Min),           0.0, 1.0),
--                        (X + Single (Width), Y + Single (Y_Min),   1.0, 1.0),
--
--                        (X, Y + Height,          0.0, 0.0),
--                        (X + Single (Width), Y + Single (Y_Min),  1.0, 1.0),
--                        (X + Single (Width), Y + Height,          1.0, 0.0));

      Vertex_Array.Bind;
      Array_Buffer.Bind (Vertex_Buffer);
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangle_Strip, 0, 4);
--        GL.Objects.Vertex_Arrays.Null_Array_Object.Bind;
    GL.Attributes.Disable_Vertex_Attrib_Array (0);

      GL.Toggles.Set (GL.Toggles.Blend, Blend_State);
      GL.Blending.Set_Blend_Func (Src_Alpha_Blend, One_Minus_Src_Alpha_Blend);
exception
   when others =>
      Put_Line ("An exception occurred in Texture_Management.Render_Text.");
      raise;
   end Render_Text;

   --  ------------------------------------------------------------------------

   procedure Setup (Font_File : String) is
   begin
      Vertex_Array.Initialize_Id;
      Vertex_Buffer.Initialize_Id;
      GL.Text.Create (Rendering_Program);
      Renderer.Create (Rendering_Program, Font_File, 0, 96);
   end Setup;

   --  ------------------------------------------------------------------------

end Text_Management;
