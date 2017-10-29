
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures.Targets;
with GL.Text;
with GL.Toggles;

with Maths;

package body Text_Management is

   procedure Load_Vertex_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (GL.Types.Singles.Vector2_Pointers);

   Rendering_Program : GL.Text.Shader_Program_Reference;
   Renderer          : GL.Text.Renderer_Reference;
   Vertex_Array      : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer     : GL.Objects.Buffers.Buffer;

   --  ------------------------------------------------------------------------

procedure Load_Data (Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
                     Data_Buffer : GL.Objects.Buffers.Buffer) is
      use GL.Objects.Buffers;

      Square : constant GL.Types.Singles.Vector2_Array
        := ((0.0, 0.0),
            (1.0, 0.0),
            (0.0, 1.0),
            (1.0, 1.0));
   begin
      Vertex_Array.Bind;
      Array_Buffer.Bind (Data_Buffer);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      Load_Vertex_Buffer (Array_Buffer, Square, Static_Draw);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 2, GL.Types.Single_Type, 0, 0);
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
      use Maths;

      Blend_State    : constant GL.Toggles.Toggle_State :=
        GL.Toggles.State (GL.Toggles.Blend);
      Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
        GL.Blending.Blend_Func_Src_Alpha;
      One_Minus_Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
        GL.Blending.One_Minus_Src_Alpha;

      Height         : Single;
      Width          : Pixel_Difference;
      Y_Min, Y_Max   : Pixel_Difference;
      Text_Image     : GL.Objects.Textures.Texture;
      Scaled_Matrix  : Singles.Matrix4;
   begin
      --  Blending allows a fragment colour's alpha value to control the resulting
      --  colour which will be transparent for all the glyph's background colours and
      --  non-transparent for the actual character pixels.
      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                  GL.Blending.One_Minus_Src_Alpha);
      Renderer.Calculate_Dimensions (Text, Width, Y_Min, Y_Max);
--        Width  := Width * Pixel_Difference (Scale);
      Height := Single (Y_Max - Y_Min); --  * Scale;
      Scaled_Matrix := Projection_Matrix *
          Scaling_Matrix ((2.0 / Single (Width), 2.0 / Single (Width), 1.0));

      Text_Image := Renderer.To_Texture (Text, Width, Y_Min, Y_Max, Colour);
      GL.Objects.Textures.Set_Active_Unit (0);
      GL.Objects.Textures.Targets.Texture_2D.Bind (Text_Image);

      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Uniforms.Set_Int (Texture_ID, 0);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Scaled_Matrix);
      GL.Uniforms.Set_Single (Dimensions_ID, Single (Width), Height);
      GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));

      Load_Data (Vertex_Array, Vertex_Buffer);

      Vertex_Array.Bind;
      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangle_Strip, 0, 4);
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
