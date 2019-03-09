
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;

with Controls;
with Program_Loader;
with Load_DDS;
with Load_Object_File;
with Utilities;
with VBO_Indexer;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Dark_Blue                : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 0.0);

   Vertices_Array_Object    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   UVs_Buffer               : GL.Objects.Buffers.Buffer;
   Vertex_Buffer            : GL.Objects.Buffers.Buffer;
   Element_Buffer           : GL.Objects.Buffers.Buffer;
   MVP_Matrix_ID            : GL.Uniforms.Uniform;
   Texture_ID               : GL.Uniforms.Uniform;

   --  ------------------------------------------------------------------------

   Procedure Load_Buffers (Indices_Size : out GL.Types.Int) is
      use GL.Objects.Buffers;
      use GL.Types;
      Vertex_Count          : Int;
      Indexed_Vertices_Size : Int;
   begin
      Vertex_Count := Load_Object_File.Mesh_Size ("src/textures/cube.obj");
      declare
         Vertices         : Singles.Vector3_Array (1 .. Vertex_Count);
         UVs              : Singles.Vector2_Array (1 .. Vertex_Count);
         Indexed_Vertices : Singles.Vector3_Array (1 .. Vertex_Count);
         Indexed_UVs      : Singles.Vector2_Array (1 .. Vertex_Count);
         Temp_Indices     : UInt_Array (1 .. Vertex_Count);

      begin
         Load_Object_File.Load_Object ("src/textures/cube.obj", Vertices, UVs);
         VBO_Indexer.Index_VBO (Vertices, UVs, Indexed_Vertices, Indexed_UVs,
                                Temp_Indices, Indices_Size, Indexed_Vertices_Size);
         declare
            Vertices_Indexed : constant Singles.Vector3_Array (1 .. Indexed_Vertices_Size)
              := Indexed_Vertices  (1 .. Indexed_Vertices_Size);
            UVs_Indexed      : constant Singles.Vector2_Array (1 .. Indexed_Vertices_Size)
              := Indexed_UVs  (1 .. Indexed_Vertices_Size);
            Indices          : constant GL.Types.UInt_Array (1 .. Indices_Size)
              := Temp_Indices  (1 .. Indices_Size);
         begin
            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices_Indexed, Static_Draw);

            UVs_Buffer.Initialize_Id;
            Array_Buffer.Bind (UVs_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, UVs_Indexed, Static_Draw);

            Element_Buffer.Initialize_Id;
            Element_Array_Buffer.Bind (Element_Buffer);
            Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);
         end;
      end;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_Buffers.");
         raise;
   end Load_Buffers;

   --  ------------------------------------------------------------------------

   procedure Render (Window : in out Glfw.Windows.Window;
                     Render_Program : GL.Objects.Programs.Program;
                     Indices_Size   : GL.Types.Int;
                     Sample_Texture : GL.Objects.Textures.Texture) is
      use GL.Objects.Buffers;
      use GL.Types;
      use GL.Types.Singles;
      Model_Matrix      : constant Singles.Matrix4 := GL.Types.Singles.Identity4;
      View_Matrix       : Singles.Matrix4;
      Projection_Matrix : Singles.Matrix4;
      MVP_Matrix        : Singles.Matrix4;
   begin
      Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);
      GL.Objects.Programs.Use_Program (Render_Program);
      Controls.Compute_Matrices_From_Inputs (Window, Projection_Matrix, View_Matrix);
      MVP_Matrix :=  Projection_Matrix * View_Matrix * Model_Matrix;
      GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);

      --  First attribute buffer : vertices
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, False, 0, 0);
      --  Second attribute buffer : UVs
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Objects.Buffers.Array_Buffer.Bind (UVs_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, False, 0, 0);

      --  Bind the texture in Texture Unit 0
      GL.Objects.Textures.Set_Active_Unit (0);
      GL.Objects.Textures.Targets.Texture_2D.Bind (Sample_Texture);
      GL.Uniforms.Set_Int (Texture_ID, 0);

--        Put_Line ("Render, Indices_Size: " & Int'Image (Indices_Size));
--        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 36 * 3);
      GL.Objects.Buffers.Draw_Elements (Triangles, Indices_Size, UInt_Type);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);
   exception
      when others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window;
                    Render_Program : out GL.Objects.Programs.Program;
                    Indices_Size   : out GL.Types.Int;
                    Sample_Texture : out GL.Objects.Textures.Texture) is
      use GL.Objects.Shaders;
      use GL.Types;
      use Glfw.Input;
      Window_Width    : constant Glfw.Size := 1024;
      Window_Height   : constant Glfw.Size := 768;
   begin
      Utilities.Clear_Background_Colour (Dark_Blue);
      Window.Set_Input_Toggle (Sticky_Keys, True);
      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.Less);
            GL.Toggles.Enable (GL.Toggles.Cull_Face);
      Window.Set_Cursor_Mode (Mouse.Disabled);
      Glfw.Input.Poll_Events;

      Window'Access.Set_Size (Window_Width, Window_Height);
      Window'Access.Set_Cursor_Pos (Mouse.Coordinate (0.5 * Single (Window_Width)),
                                    Mouse.Coordinate (0.5 * Single (Window_Height)));

      Vertices_Array_Object.Initialize_Id;
      Vertices_Array_Object.Bind;

      Render_Program := Program_Loader.Program_From
        ((Program_Loader.Src ("src/shaders/Transform_Vertex_Shader.glsl",
         Vertex_Shader),
         Program_Loader.Src ("src/shaders/Texture_Fragment_Shader.glsl",
           Fragment_Shader)));
      Utilities.Show_Shader_Program_Data (Render_Program);

      MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "MVP");

      Load_DDS ("src/textures/uvmap.DDS", Sample_Texture);
      Texture_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "myTextureSampler");

      Load_Buffers (Indices_Size);

   exception
      when others =>
         Put_Line ("An exception occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running         : Boolean := True;
   Render_Program  : GL.Objects.Programs.Program;
   Array_Size      : GL.Types.Int;
   Sample_Texture  : GL.Objects.Textures.Texture;
begin
   Setup (Main_Window, Render_Program, Array_Size, Sample_Texture);
   while Running loop
      Render (Main_Window, Render_Program, Array_Size, Sample_Texture);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and then
          not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and then not Main_Window.Should_Close;
   end loop;

exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
