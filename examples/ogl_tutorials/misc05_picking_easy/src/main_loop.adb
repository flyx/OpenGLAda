
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
   Element_Buffer               : GL.Objects.Buffers.Buffer;
   Normals_Buffer            : GL.Objects.Buffers.Buffer;
   UVs_Buffer               : GL.Objects.Buffers.Buffer;
   Vertex_Buffer            : GL.Objects.Buffers.Buffer;
   Picking_Program          : GL.Objects.Programs.Program;
   Render_Program           : GL.Objects.Programs.Program;
   MVP_Matrix_ID            : GL.Uniforms.Uniform;
   Model_Matrix_ID          : GL.Uniforms.Uniform;
   View_Matrix_ID           : GL.Uniforms.Uniform;
   Picking_Matrix_ID        : GL.Uniforms.Uniform;
   Texture_ID               : GL.Uniforms.Uniform;
   Sample_Texture           : GL.Objects.Textures.Texture;
   MVP_Matrix               : GL.Types.Singles.Matrix4;

   --  ------------------------------------------------------------------------

   procedure Setup_Matrices (Window : in out Glfw.Windows.Window;
                             Render_Program : GL.Objects.Programs.Program;
                             Picking_Program : GL.Objects.Programs.Program);

   --  ------------------------------------------------------------------------

   procedure Render (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      use GL.Types;
   begin
      Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);

      Setup_Matrices (Window, Render_Program, Picking_Program);
      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);

      GL.Objects.Textures.Set_Active_Unit (0);
      GL.Objects.Textures.Targets.Texture_2D.Bind (Sample_Texture);
      GL.Uniforms.Set_Int (Texture_ID, 0);

      --  First attribute buffer : vertices
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

      --  Second attribute buffer : UVs
      GL.Attributes.Enable_Vertex_Attrib_Array (1);
      Array_Buffer.Bind (UVs_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 0, 0);

      GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangles,
                                            First => 0,
                                            Count => 12 * 3);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);
   exception
      when others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup_Matrices (Window : in out Glfw.Windows.Window;
                             Render_Program  : GL.Objects.Programs.Program;
                             Picking_Program : GL.Objects.Programs.Program) is
      use GL.Types;
      use GL.Types.Singles;
      Model_Matrix      : constant Matrix4 := Singles.Identity4;
      Projection_Matrix : Matrix4;
      View_Matrix       : Matrix4;
   begin
      MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "MVP");
      Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "M");
      View_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "V");
      Picking_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Picking_Program, "MVP");

      Controls.Compute_Matrices_From_Inputs (Window, Projection_Matrix, View_Matrix);

      MVP_Matrix :=  Projection_Matrix * View_Matrix * Model_Matrix;
   exception
      when others =>
         Put_Line ("An exception occurred in Setup_Matrices.");
         raise;
   end Setup_Matrices;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window) is
      use GL.Types.Singles;
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      use Glfw.Input;
      Window_Width    : constant Glfw.Size := 1024;
      Window_Height   : constant Glfw.Size := 768;
      Vertex_Count    : GL.Types.Int;
      UV_Count        : GL.Types.Int;
      Normal_Count    : GL.Types.Int;
   begin
      Window.Set_Input_Toggle (Sticky_Keys, True);
      Window.Set_Cursor_Mode (Mouse.Disabled);
      Glfw.Input.Poll_Events;

      Window'Access.Set_Size (Window_Width, Window_Height);
      Window'Access.Set_Cursor_Pos (Mouse.Coordinate (0.5 * Single (Window_Width)),
                                    Mouse.Coordinate (0.5 * Single (Window_Height)));
      Utilities.Clear_Background_Colour (Dark_Blue);

      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.Less);
      GL.Toggles.Enable (GL.Toggles.Cull_Face);

      Vertices_Array_Object.Initialize_Id;
      Vertices_Array_Object.Bind;

      Render_Program := Program_Loader.Program_From
        ((Program_Loader.Src ("src/shaders/standard_vertex_shader.glsl",
         Vertex_Shader),
         Program_Loader.Src ("src/shaders/standard_fragment_shader.glsl",
           Fragment_Shader)));
      Utilities.Show_Shader_Program_Data (Render_Program);

      Picking_Program := Program_Loader.Program_From
        ((Program_Loader.Src ("src/shaders/picking_vertex_shader.glsl",
         Vertex_Shader),
         Program_Loader.Src ("src/shaders/picking_fragment_shader.glsl",
           Fragment_Shader)));
      Utilities.Show_Shader_Program_Data (Picking_Program);

      Setup_Matrices (Window, Render_Program, Picking_Program);

      Load_DDS ("src/textures/uvmap.DDS", Sample_Texture);
      Texture_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "myTextureSampler");

      Load_Object_File.Get_Array_Sizes ("src/textures/suzanne.obj", Vertex_Count, UV_Count, Normal_Count);
      declare
         Vertices         : Singles.Vector3_Array (1 .. Vertex_Count);
         UVs              : Singles.Vector2_Array (1 .. UV_Count);
         Normals          : Singles.Vector3_Array (1 .. Normal_Count);
         Indexed_Vertices : Singles.Vector3_Array (1 .. Vertex_Count);
         Indexed_UVs      : Singles.Vector2_Array (1 .. UV_Count);
         Indexed_Normals  : Singles.Vector3_Array (1 .. Normal_Count);
         Temp_Indices     : Int_Array (1 .. Vertex_Count);
         Indices_Size     : Int;
         Vertices_Size    : Int;
      begin
         Load_Object_File.Load_Object ("src/textures/suzanne.obj", Vertices, UVs, Normals);

         VBO_Indexer.Index_VBO (Vertices, UVs,  Normals,
                                Indexed_Vertices, Indexed_UVs, Indexed_Normals,
                                Temp_Indices, Indices_Size, Vertices_Size);
         declare
            Vertices_Indexed : constant Singles.Vector3_Array (1 .. Vertices_Size)
                 := Indexed_Vertices  (1 .. Vertices_Size);
            UVs_Indexed     : constant Singles.Vector2_Array (1 .. Vertices_Size)
                 := Indexed_UVs  (1 .. Vertices_Size);
            Normals_Indexed  : constant Singles.Vector3_Array (1 .. Vertices_Size)
                 := Indexed_Normals  (1 .. Vertices_Size);
            Indices          : constant GL.Types.Int_Array (1 .. Indices_Size)
                 := Temp_Indices  (1 .. Indices_Size);
         begin
            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices_Indexed, Static_Draw);

            UVs_Buffer.Initialize_Id;
            Array_Buffer.Bind (UVs_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, UVs_Indexed, Static_Draw);

            Normals_Buffer.Initialize_Id;
            Array_Buffer.Bind (Normals_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Normals_Indexed, Static_Draw);

            Element_Buffer.Initialize_Id;
            Array_Buffer.Bind (Element_Buffer);
            Utilities.Load_Element_Buffer (Array_Buffer, Indices, Static_Draw);
         end;
      end;
       Utilities.Enable_Mouse_Callbacks (Window, True);
      Window.Enable_Callback (Glfw.Windows.Callbacks.Char);
      Window.Enable_Callback (Glfw.Windows.Callbacks.Position);
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
      Render (Main_Window);
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
