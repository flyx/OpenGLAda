
with Interfaces.C;

with Ada.Numerics.Float_Random;
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
with Maths;
with Utilities;
with VBO_Indexer;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Dark_Blue                : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 0.0);
   White                    : constant GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);

   Vertices_Array_Object    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Element_Buffer           : GL.Objects.Buffers.Buffer;
   Indices_Size             : GL.Types.Int;
   Normals_Buffer           : GL.Objects.Buffers.Buffer;
   UVs_Buffer               : GL.Objects.Buffers.Buffer;
   Vertex_Buffer            : GL.Objects.Buffers.Buffer;
   MVP_Matrix_ID            : GL.Uniforms.Uniform;
   Model_Matrix_ID          : GL.Uniforms.Uniform;
   View_Matrix_ID           : GL.Uniforms.Uniform;
   Light_Position_ID        : GL.Uniforms.Uniform;
   Texture_ID               : GL.Uniforms.Uniform;
   MVP_Matrix               : GL.Types.Singles.Matrix4;

   Last_Time                : Glfw.Seconds;
   Number_Of_Frames         : Integer := 0;
   Max_Items                : constant GL.Types.Int := 100;
   Positions                : GL.Types.Singles.Vector3_Array (1 .. Max_Items);

   --  ------------------------------------------------------------------------

   procedure Load_Texture (Window       : in out Glfw.Windows.Window;
                           Vertex_Count : GL.Types.Int;
                           UV_Map       : GL.Objects.Textures.Texture) is
      use GL.Types;
      use GL.Types.Singles;
      Model_Matrix      : Matrix4;
      View_Matrix       : Matrix4;
      Projection_Matrix : Matrix4;
      Trans_Matrix      : Matrix4;
      --        Light_Pos         : constant Vector3 := (4.0, 4.0, 4.0);
      Scale             : constant Singles.Vector3 := (1.0, 1.0, 1.0);
   begin
      Controls.Compute_Matrices_From_Inputs (Window, Projection_Matrix, View_Matrix);
      Utilities.Clear_Background_Colour_And_Depth (White);
      for count in GL.Types.Int range 1 .. Max_Items loop
         Trans_Matrix := Maths.Translation_Matrix (Positions (count) * 2.0 * Scale (GL.X));
         Model_Matrix := Trans_Matrix * Maths.Scaling_Matrix (Scale);
         MVP_Matrix :=  Projection_Matrix * View_Matrix * Model_Matrix;
         GL.Uniforms.Set_Single (Model_Matrix_ID, Model_Matrix);
         GL.Uniforms.Set_Single (View_Matrix_ID, View_Matrix);
         GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);
         GL.Uniforms.Set_Single (Light_Position_ID, 4.0, 4.0, 4.0);
         --           GL.Uniforms.Set_Single (Light_Position_ID, Light_Pos);

         --  Bind the texture in Texture Unit 0
         GL.Objects.Textures.Set_Active_Unit (0);
         GL.Objects.Textures.Targets.Texture_2D.Bind (UV_Map);
         GL.Uniforms.Set_Int (Texture_ID, 0);

         --  First attribute buffer : vertices
         GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
         --  Index Buffer
         GL.Objects.Buffers.Element_Array_Buffer.Bind (Element_Buffer);
         GL.Objects.Buffers.Draw_Elements (Triangles, Vertex_Count, UInt_Type, 0);
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_Texture.");
         raise;
   end Load_Texture;

   --  ------------------------------------------------------------------------

   procedure Render (Window : in out Glfw.Windows.Window;
                     Render_Program  : GL.Objects.Programs.Program;
                     --                       Picking_Program : GL.Objects.Programs.Program;
                     Vertex_Count    : GL.Types.Int;
                     UV_Map          : GL.Objects.Textures.Texture) is
      use Interfaces.C;
      use GL.Objects.Buffers;
      use GL.Types;
      use Glfw.Input;
      Current_Time      : constant Glfw.Seconds := Glfw.Time;
   begin
      Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);
      Number_Of_Frames := Number_Of_Frames + 1;
      if Current_Time - Last_Time >= 1.0 then
         Put_Line (Integer'Image (1000 * Number_Of_Frames) & " ms/frame");
         Number_Of_Frames := 0;
         Last_Time := Last_Time + 1.0;
      end if;

      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      Load_Texture (Window, Vertex_Count, UV_Map);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
   exception
      when others =>
         Put_Line ("An exception occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup (Window : in out Glfw.Windows.Window;
                    Render_Program  : out GL.Objects.Programs.Program;
                    --                      Picking_Program : out GL.Objects.Programs.Program;
                    Vertex_Count    : out GL.Types.Int;
                    UV_Map          : out GL.Objects.Textures.Texture) is
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      use GL.Types.Singles;
      use Glfw.Input;
      Window_Width    : constant Glfw.Size := 1024;
      Window_Height   : constant Glfw.Size := 768;
      Vertices_Size   : Int;
   begin
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

      GL.Toggles.Enable (GL.Toggles.Depth_Test);
      GL.Buffers.Set_Depth_Function (GL.Types.Less);
      GL.Toggles.Enable (GL.Toggles.Cull_Face);

      Render_Program := Program_Loader.Program_From
        ((Program_Loader.Src ("src/shaders/standard_vertex_shader.glsl",
         Vertex_Shader),
         Program_Loader.Src ("src/shaders/standard_fragment_shader.glsl",
           Fragment_Shader)));
      Utilities.Show_Shader_Program_Data (Render_Program);

      MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "MVP");
      Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "M");
      View_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "V");
      Light_Position_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "Light_Position_Worldspace");
      Load_DDS ("src/textures/uvmap.DDS", UV_Map);
      Texture_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "myTextureSampler");

      Vertex_Count := Load_Object_File.Mesh_Size ("src/textures/suzanne.obj");
      declare
         use Ada.Numerics.Float_Random;
         subtype Random_Single is Single range -10.0 .. 10.0;
         Vertices         : Singles.Vector3_Array (1 .. Vertex_Count);
         UVs              : Singles.Vector2_Array (1 .. Vertex_Count);
         Normals          : Singles.Vector3_Array (1 .. Vertex_Count);
         Indexed_Vertices : Singles.Vector3_Array (1 .. Vertex_Count);
         Indexed_UVs      : Singles.Vector2_Array (1 .. Vertex_Count);
         Indexed_Normals  : Singles.Vector3_Array (1 .. Vertex_Count);
         Temp_Indices     : Int_Array (1 .. 3 * Vertex_Count);
         Random_Gen       : Ada.Numerics.Float_Random.Generator;

         function New_Value return Random_Single is
         begin
            return Random_Single (20.0 * Random (Random_Gen) - 10.0);
         end New_Value;

      begin
         Ada.Numerics.Float_Random.Reset (Random_Gen);
         for index in Positions'Range loop
            Positions (index) := (New_Value, New_Value, New_Value);
         end loop;

         Load_Object_File.Load_Object ("src/textures/suzanne.obj", Vertices, UVs, Normals);
         VBO_Indexer.Index_VBO (Vertices, UVs,  Normals,
                                Indexed_Vertices, Indexed_UVs, Indexed_Normals,
                                Temp_Indices, Indices_Size, Vertices_Size);
         declare
            Vertices_Indexed : constant Singles.Vector3_Array (1 .. Vertices_Size)
                := Indexed_Vertices  (1 .. Vertices_Size);
            UVs_Indexed      : constant Singles.Vector2_Array (1 .. Vertices_Size)
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
            Element_Array_Buffer.Bind (Element_Buffer);
            Utilities.Load_Element_Buffer (Element_Array_Buffer, Indices, Static_Draw);
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
   --     Picking_Program : GL.Objects.Programs.Program;
   Render_Program  : GL.Objects.Programs.Program;
   Running         : Boolean := True;
   Vertex_Count    : GL.Types.Int;
   UV_Map          : GL.Objects.Textures.Texture;
begin
   Setup (Main_Window, Render_Program, Vertex_Count, UV_Map);
   --     Setup (Main_Window, Render_Program, Picking_Program, Vertex_Count, UV_Map);
   while Running loop
      Render (Main_Window, Render_Program, Vertex_Count, UV_Map);
      --        Render (Main_Window, Picking_Program, Vertex_Count, UV_Map);
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