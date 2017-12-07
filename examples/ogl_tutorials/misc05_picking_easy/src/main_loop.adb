
--  with Interfaces.C;

with Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
--  with GL.Framebuffer;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
--  with GL.Pixels;
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
--  with Maths;
with Utilities;
with VBO_Indexer;

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

--     type Pixels_Array is array (Positive range <>) of aliased GL.Types.UByte;
--     procedure Read_Pix is new
--         GL.Framebuffer.Read_Pixels (Element_Type => GL.Types.UByte,
--                                     Index_Type   => Positive,
--                                     Array_Type   => Pixels_Array);
--     type Orientation is record
--        Angle : Maths.Radian;
--        Axis  : GL.Types.Singles.Vector3;
--     end record;
--     type Orientation_Array is array (GL.Types.Int range <>) of Orientation;

   Dark_Blue                : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 0.0);
--     White                    : constant GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);

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
--     Picking_Colour_ID        : GL.Uniforms.Uniform;
--     Picking_Matrix_ID        : GL.Uniforms.Uniform;
   Texture_ID               : GL.Uniforms.Uniform;
   MVP_Matrix               : GL.Types.Singles.Matrix4;

--     Last_Time                : Glfw.Seconds;
--     Number_Of_Frames         : Integer := 0;
   Max_Items                : constant GL.Types.Int := 100;
--     Orientations             : Orientation_Array (1 .. Max_Items);
   Positions                : GL.Types.Singles.Vector3_Array (1 .. Max_Items);

   --  ------------------------------------------------------------------------

   procedure Load_Texture (Window : in out Glfw.Windows.Window;
                           UV_Map : GL.Objects.Textures.Texture) is
      use GL.Types;
      use GL.Types.Singles;
      Model_Matrix      : constant Matrix4 := Singles.Identity4;
      View_Matrix       : Matrix4;
      Projection_Matrix : Matrix4;
--        Rot_Matrix        : Matrix4;
--        Trans_Matrix      : Matrix4;
--        Light_Pos         : constant Vector3 := (4.0, 4.0, 4.0);
   begin
      Controls.Compute_Matrices_From_Inputs (Window, Projection_Matrix, View_Matrix);
--        for count in GL.Types.Int range 1 .. Max_Items loop
--           Rot_Matrix := Maths.Rotation_Matrix (Orientations (count).Angle,
--                                                Orientations (count).Axis);
--           Trans_Matrix := Maths.Translation_Matrix (Positions (count));
--           Model_Matrix := Trans_Matrix * Rot_Matrix;
         MVP_Matrix :=  Projection_Matrix * View_Matrix * Model_Matrix;
         GL.Uniforms.Set_Single (Model_Matrix_ID, Model_Matrix);
         GL.Uniforms.Set_Single (View_Matrix_ID, View_Matrix);
         GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);
         GL.Uniforms.Set_Single (Light_Position_ID, 4.0, 4.0, 4.0);
      --           GL.Uniforms.Set_Single (Light_Position_ID, Light_Pos);

         --  Bind our texture in Texture Unit 0
         GL.Objects.Textures.Set_Active_Unit (0);
         GL.Objects.Textures.Targets.Texture_2D.Bind (UV_Map);
         GL.Uniforms.Set_Int (Texture_ID, 0);
--        end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_Texture.");
         raise;
   end Load_Texture;

   --  ------------------------------------------------------------------------

--     procedure Pick (Window                         : in out Glfw.Windows.Window;
--                     Picking_Program                : GL.Objects.Programs.Program;
--                     Positions                      : GL.Types.Singles.Vector3_Array;
--                     Orientations                   : Orientation_Array;
--                     View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4) is
--        use Interfaces;
--        use GL.Types;
--        use GL.Types.Singles;
--        Model_Matrix    : Matrix4;
--        Rot_Matrix      : Matrix4;
--        Trans_Matrix    : Matrix4;
--        R               : Single;
--        G               : Single;
--        B               : Single;
--        Window_Width    : Glfw.Size;
--        Window_Height   : Glfw.Size;
--        Pixel_Data      : Pixels_Array (1 .. 4);
--        Picked_ID       : Int;
--        --        Message         : Ada.Strings.Unbounded.Unbounded_String;
--     begin
--        Utilities.Clear_Background_Colour_And_Depth (White);
--        GL.Objects.Programs.Use_Program (Picking_Program);
--        --  Only the positions are needed (not the UVs and normals)
--        GL.Attributes.Enable_Vertex_Attrib_Array (0);
--        for count in GL.Types.Int range 1 .. Max_Items loop
--           Rot_Matrix := Maths.Rotation_Matrix (Orientations (count).Angle,
--                                                Orientations (count).Axis);
--           Trans_Matrix := Maths.Translation_Matrix (Positions (count));
--           Model_Matrix := Trans_Matrix * Rot_Matrix;
--           MVP_Matrix :=  Projection_Matrix * View_Matrix * Model_Matrix;
--           GL.Uniforms.Set_Single (Picking_Matrix_ID, MVP_Matrix);
--
--           --  Convert count, the integer mesh ID, into an RGB color
--           R :=  Single (Unsigned_32 (count) and 16#FF#) / 255.0;
--           G :=  Single (Shift_Right (Unsigned_32 (count) and 16#FF00#, 8)) / 255.0;
--           B :=  Single (Shift_Right (Unsigned_32 (count) and 16#FF0000#, 16)) / 255.0;
--           GL.Uniforms.Set_Single (Picking_Colour_ID, R, G, B, 1.0);
--
--           GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
--           GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
--
--           GL.Objects.Buffers.Element_Array_Buffer.Bind (Element_Buffer);
--           GL.Objects.Buffers.Draw_Elements (Mode       => Triangles,
--                                             Count      => Indices_Size,
--                                             Index_Type => UInt_Type);
--        end loop;
--        GL.Attributes.Disable_Vertex_Attrib_Array (0);
--        GL.Flush;
--
--        GL.Pixels.Set_Pack_Alignment (GL.Pixels.Unpack_Alignment);
--        Window'Access.Get_Size (Window_Width, Window_Height);
--        --  Read the pixel at the center of the screen
--        Read_Pix (Int (Window_Width) / 2, Int (Window_Height) / 2, 1, 1,
--                  GL.Pixels.RGBA, GL.Pixels.Float, Pixel_Data);
--        --   Convert the color back to an integer ID
--        Picked_ID := Int (Pixel_Data (1)) + 256 * Int (Pixel_Data (2)) +
--            256 * 256 * Int (Pixel_Data (3));
--        if Picked_ID = 16#00FFFFFF# then  --  Full white, must be the background!
--           Put_Line ("Background " & Int'Image (Picked_ID));
--           --           Message := Ada.Strings.Unbounded.To_Unbounded_String ("background");
--        else
--           Put_Line ("Mesh " & Int'Image (Picked_ID));
--           --           Message := Ada.Strings.Unbounded.To_Unbounded_String ("");
--        end if;
--
--     exception
--        when others =>
--           Put_Line ("An exception occurred in Pick.");
--           raise;
--     end Pick;

   --  ------------------------------------------------------------------------

   procedure Render (Window : in out Glfw.Windows.Window;
                     Render_Program  : GL.Objects.Programs.Program;
--                       Picking_Program : GL.Objects.Programs.Program;
                     Vertex_Count    : GL.Types.Int;
                     UV_Map          : GL.Objects.Textures.Texture) is
--        use Interfaces.C;
      use GL.Objects.Buffers;
--        use GL.Objects.Shaders;
      use GL.Types;
      use Glfw.Input;
--        View_Matrix       : GL.Types.Singles.Matrix4;
--        Projection_Matrix : GL.Types.Singles.Matrix4;
--        Current_Time      : constant Glfw.Seconds := Glfw.Time;
   begin
      Utilities.Clear_Background_Colour_And_Depth (Dark_Blue);
--        Number_Of_Frames := Number_Of_Frames + 1;
--        if Current_Time - Last_Time >= 1.0 then
--           Put_Line (Integer'Image (1000 * Number_Of_Frames) & " ms/frame");
--           Number_Of_Frames := 0;
--           Last_Time := Last_Time + 1.0;
--        end if;

      GL.Objects.Programs.Use_Program (Render_Program);
      Load_Texture (Window, UV_Map);

--        if Window.Mouse_Button_State (Mouse.Left_Button) = Glfw.Input.Pressed then
--           Pick (Window, Picking_Program, Positions, Orientations,
--                 View_Matrix, Projection_Matrix);
--        end if;

      --  First attribute buffer : vertices
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Objects.Buffers.Array_Buffer.Bind (Vertex_Buffer);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
      --  Index Buffer
--        GL.Attributes.Enable_Vertex_Attrib_Array (1);
      GL.Objects.Buffers.Element_Array_Buffer.Bind (Element_Buffer);
--        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 0, 0);

      GL.Objects.Buffers.Draw_Elements (Triangles, Vertex_Count, UInt_Type, 0);

      GL.Attributes.Disable_Vertex_Attrib_Array (0);
      GL.Attributes.Disable_Vertex_Attrib_Array (1);
      GL.Attributes.Disable_Vertex_Attrib_Array (2);
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
      UVs_Size        : Int;
      Normals_Size    : Int;
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

--        Picking_Program := Program_Loader.Program_From
--            ((Program_Loader.Src ("src/shaders/picking_vertex_shader.glsl",
--             Vertex_Shader),
--             Program_Loader.Src ("src/shaders/picking_fragment_shader.glsl",
--                 Fragment_Shader)));
--        Utilities.Show_Shader_Program_Data (Picking_Program);

      MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "MVP");
      Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "M");
      View_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "V");
      Light_Position_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "Light_Position_Worldspace");
--        Picking_Matrix_ID := GL.Objects.Programs.Uniform_Location
--          (Picking_Program, "MVP");
      Load_DDS ("src/textures/uvmap.DDS", UV_Map);
      Texture_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "myTextureSampler");

      Vertex_Count := Load_Object_File.Mesh_Size ("src/textures/suzanne.obj");
      declare
         use Ada.Numerics.Float_Random;
         subtype Random_Single is Single range 10.0 .. 20.0;
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
            return Random_Single (10.0 + 10.0 * Random (Random_Gen));
         end New_Value;

      begin
         Ada.Numerics.Float_Random.Reset (Random_Gen);
         for index in Positions'Range loop
            Positions (index) := (New_Value, New_Value, New_Value);
--              Orientations (index) := (Maths.Radian (New_Value),
--                                       (New_Value, New_Value, New_Value));
         end loop;

         Load_Object_File.Load_Object ("src/textures/suzanne.obj", Vertices, UVs, Normals);
         VBO_Indexer.Index_VBO (Vertices, UVs,  Normals,
                                Indexed_Vertices, Indexed_UVs, Indexed_Normals,
                                Temp_Indices, Indices_Size, Vertices_Size,
                                UVs_Size, Normals_Size);
         declare
            Vertices_Indexed : constant Singles.Vector3_Array (1 .. Vertices_Size)
                := Indexed_Vertices  (1 .. Vertices_Size);
            UVs_Indexed      : constant Singles.Vector2_Array (1 .. UVs_Size)
                := Indexed_UVs  (1 .. UVs_Size);
            Normals_Indexed  : constant Singles.Vector3_Array (1 .. Normals_Size)
                := Indexed_Normals  (1 .. Normals_Size);
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

--        Picking_Colour_ID := GL.Objects.Programs.Uniform_Location
--            (Picking_Program, "Picking_Colour");
--        Light_Position_ID := GL.Objects.Programs.Uniform_Location
--            (Picking_Program, "Light_Position_Worldspace");

--        Last_Time := Glfw.Time;
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
