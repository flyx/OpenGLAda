
with Ada.Numerics.Float_Random;

with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C.Pointers;

with GL.Attributes;
with GL.Buffers;
with GL.Objects;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with Load_VB_Object;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   type PV_Buffer is record
      Position : Singles.Vector4;
      Velocity : Singles.Vector3;
   end record;

   type PV_Buffer_Array is array (UInt range <>) of aliased PV_Buffer;

   package PV_Buffer_Package is new Interfaces.C.Pointers
     (UInt, PV_Buffer, PV_Buffer_Array, PV_Buffer'(others => <>));
   procedure Map_PV_Buffer is new
     GL.Objects.Buffers.Map (PV_Buffer_Package);

   Vec3_Size                   : constant UInt := GL.Types.Singles.Vector3'Size / 8;
   Vec4_Size                   : constant UInt := GL.Types.Singles.Vector4'Size / 8;
   PV_Buffer_Size              : constant UInt := Vec4_Size + Vec3_Size;
   Background                  : constant GL.Types.Colors.Color := (0.0, 1.0, 0.0, 0.0);

   Update_Program              : GL.Objects.Programs.Program;
   VAO                         : array (1 .. 2) of GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   VBO                         : array (1 .. 2) of GL.Objects.Buffers.Buffer;

   Render_Program              : GL.Objects.Programs.Program;
   Geometry_VBO                : GL.Objects.Buffers.Buffer;
   Render_VAO                  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Render_Model_Matrix_ID      : GL.Uniforms.Uniform;
   Render_Projection_Matrix_ID : GL.Uniforms.Uniform;

   Geometry_Texture            : GL.Objects.Buffers.Buffer;

   Model_Matrix_ID             : GL.Uniforms.Uniform;
   Projection_Matrix_ID        : GL.Uniforms.Uniform;
   Triangle_Count_ID           : GL.Uniforms.Uniform;
   Time_Step_ID                : GL.Uniforms.Uniform;

   VBM_Object                  : Load_VB_Object.VB_Object;

   Random_Gen                  : Ada.Numerics.Float_Random.Generator;
   Num_Points                  : constant UInt := 5000;  --  Point_Count

   --  Display variables
   Frame_Count                 : UInt := 1;
   Last_Time                   : Float := 0.0;  --  q

   --  ------------------------------------------------------------------------

   function Random_Vector (Min_Mag : Float := 0.0; Max_Mag : Float := 1.0)
                           return GL.Types.Singles.Vector3 is
      use Ada.Numerics.Float_Random;
      use GL.Types.Singles;
      RV  : Vector3;
   begin
      RV := (2.0 * Single (Random (Random_Gen)) - 1.0,
             2.0 * Single (Random (Random_Gen)) - 1.0,
             2.0 * Single (Random (Random_Gen)) - 1.0);
      RV :=  Single (Min_Mag + Random (Random_Gen) * (Max_Mag - Min_Mag)) * RV;
      return Maths.Normalized (RV);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Random_Vector.");
         raise;
   end Random_Vector;

   --  ------------------------------------------------------------------------

   procedure Display (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      use GL.Types.Singles;
      use Maths;
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
      Aspect            : Single;
      Model_Matrix      : Singles.Matrix4 := Identity4;
      Projection_Matrix : Singles.Matrix4;
      Scale             : constant Single := 0.003;
      Current_Time      : Float;  --  t
   begin
      Current_Time :=  Float (Glfw.Time);
      Utilities.Clear_Background_Colour_And_Depth (Background);

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Aspect := Single (Window_Height) / Single (Window_Width);

      Init_Orthographic_Transform (-1.0, 1.0, -Aspect, Aspect, -1.0, 5000.0,
                                   Projection_Matrix);
      Projection_Matrix := Translation_Matrix ((0.0, 0.0, 1.99)) *
        Projection_Matrix;

      Model_Matrix :=  Scaling_Matrix (Scale) *
        Rotation_Matrix (Degree (360.0 * Current_Time), (0.0, 1.0, 0.0)) *
          Rotation_Matrix (Degree (360.0 * 3.0 * Current_Time), (0.0, 0.0, 1.0)) *
          Model_Matrix;

      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Buffers.Set_Depth_Function (LEqual);

      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Uniforms.Set_Single (Render_Model_Matrix_ID, Model_Matrix);
      GL.Uniforms.Set_Single (Render_Projection_Matrix_ID, Projection_Matrix);

      Render_VAO.Bind;
      Transform_Feedback_Buffer.Bind_Buffer_Base (0, Geometry_VBO);

      GL.Objects.Programs.Begin_Transform_Feedback (Triangles);
      Load_VB_Object.Render (VBM_Object);
      GL.Objects.Programs.End_Transform_Feedback;

      GL.Objects.Programs.Use_Program (Update_Program);

      Model_Matrix := Scaling_Matrix (Scale) * Identity4;
      GL.Uniforms.Set_Single (Model_Matrix_ID, Model_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);
      GL.Uniforms.Set_Int (Triangle_Count_ID,
                           Load_VB_Object.Get_Vertex_Count (VBM_Object) / 3);

      if Current_Time > Last_Time then    --  t > q
         GL.Uniforms.Set_Single (Time_Step_ID,
                                 2000.0 * Single (Current_Time - Last_Time));
      end if;
      Last_Time := Current_Time;        --  q = t

      if (Frame_Count rem 2) = 0 then   --  (frame_count & 1) != 0
         VAO (2).Bind;
         Transform_Feedback_Buffer.Bind_Buffer_Base (0, VBO (1));
      else
         VAO (1).Bind;
         Transform_Feedback_Buffer.Bind_Buffer_Base (0, VBO (2));
      end if;

      GL.Objects.Programs.Begin_Transform_Feedback (Points);
      GL.Objects.Vertex_Arrays.Draw_Arrays
        (Mode  => GL.Types.Points, First => 0,
         Count => GL.Types.Size (UInt'Min (Num_Points, Frame_Count)));
      GL.Objects.Programs.End_Transform_Feedback;

      GL.Objects.Vertex_Arrays.Bind (GL.Objects.Vertex_Arrays.Null_Array_Object);

      if Frame_Count > 5000 then
         Frame_Count := 0;
      else
         Frame_Count := Frame_Count + 1;
      end if;

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------
   --  Load_PV_Buffer loads Transform_Feedback_Buffer with data points
   procedure Load_PV_Buffer is
      use GL.Objects.Buffers;
      use GL.Types.Singles;
      Buffer_Pointer : PV_Buffer_Package.Pointer;  --  to PV_Buffer_Array
      Velocity       : Vector3;
   begin
      Map_PV_Buffer (Transform_Feedback_Buffer, GL.Objects.Write_Only, Buffer_Pointer);
      for B_Index in 1 .. Num_Points loop   --  j
         Velocity := Random_Vector;
         Buffer_Pointer.Position := To_Vector4 (Velocity) + (-0.5, 40.0, 0.0, 0.0);
         Buffer_Pointer.Position := To_Vector4 (0.1 * Velocity) + (-0.5, 0.2, 0.0, 0.0);
         Buffer_Pointer.Velocity := (Velocity (GL.X), 0.3 * Velocity (GL.Y),
                         0.3 * Velocity (GL.Z));
         PV_Buffer_Package.Increment (Buffer_Pointer);
      end loop;
      Unmap (Transform_Feedback_Buffer);
   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Load_PV_Buffer.");
         raise;
   end Load_PV_Buffer;

   --  ------------------------------------------------------------------------

   function Setup return Boolean is
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
      VBM_Result     : Boolean := False;
      Varyings       : constant String := "position1,velocity1";
      Varyings_2     : constant String := "world_space_position1";
      Name           : String (1 .. 30);
      Name_Length    : GL.Types.Size := 99;
      V_Length       : GL.Types.Size := 99;
      Max_Length     : Int;
      V_Type         : Buffer_Mode;
   begin
      VAO (1).Initialize_Id;
      VAO (1).Bind;
      VAO (2).Initialize_Id;
      VAO (2).Bind;
      Render_VAO.Initialize_Id;
      GL.Objects.Vertex_Arrays.Bind (Render_VAO);

      for index in VBO'Range loop
         VBO (index).Initialize_Id;
         Transform_Feedback_Buffer.Bind (VBO (index));
      end loop;

      --  Program_From includes linking
      Update_Program := Program_From
        ((Src ("src/shaders/update_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/update_fragment_shader.glsl", Fragment_Shader)));

      if not GL.Objects.Programs.Link_Status (Update_Program) then
         Put_Line ("Setup, Update_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Update_Program));
      else
         Put_Line ("Setup, Update_Program Link ok");
      end if;

      GL.Objects.Programs.Use_Program  (Update_Program);
      Transform_Feedback_Varyings (Update_Program, Varyings, Interleaved_Attribs);
      Update_Program.Link;

      if not GL.Objects.Programs.Link_Status (Update_Program) then
         Put_Line ("Setup, Update_Program Transform_Feedback_Varyings Link failed.");
         Put_Line (GL.Objects.Programs.Info_Log (Update_Program));
      else
         Put_Line ("Setup, Update_Program Transform_Feedback_Varyings Link ok");
      end if;

      V_Type := Transform_Feedback_Buffer_Mode (Update_Program);
      V_Length := Transform_Feedback_Varyings_Size (Update_Program);
      Max_Length := Transform_Feedback_Varying_Max_Length (Update_Program);
      Put_Line ("V_Type: " & Buffer_Mode'Image (V_Type) & "   V_Length: " &
                  Int'Image (V_Length) & "   Max Length: " & Int'Image (Max_Length));

      Get_Transform_Feedback_Varying (Object   => Update_Program,
                                      Index    => 0,
                                      Length   => Name_Length,
                                      V_Length => V_Length,
                                      V_Type   => V_Type,
                                      Name     => Name);

      Put_Line ("Name: " & Name);
      Put_Line ("Length: " & Int'Image (Name_Length));
      Put_Line ("V_Length" &  Int'Image (V_Length));
      if V_Type = Interleaved_Attribs or V_Type = Separate_Attribs then
         Put_Line ("V_Type: "  & Buffer_Mode'Image (V_Type));
      else
         Put_Line ("Setup, Get_Transform_Feedback_Varying returned an invalid Buffer_Mode value: "
                  & Integer'Image (V_Type'Enum_Rep));
      end if;

      Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Update_Program, "model_matrix");
      Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Update_Program, "projection_matrix");
      Triangle_Count_ID := GL.Objects.Programs.Uniform_Location
        (Update_Program, "triangle_count");
      Time_Step_ID := GL.Objects.Programs.Uniform_Location
        (Update_Program, "time_step");
      Put_Line (GL.Objects.Programs.Info_Log (Update_Program));

      Render_Program := Program_From
        ((Src ("src/shaders/render_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/render_fragment_shader.glsl", Fragment_Shader)));

      Transform_Feedback_Varyings (Render_Program, Varyings_2, Interleaved_Attribs);
      Render_Program.Link;
      if not GL.Objects.Programs.Link_Status (Render_Program) then
         Put_Line ("Setup, Render_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Render_Program));
      else
         Put_Line ("Setup, Render_Program Link OK");
      end if;

      GL.Objects.Programs.Use_Program  (Render_Program);
      Render_Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "model_matrix");
      Render_Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "projection_matrix");

      for index in VBO'Range loop
         Transform_Feedback_Buffer.Bind (VBO (index));
         Transform_Feedback_Buffer.Allocate
           (Long (Num_Points * PV_Buffer_Size), Dynamic_Copy);
         if index = VBO'First then
            Load_PV_Buffer;
         end if;

         VAO (index).Bind;
         Array_Buffer.Bind (VBO (index));

         GL.Attributes.Set_Vertex_Attrib_Pointer
           (0, 4, Single_Type, True, Int (PV_Buffer_Size), 0);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (1, 3, Single_Type, True, Int (PV_Buffer_Size), Int (Vec4_Size));
         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);
      end loop;

      Geometry_VBO.Initialize_Id;
      Geometry_Texture.Initialize_Id;
      Texture_Buffer.Bind (Geometry_VBO);
      Allocate (Texture_Buffer, Long (1024 * 1024 * Vec4_Size), Dynamic_Copy);
      Texture_Buffer.Bind (Geometry_Texture);
      Allocate (Texture_Buffer, GL.Pixels.RGBA32F, Geometry_VBO);

      Render_VAO.Bind;
      Array_Buffer.Bind (Geometry_VBO);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 4, Single_Type, True, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

      Utilities.Clear_Background_Colour_And_Depth (Background);

      Load_VB_Object.Load_From_VBM ("../media/armadillo_low.vbm", VBM_Object,
                                    0, 1, 2, VBM_Result);
      VBM_Result := VBM_Result and Load_VB_Object.Get_Vertex_Count (VBM_Object) > 0;

      If not VBM_Result then
         Put_Line ("Main_Loop.Setup; Load_From_VBM failed.");
      end if;
      Load_VB_Object.Print_VBM_Object_Data ("Setup", VBM_Object);
      return VBM_Result;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   if Setup then
      while Running loop
--           delay (0.03);
         Display (Main_Window);
         Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
         Glfw.Input.Poll_Events;
         delay (0.03);
         Running := Running and not
           (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
         Running := Running and not Main_Window.Should_Close;
      end loop;
   end if;

exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
