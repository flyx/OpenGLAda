
with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
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

   Vec3_Size                   : constant UInt := GL.Types.Singles.Vector3'Size / 8;
   Vec4_Size                   : constant UInt := GL.Types.Singles.Vector4'Size / 8;
   Buffer_Size                 : constant UInt := Vec4_Size + Vec3_Size;
   Background                  : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);

   VAO                         : array (1 .. 2) of GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   VBO                         : array (1 .. 2) of GL.Objects.Buffers.Buffer;
   Transform_BO                : GL.Objects.Buffers.Transform_Buffer;

   Sort_Program                : GL.Objects.Programs.Program;
   Render_Program              : GL.Objects.Programs.Program;
   Model_Matrix_ID             : GL.Uniforms.Uniform;
   Projection_Matrix_ID        : GL.Uniforms.Uniform;
   Pass_Colour_ID              : GL.Uniforms.Uniform;

   VBM_Object                  : Load_VB_Object.VB_Object;
   Colours                     : constant array (1 .. 2) of
     GL.Types.Singles.Vector4 := ((0.8, 0.8, 0.9, 0.5),
                                  (0.3, 1.0, 0.3, 0.8));


   --  ------------------------------------------------------------------------

   procedure Display (Window : in out Glfw.Windows.Window) is
      use GL.Toggles;
      use GL.Types.Singles;
      use Maths;
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
      Aspect            : Single;
      Model_Matrix      : Singles.Matrix4;
      Projection_Matrix : Singles.Matrix4;
      Current_Time      : constant Single := Single (Glfw.Time);
   begin
      Disable (Cull_Face);
      Enable (Depth_Test);
      GL.Buffers.Set_Depth_Function (LEqual);
      Utilities.Clear_Background_Colour_And_Depth (Background);

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Aspect := Single (Window_Height) / Single (Window_Width);

      GL.Objects.Programs.Use_Program  (Sort_Program);
      Projection_Matrix :=
        Frustum_Matrix (Left   => -1.0,   Right => 1.0,
                        Bottom => Aspect, Top   => -Aspect,
                        Near   => 1.0,    Far   => 500.0);

      Model_Matrix :=
        Translation_Matrix ((0.0, 0.0, 100.0 *  Single_Math_Functions.Sin (2.0 * Ada.Numerics.Pi * Current_Time) - 230.0)) *
        Rotation_Matrix (Degree (360.0 * Current_Time), (1.0, 0.0, 0.0)) *
          Rotation_Matrix (Degree (360.0 * 2.0 * Current_Time), (0.0, 1.0, 0.0)) *
            Rotation_Matrix (Degree (360.0 * 5.0 * Current_Time), (0.0, 0.0, 0.1)) *
              Translation_Matrix ((0.0, -80.0, 0.0));

      GL.Uniforms.Set_Single (Model_Matrix_ID, Model_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      GL.Toggles.Enable (Rasterizer_Discard);
      GL.Objects.Buffers.Bind_Transform_Feedback (Transform_BO);

      GL.Objects.Buffers.Transform_Feedback_Buffer.Bind_Buffer_Base (0, VBO (1));
      GL.Objects.Buffers.Transform_Feedback_Buffer.Bind_Buffer_Base (1, VBO (2));

      GL.Objects.Programs.Begin_Transform_Feedback (Points);
         Load_VB_Object.Render (VBM_Object);
      GL.Objects.Programs.End_Transform_Feedback;

      GL.Toggles.Disable (Rasterizer_Discard);

      GL.Objects.Programs.Use_Program (Render_Program);
      for index in 1 .. 2 loop
         GL.Uniforms.Set_Single (Pass_Colour_ID, Colours (index));
         VAO (index).Bind;
         GL.Objects.Buffers.Draw_Transform_Feedback_Stream
           (Triangles, Transform_BO, UInt (index - 1));
      end loop;
   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Display.");
         raise;
   end Display;

   --  ------------------------------------------------------------------------

   function Setup return Boolean is
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
      VBM_Result     : Boolean := False;
      Varyings       : constant String :=
                         "rf_position,rf_normal,gl_NextBuffer,lf_position,lf_normal";
   begin
      Transform_BO.Initialize_Id;
      GL.Objects.Buffers.Bind_Transform_Feedback (Transform_BO);

      for index in VAO'Range loop
         VAO (index).Initialize_Id;
         VAO (index).Bind;
         VBO (index).Initialize_Id;
         Transform_Feedback_Buffer.Bind (VBO (index));
      end loop;

      --  Program_From includes linking
      Sort_Program := Program_From
        ((Src ("src/shaders/sort_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/sort_geometry_shader.glsl", Geometry_Shader)));

      if not GL.Objects.Programs.Link_Status (Sort_Program) then
         Put_Line ("Setup, Sort_Program Link failed");
         Put_Line (GL.Objects.Programs.Info_Log (Sort_Program));
      end if;

      Transform_Feedback_Varyings (Sort_Program, Varyings, Interleaved_Attribs);
      Sort_Program.Link;
      if not GL.Objects.Programs.Link_Status (Sort_Program) then
         Put_Line ("Setup, Sort_Program Transform_Feedback_Varyings Link failed.");
         Put_Line (GL.Objects.Programs.Info_Log (Sort_Program));
      end if;

      GL.Objects.Programs.Use_Program  (Sort_Program);

      Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Sort_Program, "model_matrix");
      Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Sort_Program, "projection_matrix");

      for index in 1 .. 2 loop
         VAO (index).Bind;
         Array_Buffer.Bind (VBO (index));

         Transform_Feedback_Buffer.Bind (VBO (index));
         Transform_Feedback_Buffer.Allocate (Long (1024 * 1024), Dynamic_Copy);

         GL.Attributes.Set_Vertex_Attrib_Pointer
           (0, 4, Single_Type, True, Int (Buffer_Size), 0);
         GL.Attributes.Set_Vertex_Attrib_Pointer
           (1, 3, Single_Type, True, Int (Buffer_Size), Int (Vec4_Size));
         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);
      end loop;

      Render_Program := Program_From
        ((Src ("src/shaders/render_10_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/render_10_fragment_shader.glsl", Fragment_Shader)));
      Pass_Colour_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "pass_colour");

      Utilities.Clear_Background_Colour_And_Depth (Background);

      Load_VB_Object.Load_From_VBM ("../media/ninja.vbm", VBM_Object,
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
         Display (Main_Window);
         Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
         Glfw.Input.Poll_Events;
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
