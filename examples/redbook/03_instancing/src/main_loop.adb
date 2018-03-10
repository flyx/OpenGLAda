
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C.Pointers;

with GL.Attributes;
with GL.Buffers;
with GL.Objects;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
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

--     type PV_Buffer is record
--        Position : Singles.Vector4;
--        Velocity : Singles.Vector3;
--     end record;
--
--     type PV_Buffer_Array is array (UInt range <>) of aliased PV_Buffer;
--
--     package PV_Buffer_Package is new Interfaces.C.Pointers
--       (UInt, PV_Buffer, PV_Buffer_Array, PV_Buffer'(others => <>));
--     procedure Map_PV_Buffer is new
--       GL.Objects.Buffers.Map (PV_Buffer_Package);

   Vec3_Size                   : constant UInt := GL.Types.Singles.Vector3'Size / 8;
   Vec4_Size                   : constant UInt := GL.Types.Singles.Vector4'Size / 8;
--     PV_Buffer_Size              : constant UInt := Vec4_Size + Vec3_Size;
   Background                  : constant GL.Types.Colors.Color := (0.0, 1.0, 0.0, 0.0);
   --     Dark_Blue           : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 1.0);

   --  BEGIN_APP_DECLARATION Member variables
   VAO                         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Colour_Buffer               : GL.Objects.Buffers.Buffer;
   Model_Matrix_Buffer         : GL.Objects.Buffers.Buffer;
   Colour_TBO                  : GL.Objects.Buffers.Buffer;
   Model_Matrix_TBO            : GL.Objects.Buffers.Buffer;
   Render_Program              : GL.Objects.Programs.Program;

   View_Matrix_ID              : GL.Uniforms.Uniform;
   Projection_Matrix_ID        : GL.Uniforms.Uniform;

   VBM_Object                  : Load_VB_Object.VB_Object;
   --  END_APP_DECLARATION

   Instance_Count              : constant Int := 100;

   --  Display static variables
   Last_Time                   : Float := 0.0;


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
      Scale             : constant Single := 40.0;
      --        Scale             : constant Single := 0.3;
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

      if Current_Time > Last_Time then    --  t > q
         GL.Uniforms.Set_Single (Time_Step_ID,
                                 2000.0 * Single (Current_Time - Last_Time));
      end if;
      Last_Time := Current_Time;   --  q = t

      if (Frame_Count rem 2) = 0 then   --  (frame_count & 1) != 0
         VAO (2).Bind;
         Transform_Feedback_Buffer.Bind_Buffer_Base (0, VBO (1));
      else
         VAO (1).Bind;
         Transform_Feedback_Buffer.Bind_Buffer_Base (0, VBO (2));
      end if;


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

   function Setup return Boolean is
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use Program_Loader;
      Colour_TBO_ID        : GL.Uniforms.Uniform;
      Model_Matrix_TBO_ID  : GL.Uniforms.Uniform;
      Colours              : Singles.Vector4_Array (1 .. Instance_Count);
      a                    : Single;
      b                    : Single;
      c                    : Single;
      VBM_Result           : Boolean := False;
   begin
      VAO.Initialize_Id;
      VAO.Bind;

      Colour_TBO.Initialize_Id;
      Texture_Buffer.Bind (Colour_TBO);
      Colour_Buffer.Initialize_Id;
      Texture_Buffer.Bind (Colour_Buffer);
      Model_Matrix_TBO.Initialize_Id;
      Texture_Buffer.Bind (Model_Matrix_TBO);
      Model_Matrix_Buffer.Initialize_Id;
      Texture_Buffer.Bind (Model_Matrix_Buffer);

      Render_Program := Program_From
        ((Src ("src/shaders/render_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/render_fragment_shader.glsl", Fragment_Shader)));

      GL.Objects.Programs.Use_Program  (Render_Program);
      View_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "View_matrix");
      Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "projection_matrix");
      --   Set up the TBO samplers
      Colour_TBO_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "colour_tbo");
      Model_Matrix_TBO_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "model_matrix_tbo");
      --  Set them to the correct texture unit indices
      GL.Uniforms.Set_Int (Colour_TBO_ID, 0);
      GL.Uniforms.Set_Int (Model_Matrix_TBO_ID, 1);

      Load_VB_Object.Load_From_VBM ("../media/armadillo_low.vbm", VBM_Object,
                                    0, 1, 2, VBM_Result);
      VBM_Result := VBM_Result and Load_VB_Object.Get_Vertex_Count (VBM_Object) > 0;

      If not VBM_Result then
         Put_Line ("Main_Loop.Setup; Load_From_VBM failed.");
      end if;
      Load_VB_Object.Print_VBM_Object_Data ("Setup", VBM_Object);

      for index in Colours'Range loop
         a := Single (index) / 4.0;
         b := Single (index) / 5.0;
         c := Single (index) / 6.0;
         Colours (index) (GL.X) :=
           0.5 + 0.25 * (1.0 + Maths.Single_Math_Functions.Sin (1.0 + a));
         Colours (index) (GL.Y) :=
           0.5 + 0.25 * (1.0 + Maths.Single_Math_Functions.Sin (2.0 + a));
         Colours (index) (GL.Z) :=
           0.5 + 0.25 * (1.0 + Maths.Single_Math_Functions.Sin (3.0 + a));
         Colours (index) (GL.W) := 1.0;
      end loop;

      Texture_Buffer.Bind (Colour_Buffer);
      Utilities.Load_Texture_Buffer (Texture_Buffer, Colours, Static_Draw);
      Allocate (Texture_Buffer, GL.Pixels.RGBA32F, Colour_Buffer);

      GL.Objects.Textures.Set_Active_Unit (1);
      Texture_Buffer.Bind (Model_Matrix_TBO);

      Texture_Buffer.Bind (Model_Matrix_Buffer);
      Texture_Buffer_Allocate (Texture_Buffer,
                               Long (Instance_Count * Singles.Matrix4'Size),
                               Dynamic_Draw);

      Allocate (Texture_Buffer, GL.Pixels.RGBA32F, Model_Matrix_Buffer);
      GL.Objects.Textures.Set_Active_Unit (0);

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
