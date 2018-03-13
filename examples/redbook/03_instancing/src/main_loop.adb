
with Ada.Text_IO; use Ada.Text_IO;

--  with Interfaces.C.Pointers;

--  with GL.Attributes;
--  with GL.Buffers;
with GL.Objects;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
--  with GL.Objects.Textures;
--  with GL.Pixels;
--  with GL.Toggles;
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

   Background            : constant GL.Types.Colors.Color := (0.0, 1.0, 0.0, 0.0);

   --  BEGIN_APP_DECLARATION Member variables
   Colour_Buffer         : GL.Objects.Buffers.Buffer;
   Model_Matrix_Buffer   : GL.Objects.Buffers.Buffer;
   Colour_TBO            : GL.Objects.Buffers.Buffer;
   Model_Matrix_TBO      : GL.Objects.Buffers.Buffer;
   Render_Program        : GL.Objects.Programs.Program;

   View_Matrix_ID        : GL.Uniforms.Uniform;
   Projection_Matrix_ID  : GL.Uniforms.Uniform;

   VBM_Object            : Load_VB_Object.VB_Object;
   --  END_APP_DECLARATION

   Vertex_Location       : constant Int := 0;
   Normal_Location       : constant Int := 1;
   Tex_Coord0_Location   : constant Int := 2;

   --  Display static variables
   Num_Instances         :  UInt := 100;

   --  ------------------------------------------------------------------------

   procedure Display (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      use GL.Types.Singles;
      use Maths;
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
--        Aspect            : Single;
--        Model_Matrices    : Singles.Matrix4_Array (1 .. 4);
      View_Matrix       : Singles.Matrix4;
      Projection_Matrix : Singles.Matrix4;
      Scale             : constant Single := 400.0;
--        Current_Time      : Float;  --  t
--        a                 : Single;
--        b                 : Single;
--        c                 : Single;
--        Time_Component    : Single;
   begin
--        Current_Time :=  Float (Glfw.Time);
      Utilities.Clear_Background_Colour_And_Depth (Background);
--        GL.Toggles.Enable (GL.Toggles.Cull_Face);
--        GL.Toggles.Enable (GL.Toggles.Depth_Test);
--        GL.Buffers.Set_Depth_Function (LEqual);

      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (10, 10, GL.Types.Int (Window_Width) - 10,
                              GL.Types.Int (Window_Height) - 10);
--        Aspect := Single (Window_Height) / Single (Window_Width);

--        Time_Component := Single (360.0 * Current_Time);
--        for Index in Model_Matrices'Range loop
--           a := Single (50 * Index) / 4.0;
--           b := Single (50 * Index) / 5.0;
--           c := Single (50 * Index) / 6.0;
--           Model_Matrices (Index) := Identity4;
--             Maths.Translation_Matrix ((a + 10.0, b + 40.0, c + 50.0)) *
--             Maths.Rotation_Matrix (Degree (c + Time_Component), (0.0, 0.0, 1.0)) *
--             Maths.Rotation_Matrix (Degree (a + Time_Component), (1.0, 0.0, 0.0)) *
--             Maths.Rotation_Matrix (Degree (b + Time_Component), (0.0, 1.0, 0.0));
--        end loop;

      --  Bind the weight VBO and change its data
--        Texture_Buffer.Bind (Model_Matrix_Buffer);
--        Utilities.Load_Texture_Buffer (Texture_Buffer, Model_Matrices, Dynamic_Draw);

      GL.Objects.Programs.Use_Program (Render_Program);
      View_Matrix := Maths.Scaling_Matrix (Scale) * Identity4;
--             Maths.Translation_Matrix ((0.0, 0.0, -1500.0)) *
--             Maths.Rotation_Matrix (Degree (2.0 * Time_Component), (0.0, 1.0, 0.0));
--        Init_Orthographic_Transform (-1.0, 1.0, -Aspect, Aspect, -1.0, 5000.0,
--                                     Projection_Matrix);
      Projection_Matrix := Identity4;
      GL.Uniforms.Set_Single (View_Matrix_ID, View_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);
      Num_Instances := 0;
      Load_VB_Object.Render (VBM_Object, 1, Num_Instances);

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
      Colours              : Singles.Vector4_Array (1 .. Int (Num_Instances));
      a                    : Single;
      b                    : Single;
      c                    : Single;
      VBM_Result           : Boolean := False;
   begin
      Load_VB_Object.Load_From_VBM ("../media/armadillo_low.vbm", VBM_Object,
                                    Vertex_Location, Normal_Location,
                                    Tex_Coord0_Location, VBM_Result);
      VBM_Result := VBM_Result and Load_VB_Object.Get_Vertex_Count (VBM_Object) > 0;

      If not VBM_Result then
         Put_Line ("Main_Loop.Setup; Load_From_VBM failed.");
      end if;
      Load_VB_Object.Print_VBM_Object_Data ("Setup", VBM_Object);
      Load_VB_Object.Print_VBM_Frame_Data ("Setup, Frame 1", VBM_Object, 1);
      Load_VB_Object.Print_Attributes_Header ("Setup, Vertex Attribute", VBM_Object, 0);
      Load_VB_Object.Print_Attributes_Header ("Setup, Normal Attribute", VBM_Object, 1);
      Load_VB_Object.Print_Attributes_Header ("Setup, Tex Coord Attribute", VBM_Object, 2);

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

      for index in Colours'Range loop
         a := Single (index) / 4.0;
         b := Single (index) / 5.0;
         c := Single (index) / 6.0;
         Colours (index) (GL.X) :=
           0.5 + 0.25 * (1.0 + Maths.Single_Math_Functions.Sin (1.0 + a));
         Colours (index) (GL.Y) :=
           0.5 + 0.25 * (1.0 + Maths.Single_Math_Functions.Sin (2.0 + b));
         Colours (index) (GL.Z) :=
           0.5 + 0.25 * (1.0 + Maths.Single_Math_Functions.Sin (3.0 + c));
         Colours (index) (GL.W) := 1.0;
      end loop;

--        Texture_Buffer.Bind (Colour_Buffer);
--        Utilities.Load_Texture_Buffer (Texture_Buffer, Colours, Static_Draw);
--        Allocate (Texture_Buffer, GL.Pixels.RGBA32F, Colour_Buffer);

--        GL.Objects.Textures.Set_Active_Unit (1);
--        Texture_Buffer.Bind (Model_Matrix_TBO);

--        Texture_Buffer.Bind (Model_Matrix_Buffer);
--        Texture_Buffer_Allocate (Texture_Buffer,
--                                 Long (Num_Instances * Singles.Matrix4'Size),
--                                 Dynamic_Draw);
--
--        Allocate (Texture_Buffer, GL.Pixels.RGBA32F, Model_Matrix_Buffer);
--        GL.Objects.Textures.Set_Active_Unit (0);

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
