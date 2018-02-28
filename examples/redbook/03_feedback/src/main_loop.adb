

with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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

--  with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   type PV_Buffer is record
      Position : Singles.Vector4;
      Velocity : Singles.Vector3;
   end record;
   type Buffer_Array is array (UInt range <>) of aliased PV_Buffer;
   --     type Texture_Array is array (Integer range <>) of aliased Singles.Vector4;

   package Buffer_Pointers_Package is new
     Interfaces.C.Pointers (UInt, PV_Buffer, Buffer_Array,
                            ((0.0, 0.0, 0.0, 0.0), (10.0 ** 20, 10.0 ** 20, 10.0 ** 20)));

   --     package Texture_Pointers_Package is new
   --       Interfaces.C.Pointers (Integer, Singles.Vector4, Texture_Array,
   --                              (10.0 ** 20, 10.0 ** 20, 10.0 ** 20, 10.0 ** 20));

   procedure Load_Transform_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (Buffer_Pointers_Package);
   --     procedure Load_Texture_Buffer is new
   --       GL.Objects.Buffers.Load_To_Buffer (Texture_Pointers_Package);

   procedure Map_Buffer is new
     GL.Objects.Buffers.Map (Buffer_Pointers_Package);

--       type Varyings_Length_1 is new Transform_Feedback_API.Varyings_Array (1 .. 1);
--     type Varyings_Length_2 is new Transform_Feedback_API.Varyings_Array (1 .. 2);

   Black               : constant GL.Types.Colors.Color := (0.0, 0.0, 0.0, 1.0);
   --     Dark_Blue           : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 1.0);

   Vertex_Arrays       : array (1 .. 2) of GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffers      : array (1 .. 2) of GL.Objects.Buffers.Buffer;
   VBM_Object          : Load_VB_Object.VB_Object;
   Geometry_VBO        : GL.Objects.Buffers.Buffer;
   Geometry_Texture    : GL.Objects.Buffers.Buffer;
   Render_Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Point_Count         : constant UInt := 5000;
   Triangle_Count              : constant UInt := 0;
   Time_Step                   : constant UInt := 0;
   Model_Matrix_ID             : GL.Uniforms.Uniform;
   Projection_Matrix_ID        : GL.Uniforms.Uniform;
   Triangle_Count_ID           : GL.Uniforms.Uniform;
   Time_Step_ID                : GL.Uniforms.Uniform;
   Render_Model_Matrix_ID      : GL.Uniforms.Uniform;
   Render_Projection_Matrix_ID : GL.Uniforms.Uniform;
   Model_Matrix        :  Singles.Matrix4 := GL.Types.Singles.Identity4;
   Render_Model_Matrix : constant Singles.Matrix4 := GL.Types.Singles.Identity4;
   Projection_Matrix    : Singles.Matrix4;
   Render_Projection_Matrix : Singles.Matrix4;
   Render_Program       : GL.Objects.Programs.Program;
   Update_Program       : GL.Objects.Programs.Program;
   Buffer               : Buffer_Array (1 .. Point_Count);
   Buffer_Pointer       : Buffer_Pointers_Package.Pointer;
   --     T_Buffer             : Texture_Array (1 .. Point_Count);
   Frame_Count          : UInt := 0;

   --  ------------------------------------------------------------------------

   function Random_Vector (Min_Mag : Float := 0.0; Max_Mag : Float := 1.0)
                           return GL.Types.Singles.Vector3 is
      use Ada.Numerics.Float_Random;
      use GL.Types.Singles;
      Gen : Generator;
      RV  : Vector3;
   begin
      --        GL.Toggles.Enable (GL.Toggles.Normalize);
      RV := (2.0 * Single (Random (Gen)) - 1.0,
             2.0 * Single (Random (Gen)) - 1.0, 2.0 * Single (Random (Gen)) - 1.0);
      return Single (Min_Mag + Random (Gen) * (Max_Mag - Min_Mag)) * RV;

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
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
      Aspect        : Single;
      Current_Time  : Float;
--        q             : Float := 0.0;
--        X             : Vector3 := (1.0, 0.0, 0.0);
--        Y             : Vector3 := (0.0, 1.0, 0.0);
--        Z             : Vector3 := (0.0, 0.0, 1.0);
   begin
      Current_Time :=  Float (Glfw.Time);
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width),
                              GL.Types.Int (Window_Height));
      Aspect := Single (Window_Height) / Single (Window_Width);
      Init_Orthographic_Transform (-1.0, 1.0, -Aspect, Aspect, 1.0, 5000.0,
                                   Projection_Matrix);
      Projection_Matrix := Translation_Matrix ((0.0, 0.0, -100.0)) *
        Projection_Matrix;
      Model_Matrix :=  Scaling_Matrix (0.3) *
        Rotation_Matrix (Degree (360.0 * Current_Time), (0.0, 1.0, 0.0)) *
          Rotation_Matrix (Degree (360.0 * 3.0 * Current_Time), (0.0, 0.0, 1.0));

      Utilities.Clear_Colour_And_Depth;
      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      GL.Buffers.Set_Depth_Function (LEqual);

      GL.Objects.Programs.Use_Program (Render_Program);
      GL.Uniforms.Set_Single (Render_Model_Matrix_ID, Render_Model_Matrix);
      GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

      Render_Vertex_Array.Bind;
      Transform_Feedback_Buffer.Bind_Buffer_Base (0, Geometry_VBO);

      GL.Objects.Programs.Begin_Transform_Feedback (Triangles);
      Load_VB_Object.Render (VBM_Object);
      GL.Objects.Programs.End_Transform_Feedback;

      Model_Matrix := Identity4;
      GL.Objects.Programs.Use_Program (Render_Program);
--        Put_Line ("Main_Loop.Display returned from Use_Program.");
      GL.Uniforms.Set_Single (Model_Matrix_ID, Model_Matrix);
      GL.Uniforms.Set_Single (Render_Projection_Matrix_ID, Render_Projection_Matrix);
      GL.Uniforms.Set_UInt (Triangle_Count_ID, Triangle_Count);
      GL.Uniforms.Set_UInt (Time_Step_ID, Time_Step);
      --
      --        GL.Attributes.Enable_Vertex_Attrib_Array (0);
      --
      --        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
      --
      --        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 3);
--        GL.Attributes.Disable_Vertex_Attrib_Array (0);
      if (Frame_Count rem 2) /= 0 then
         Vertex_Arrays (1).Initialize_Id;
      else
         Vertex_Arrays (2).Initialize_Id;
      end if;

      GL.Objects.Programs.Begin_Transform_Feedback (Points);
      GL.Objects.Vertex_Arrays.Draw_Arrays
        (Points, 1, GL.Types.Size (Maths.Minimum (Point_Count, Frame_Count / 8)));
      GL.Objects.Programs.End_Transform_Feedback;

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

   type Varyings_Length_1 is new GL.Objects.Programs.Varyings_Array (1 .. 1);
   procedure Setup is
      use GL.Objects.Buffers;
      use GL.Objects.Programs;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;
      Velocity   : Vector3;
      VBM_Result : Boolean;
      Varyings   : constant Varyings_Array (1 .. 2) :=
        (To_Unbounded_String ("position_out"),
         To_Unbounded_String ("velocity_out"));
      Varyings_2  : constant Varyings_Length_1 :=
        (Varyings_Length_1'First => To_Unbounded_String ("world_space_position"));
   begin
      Projection_Matrix := GL.Types.Singles.Identity4;
      Render_Projection_Matrix := GL.Types.Singles.Identity4;
      Render_Program := Program_From
        ((Src ("src/shaders/render_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/blue_fragment_shader.glsl", Fragment_Shader)));

      Update_Program := Program_From
        ((Src ("src/shaders/update_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/white_fragment_shader.glsl", Fragment_Shader)));

      Transform_Feedback_Varyings (Update_Program, 2, Varyings, Interleaved_Attribs);
      Transform_Feedback_Varyings (Render_Program, 1, Varyings_2, Interleaved_Attribs);

      GL.Objects.Programs.Link (Render_Program);
      if GL.Objects.Programs.Link_Status (Render_Program) then
         Put_Line ("Setup, Render_Program Link failed");
      end if;

      Put_Line (GL.Objects.Programs.Info_Log (Update_Program));
      Put_Line (GL.Objects.Programs.Info_Log (Render_Program));
      If not GL.Objects.Programs.Validate_Status (Update_Program) then
         Put_Line ("Setup, Invalid Update_Program status");
      end if;
      If not GL.Objects.Programs.Validate_Status (Render_Program) then
         Put_Line ("Setup, Invalid Render_Program status");
      end if;
      GL.Objects.Programs.Use_Program (Render_Program);
      Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "model_matrix");
      Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "projection_matrix");
      Triangle_Count_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "triangle_count");
      Time_Step_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "time_step");
      Render_Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "model_matrix");
      Render_Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "projection_matrix");

      Vertex_Arrays (1).Initialize_Id;
      Vertex_Arrays (2).Initialize_Id;

      Vertex_Buffers (1).Initialize_Id;
      Vertex_Buffers (2).Initialize_Id;

      for index in Vertex_Buffers'Range loop
         Transform_Feedback_Buffer.Bind (Vertex_Buffers (index));
         Load_Transform_Buffer (Transform_Feedback_Buffer, Buffer, Dynamic_Copy);
         if index = Vertex_Buffers'First then
            Map_Buffer (Transform_Feedback_Buffer, GL.Objects.Write_Only, Buffer_Pointer);
            for i2 in 1 .. Point_Count loop
               Velocity := Random_Vector;
               Buffer (i2).Position := To_Vector4 (Velocity) + (-0.5, 40.0, 0.0, 1.0);
               Buffer (i2).Velocity := (Velocity (GL.X), 0.3 * Velocity (GL.Y),
                                        0.3 * Velocity (GL.Z));
            end loop;
            Unmap (Transform_Feedback_Buffer);
         end if;
         Vertex_Arrays (index).Bind;
         Array_Buffer.Bind (Vertex_Buffers (index));

         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 4, Single_Type, 0, 0);
         GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, 0, 0);
         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         GL.Attributes.Enable_Vertex_Attrib_Array (1);
      end loop;

      Geometry_VBO.Initialize_Id;
      Geometry_Texture.Initialize_Id;
      Texture_Buffer.Bind (Geometry_VBO);
      --        Load_Texture_Buffer (Texture_Buffer, Point_Count, Dynamic_Copy);
      Texture_Buffer.Bind (Geometry_Texture);
      Allocate (Texture_Buffer, GL.Pixels.RGBA32F, Geometry_VBO);

      Render_Vertex_Array.Initialize_Id;
      GL.Objects.Vertex_Arrays.Bind (Render_Vertex_Array);
      Array_Buffer.Bind (Geometry_VBO);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 4, Single_Type, 0, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

      Utilities.Clear_Background_Colour_And_Depth (Black);

      Load_VB_Object.Load_From_VBM ("../media/armadillo_low.vbm", VBM_Object,
                                    0, 1, 2, VBM_Result);
      If not VBM_Result then
         Put_Line ("Main_Loop.Setup; Load_From_VBM failed.");
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup;
   while Running loop
      Display (Main_Window);
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;
exception
   when others =>
      Put_Line ("An exception occurred in Main_Loop.");
      raise;
end Main_Loop;
