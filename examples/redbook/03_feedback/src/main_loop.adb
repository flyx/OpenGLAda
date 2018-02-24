

with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C.Pointers;

with GL.Attributes;
with GL.Objects;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

with Feedback;
with Transform_Feedback_API;
--  with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;
   use Ada.Strings.Unbounded;

   type PV_Buffer is record
      Position : Singles.Vector4;
      Velocity : Singles.Vector3;
   end record;
   type Buffer_Array is array (Integer range <>) of aliased PV_Buffer;

   package Buffer_Pointers_Package is new
     Interfaces.C.Pointers (Integer, PV_Buffer, Buffer_Array,
                            ((0.0, 0.0, 0.0, 0.0), (10.0 ** 20, 10.0 ** 20, 10.0 ** 20)));

   procedure Load_Transform_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (Buffer_Pointers_Package);

   procedure Map_Buffer is new
     GL.Objects.Buffers.Map (Buffer_Pointers_Package);

   type Varyings_Array_1 is new Transform_Feedback_API.Varyings_Array (1 .. 1);
   type Varyings_Array_2 is new Transform_Feedback_API.Varyings_Array (1 .. 2);

   Vertex_Arrays     : array (1 .. 2) of GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffers    : array (1 .. 2) of GL.Objects.Buffers.Buffer;
   Geometry_Buffer   : GL.Objects.Buffers.Buffer;
   Geometry_Texture   : GL.Objects.Textures.Texture;
   Render_Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Point_Count       : constant Integer := 5000;
   Model_Matrix      : constant Singles.Matrix4 := GL.Types.Singles.Identity4;
   View_Matrix       : Singles.Matrix4;
   Projection_Matrix : Singles.Matrix4;
   MVP_Matrix        : Singles.Matrix4;
   Render_Program    : GL.Objects.Programs.Program;
   Update_Program    : GL.Objects.Programs.Program;
   Varyings          : constant Varyings_Array_2 :=
     (To_Unbounded_String ("position_out"),
      To_Unbounded_String ("velocity_out"));
   Varyings_2        : constant Varyings_Array_1 :=
     (Varyings_Array_1'First => To_Unbounded_String ("world_space_position"));
   Buffer            : Buffer_Array (1 .. Point_Count) :=
     (others => ((0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0)));
   Buffer_Pointer    : Buffer_Pointers_Package.Pointer;

   --  ------------------------------------------------------------------------

   function Random_Vector (Min_Mag : Float := 0.0; Max_Mag : Float := 1.0)
                           return GL.Types.Singles.Vector3 is
      use Ada.Numerics.Float_Random;
      use GL.Toggles;
      use GL.Types.Singles;
      Gen : Generator;
      RV  : constant Vector3 :=
        (2.0 * Single (Random (Gen)) - 1.0, 2.0 * Single (Random (Gen)) - 1.0,
         2.0 * Single (Random (Gen)) - 1.0);
   begin
      Enable (Normalize);
     return Single (Min_Mag + Random (Gen) * (Max_Mag - Min_Mag)) * RV;
   end Random_Vector;

   --  ------------------------------------------------------------------------

  procedure Render is
      use GL.Objects.Buffers;
      Dark_Blue : constant GL.Types.Colors.Color := (0.0, 0.0, 0.4, 1.0);
   begin
      Utilities.Clear_Background_Colour (Dark_Blue);
      GL.Objects.Programs.Use_Program (Render_Program);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);

      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 3);
      GL.Attributes.Disable_Vertex_Attrib_Array (0);

   exception
      when  others =>
         Put_Line ("An exceptiom occurred in Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup is
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use GL.Types.Singles;
      use Program_Loader;

      Model_Matrix_ID             : GL.Uniforms.Uniform;
      Projection_Matrix_ID        : GL.Uniforms.Uniform;
      Triangle_Count_ID           : GL.Uniforms.Uniform;
      Time_Step_ID                : GL.Uniforms.Uniform;
      Render_Model_Matrix_ID      : GL.Uniforms.Uniform;
      Render_Projection_Matrix_ID : GL.Uniforms.Uniform;
      Velocity                    : Vector3;
   begin
      Render_Program := Program_From
        ((Src ("src/shaders/render_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/blue_fragment_shader.glsl", Fragment_Shader)));

      Update_Program := Program_From
        ((Src ("src/shaders/update_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/white_fragment_shader.glsl", Fragment_Shader)));

      Feedback.Transform_Feedback_Varyings (Update_Program, 2,
                                            Transform_Feedback_API.Varyings_Array (Varyings),
                                            Transform_Feedback_API.GL_Interleaved_Attribs);

      Feedback.Transform_Feedback_Varyings (Update_Program, 2,
                                            Transform_Feedback_API.Varyings_Array (Varyings_2),
                                            Transform_Feedback_API.GL_Interleaved_Attribs);
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

      Geometry_Buffer.Initialize_Id;
      Render_Vertex_Array.Initialize_Id;
      Texture_Buffer.Bind (Geometry_Buffer);
      GL.Uniforms.Set_Single (Model_Matrix_ID, Model_Matrix);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup.");
         raise;
   end Setup;

   --  ------------------------------------------------------------------------

   use Glfw.Input;
   Running : Boolean := True;
begin
   Setup;
   while Running loop
      Render;
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;
exception
   when others =>
      Put_Line ("An exceptiom occurred in Main_Loop.");
      raise;
end Main_Loop;
