

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;
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
with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is
   use GL.Types;

   Render_Program              : GL.Objects.Programs.Program;
   Vertex_Array                : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer               : GL.Objects.Buffers.Buffer;
   Element_Buffer              : GL.Objects.Buffers.Buffer;

   Render_Model_Matrix_ID      : GL.Uniforms.Uniform;
   Render_Projection_Matrix_ID : GL.Uniforms.Uniform;

   --  ------------------------------------------------------------------------

   procedure Render (Window :  in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      use GL.Objects.Vertex_Arrays;
      use GL.Types.Singles;
      Background        : constant GL.Types.Colors.Color := (0.7, 0.7, 0.7, 1.0);
      Window_Width      : Glfw.Size;
      Window_Height     : Glfw.Size;
      Aspect            : Single;
      Scale             : constant Single := 1.5; --  Increase to reduce size
      Model_Matrix      : Matrix4 := GL.Types.Singles.Identity4;
      Projection_Matrix : Matrix4;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      GL.Window.Set_Viewport (10, 10, GL.Types.Int (Window_Width) - 20,
                              GL.Types.Int (Window_Height) - 20);
      Aspect := Scale * Single (Window_Height) / Single (Window_Width);

      GL.Toggles.Enable (GL.Toggles.Cull_Face);
      Utilities.Clear_Background_Colour_And_Depth (Background);

      GL.Objects.Programs.Use_Program (Render_Program);

      --  Set up the projection matrix
      --  Top, Bottom, Left, Right, Near, Far
      Maths.Init_Orthographic_Transform (Scale, -Scale, -Aspect, Aspect, -1.0, 500.0,
                                         Projection_Matrix);
      GL.Uniforms.Set_Single (Render_Projection_Matrix_ID, Projection_Matrix);

      --  Set up for the Draw_Elements call
      Vertex_Array.Bind;
      Element_Array_Buffer.Bind (Element_Buffer);

      --  Draw arrays
      Model_Matrix := Model_Matrix * Maths.Translation_Matrix ((0.0, -0.3, 5.0));
      GL.Uniforms.Set_Single (Render_Model_Matrix_ID, Model_Matrix);
      Draw_Arrays (Triangles, 0, 3);

      -- Draw elements
      Model_Matrix :=  Model_Matrix * Maths.Translation_Matrix ((-0.5, 0.2, 5.0));
      GL.Uniforms.Set_Single (Render_Model_Matrix_ID, Model_Matrix);
      Draw_Elements (Triangles, 3, UInt_Type);

      -- Draw elements base vertex
      Model_Matrix :=  Maths.Translation_Matrix ((1.0, 0.4, 5.0));
      GL.Uniforms.Set_Single (Render_Model_Matrix_ID, Model_Matrix);
      Draw_Elements (Triangles, 3, UInt_Type);

      --  Draw arrays instanced
      Model_Matrix :=  Maths.Translation_Matrix ((1.5, 0.5, 5.0));
      GL.Uniforms.Set_Single (Render_Model_Matrix_ID, Model_Matrix);
      Draw_Arrays_Instanced (Triangles, 0, 3, 1);

   exception
      when  others =>
         Put_Line ("An exception occurred in Main_Loop.Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Setup is
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use Program_Loader;
      Vertex_Data_Bytes : constant Int := Vertex_Data.Vertex_Positions'Size / 8;
      Colour_Data_Size  : constant Int := Vertex_Data.Vertex_Colours'Size / 8;
   begin
      Render_Program := Program_From
        ((Src ("src/shaders/primitive_restart_vs.glsl", Vertex_Shader),
         Src ("src/shaders/primitive_restart_fs.glsl", Fragment_Shader)));

      Render_Model_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "model_matrix");
      Render_Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
        (Render_Program, "projection_matrix");

      --  Set up the vertex attributes
      --  This must be done before setting up the element array buffer
      --  otherwise, calls to Draw_Elements will fail
      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      --  Set up the element array buffer
      Element_Buffer.Initialize_Id;
      Element_Array_Buffer.Bind (Element_Buffer);
      Utilities.Load_Element_Buffer (Element_Array_Buffer,
                                     Vertex_Data.Vertex_Indices,
                                     Static_Draw);

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);

      Allocate (Array_Buffer, Long (Vertex_Data_Bytes + Colour_Data_Size), Static_Draw);
      Utilities.Load_Vertex_Sub_Buffer
        (Array_Buffer, 0, Vertex_Data.Vertex_Positions);
      Utilities.Load_Vertex_Sub_Buffer
        (Array_Buffer, Vertex_Data_Bytes, Vertex_Data.Vertex_Colours);

      GL.Attributes.Set_Vertex_Attrib_Pointer
        (0, 4, Single_Type, False, 0, 0);
      GL.Attributes.Set_Vertex_Attrib_Pointer
        (1, 4, Single_Type, False, 4 * 4, 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Enable_Vertex_Attrib_Array (1);

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
      Render (Main_Window);
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
