-- Copyright (c) 2017, Leo Brewin <Leo.Brewin@monash.edu>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Deallocation;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;   use GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Windows.Context;

with Maths;
with Program_Loader;
with Utilities;

with libOBJ;

package body meshviewlib is

   package Double_Math_Functions is new Ada.Numerics.Generic_Elementary_Functions (GL.Types.Double);

   --  Camera position, Look_At and Up are in world coordinates.
   Initial_Camera_Position : GL.Types.Singles.Vector3 := (0.0, 0.0, 4.0);
   Initial_Direction       : GL.Types.Singles.Vector3 := (0.0, 0.0, -1.0);
   Initial_Look_At         : GL.Types.Singles.Vector3 := (0.0, 0.0, 0.0);
   Initial_Up              : GL.Types.Singles.Vector3 := (0.0, 1.0, 0.0);

   Camera_Position         : GL.Types.Singles.Vector3 := Initial_Camera_Position;
   Direction               : GL.Types.Singles.Vector3 := Initial_Direction;
   Look_At                 : GL.Types.Singles.Vector3 := Initial_Look_At;
   Up                      : GL.Types.Singles.Vector3 := Initial_Up;
   Right                   : GL.Types.Singles.Vector3 := GL.Types.Singles.Cross_Product (Direction, Up);

   function Degrees (Angle : GL.Types.Double) return GL.Types.Double is
      Radians_Per_Degree : constant GL.Types.Double := Ada.Numerics.Pi / 180.0;
      Degrees_Per_Radian : constant GL.Types.Double := 180.0 / Ada.Numerics.Pi;
   begin
      return Angle * Degrees_Per_Radian;
   end Degrees;

   procedure Mouse_Position_Changed
     (Object : not null access Main_Window_Type;
      X, Y   : Glfw.Input.Mouse.Coordinate)
   is
      use GL.Types.Doubles;
      use Double_Math_Functions;

      vec_beg : Vector3 := (1.0, 0.0, 0.0);
      vec_end : Vector3 := (1.0, 0.0, 0.0);

      window_height : Glfw.Size := 360;
      window_width  : Glfw.Size := 480;

      function Unit_Vector (the_vec : GL.Types.Doubles.Vector3) return GL.Types.Doubles.Vector3 is
         tmp_len : GL.Types.Double;
         tmp_x   : GL.Types.Double := the_vec (GL.X);
         tmp_y   : GL.Types.Double := the_vec (GL.Y);
         tmp_z   : GL.Types.Double := the_vec (GL.Z);
      begin

         tmp_len := Sqrt (tmp_x * tmp_x + tmp_y * tmp_y + tmp_z * tmp_z);
         return (tmp_x / tmp_len, tmp_y / tmp_len, tmp_z / tmp_len);

      end Unit_Vector;

      function Length (the_vec : GL.Types.Doubles.Vector3) return Double is
         tmp_x : GL.Types.Double := the_vec (GL.X);
         tmp_y : GL.Types.Double := the_vec (GL.Y);
         tmp_z : GL.Types.Double := the_vec (GL.Z);
      begin

         return Sqrt (tmp_x * tmp_x + tmp_y * tmp_y + tmp_z * tmp_z);

      end Length;

      -------------------------------------------------------------------------
      -- adapated from tinyobj/examples/viewer/trackball.c
      -- original C code by Gavin Bell, Syoyo Fujita and others
      -- see full credits at https://github.com/syoyo/tinyobjloader-c

      function arcball_vector (vec : GL.Types.Doubles.Vector2) return GL.Types.Doubles.Vector3 is
         use GL;

         the_vec : Vector3;
         the_dot : Double;

      begin

         -- vector from the centre of the window to the (x,y) point

         the_vec := ( 2.0 * vec (GL.X) / Double (window_width)  - 1.0,
                     -2.0 * vec (GL.Y) / Double (window_height) + 1.0, -- note: sign change required for correct up-down rotations
                      0.0);

         the_dot := Dot_Product (the_vec, the_vec);

         -- old code: rotations are *not* continuous with mouse movement
         -- if the_dot < 1.0
         --    then the_vec (GL.z) := sqrt (1.0-the_dot);  -- extend vector to the unit xyz-sphere
         --    else the_vec := the_vec / sqrt (the_dot);   -- extend vector to the unit xy-circle
         -- end if;

         -- new code: rotations *are* continuous with mouse movement
         if (2.0 * the_dot < 1.0)
            then the_vec (GL.Z) := Sqrt (1.0 - the_dot);
            else the_vec (GL.Z) := 1.0 / (2.0 * Sqrt (the_dot));
         end if;

         return the_vec;

      end arcball_vector;

      function Capped (x : GL.Types.Double) return GL.Types.Double is
      begin
         if    x >  1.0 then return  1.0;
         elsif x < -1.0 then return -1.0;
         else return x;
         end if;
      end Capped;

      new_pos : GL.Types.Doubles.Vector2 renames Object.new_pos;
      old_pos : GL.Types.Doubles.Vector2 renames Object.old_pos;

      rotation_axis  : GL.Types.Doubles.Vector3 renames Object.rotation_axis;
      rotation_angle : GL.Types.Double          renames Object.rotation_angle;

   begin

      Object.Get_Framebuffer_Size (window_width, window_height);

      if Object.Left_Button_Down then
         new_pos        := (Double (X), Double (Y));
         vec_beg        := arcball_vector (old_pos);  -- vector from centre of window to mouse down pos
         vec_end        := arcball_vector (new_pos);  -- vector from centre of window to mouse up pos
         rotation_axis  := Unit_Vector (Cross_Product (vec_beg, vec_end));
         rotation_angle := Degrees (2.0 * Arcsin (Capped (Length (vec_beg - vec_end) / 2.0)));
      end if;

      old_pos := (Double (X), Double (Y));

   end Mouse_Position_Changed;

   procedure Mouse_Button_Changed
     (Object : not null access Main_Window_Type;
      Button : Glfw.Input.Mouse.Button;
      State  : Glfw.Input.Button_State;
      Mods   : Glfw.Input.Keys.Modifiers)
   is
      use Glfw.Input;
      use Glfw.Input.Mouse;
   begin

      if Button = Left_Button then
         case State is
            when Pressed  => Object.Left_Button_Down := True;
            when Released => Object.Left_Button_Down := False;
         end case;
      end if;

   end Mouse_Button_Changed;

   procedure Mouse_Scrolled
     (Object : not null access Main_Window_Type;
      X, Y   : Glfw.Input.Mouse.Scroll_Offset)
   is
   begin
      Object.camera_shift := -Single (Y);
   end Mouse_Scrolled;

   -- unused here
   procedure Key_Changed
     (Object   : not null access Main_Window_Type;
      Key      : Glfw.Input.Keys.Key;
      Scancode : Glfw.Input.Keys.Scancode;
      Action   : Glfw.Input.Keys.Action;
      Mods     : Glfw.Input.Keys.Modifiers)
   is
      use Glfw.Input.Keys;
   begin
      null;
   end Key_Changed;

   -- unused here
   procedure Character_Entered
     (Object : not null access Main_Window_Type;
      Char   : Wide_Wide_Character)
   is
   begin
      null;
   end Character_Entered;

   function Which_Key_Pressed (Window : in out Main_Window_Type'Class) return Glfw.Input.Keys.Key is
      use Glfw.Input;
   begin

      if    Window.Key_State (Glfw.Input.Keys.R)     = Glfw.Input.Pressed then return Glfw.Input.Keys.R;
      elsif Window.Key_State (Glfw.Input.Keys.Left)  = Glfw.Input.Pressed then return Glfw.Input.Keys.Left;
      elsif Window.Key_State (Glfw.Input.Keys.Right) = Glfw.Input.Pressed then return Glfw.Input.Keys.Right;
      elsif Window.Key_State (Glfw.Input.Keys.Up)    = Glfw.Input.Pressed then return Glfw.Input.Keys.Up;
      elsif Window.Key_State (Glfw.Input.Keys.Down)  = Glfw.Input.Pressed then return Glfw.Input.Keys.Down;
      else
         return Glfw.Input.Keys.Unknown;
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Which_Key_Pressed.");
         raise;
   end Which_Key_Pressed;

   procedure Main_Loop (Main_Window : in out Main_Window_Type'Class) is

      Dark_Blue             : Colors.Color := (0.0, 0.0, 0.4, 1.0);

      Background_Colour     : Colors.Color := Dark_Blue;

      Light_Position        : GL.Types.Singles.Vector3 := (4.0, 4.0, 4.0);

      Vertices_Array_Object : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Vertex_Buffer         : GL.Objects.Buffers.Buffer;
      Colour_Buffer         : GL.Objects.Buffers.Buffer;
      Normal_Buffer         : GL.Objects.Buffers.Buffer;

      Render_Program        : GL.Objects.Programs.Program;

      MVP_Matrix_ID         : GL.Uniforms.Uniform;
      View_Matrix_ID        : GL.Uniforms.Uniform;
      Model_Matrix_ID       : GL.Uniforms.Uniform;
      Light_Position_ID     : GL.Uniforms.Uniform;

      MVP_Matrix            : GL.Types.Singles.Matrix4;        -- This is THE matrix, built from MVP = P*V*M.

      Model_Matrix          : GL.Types.Singles.Matrix4;        -- The Model_Matrix operates in world coordinates.

      View_Matrix           : GL.Types.Singles.Matrix4;        -- The View_Matrix transforms the world_cordinates of the world view
                                                               -- into view (camera) coordinates.

      Projection_Matrix     : GL.Types.Singles.Matrix4;        -- The Projection_Matrix projects the camera view in camera coordinates
                                                               -- onto the camera view's Near plane

      num_vertices          : Int;
      num_textures          : Int;
      num_normals           : Int;
      num_triangles         : Int;

      num_data              : Int;  -- 3 * num_triangles = number of items loaded into the vertex buffers,
                                    -- one item per vertex per triangle

      new_time              : Single := 0.0;
      del_time              : Single := 0.0;
      old_time              : Single := 0.0;

      procedure Centre_Mouse_Cursor (Window : in out Main_Window_Type'Class) is
         Window_Width  : Glfw.Size;
         Window_Height : Glfw.Size;
         the_x, the_y  : Single;
      begin

         -- put mouse cursor at the middle of the window

         Window.Get_Size (Window_Width, Window_Height);

         the_x := 0.5 * Single (Window_Width);
         the_y := 0.5 * Single (Window_Height);

         Window.Set_Cursor_Pos (Glfw.Input.Mouse.Coordinate (the_x),
                                Glfw.Input.Mouse.Coordinate (the_y));

         Window.new_pos := (Double (the_x), Double (the_y));
         Window.old_pos := (Double (the_x), Double (the_y));

      end Centre_Mouse_Cursor;

      procedure Initial_MVP_Matrix
        (Window         : in out Main_Window_Type'Class;
         Render_Program :        GL.Objects.Programs.Program)
      is
         use GL.Types.Singles;
         use Glfw.Input;
         use Maths;
         Window_Width  : Glfw.Size;
         Window_Height : Glfw.Size;
      begin

         Window.Get_Framebuffer_Size (Window_Width, Window_Height);

         Centre_Mouse_Cursor (Window);

         Camera_Position := Initial_Camera_Position;
         Direction       := Initial_Direction;
         Look_At         := Initial_Look_At;
         Up              := Initial_Up;

         Model_Matrix    := Singles.Identity4;                                -- set the Model matrix

         Look_At         := Camera_Position + Direction;                      -- set the point to look at

         Init_Lookat_Transform      (Camera_Position,  Look_At, Up,
                                     View_Matrix);                            -- set the View matrix

         Init_Perspective_Transform (60.0,
                                     Single (Window_Width),
                                     Single (Window_Height),
                                     0.1, 100.0,
                                     Projection_Matrix);                      -- set the Projection matrix

         MVP_Matrix := Projection_Matrix * View_Matrix * Model_Matrix;        -- set the MVP matrix

      exception
         when others =>
            Put_Line ("An exception occurred in Initial_MVP_Matrix.");
            raise;
      end Initial_MVP_Matrix;

      procedure Update_MVP_Matrix
        (Window         : in out Main_Window_Type'Class;
         Render_Program :        GL.Objects.Programs.Program)
      is
         use GL.Types.Singles;
         use Glfw.Input;
         use Glfw.Input.Mouse;
         use Maths;

         the_axis  : Vector3;
         the_angle : Degree;

         speed_move   : Single := 2.0;
         speed_rotate : Degree := 2.0;

      begin

         the_angle := speed_rotate * Degree (Window.rotation_angle);

         the_axis (GL.X) := Single (Window.rotation_axis (GL.X));
         the_axis (GL.Y) := Single (Window.rotation_axis (GL.Y));
         the_axis (GL.Z) := Single (Window.rotation_axis (GL.Z));

         Model_Matrix := Maths.Rotation_Matrix (the_angle, the_axis) * Model_Matrix;

         case Which_Key_Pressed (Window) is

            when Glfw.Input.Keys.R     => Initial_MVP_Matrix (Window, Render_Program);
            when Glfw.Input.Keys.Left  => Camera_Position := Camera_Position + speed_move * del_time * Right;
            when Glfw.Input.Keys.Right => Camera_Position := Camera_Position - speed_move * del_time * Right;
            when Glfw.Input.Keys.Up    => Camera_Position := Camera_Position - speed_move * del_time * Up;
            when Glfw.Input.Keys.Down  => Camera_Position := Camera_Position + speed_move * del_time * Up;

            when others =>
               null;

         end case;

         Camera_Position (GL.Z) := Camera_Position (GL.Z) + Window.camera_shift;   -- update the camera position

         Look_At := Camera_Position + Direction;                                   -- update the point to look at

         Init_Lookat_Transform (Camera_Position, Look_At, Up, View_Matrix);        -- update the View matrix

         MVP_Matrix := Projection_Matrix * View_Matrix * Model_Matrix;             -- update the MVP matrix

         if not Window.Left_Button_Down then  -- allow object to spin when left button down during drag
            Window.rotation_angle := 0.0;
         end if;

         Window.camera_shift := 0.0;

      exception
         when others =>
            Put_Line ("An exception occurred in Update_MVP_Matrix.");
            raise;
      end Update_MVP_Matrix;

      procedure Render (Window : in out Main_Window_Type'Class) is
         use GL.Types.Singles;
         use GL.Objects.Buffers;
         use Maths;
         Window_Width  : Glfw.Size;
         Window_Height : Glfw.Size;
      begin

         Window.Get_Framebuffer_Size (Window_Width, Window_Height);

         GL.Window.Set_Viewport (0, 0, GL.Types.Int (Window_Width), GL.Types.Int (Window_Height));

         Utilities.Clear_Background_Colour_And_Depth (Background_Colour);

         GL.Objects.Programs.Use_Program (Render_Program);

         Update_MVP_Matrix (Window, Render_Program);

         GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);
         GL.Uniforms.Set_Single (View_Matrix_ID, View_Matrix);
         GL.Uniforms.Set_Single (Model_Matrix_ID, Model_Matrix);
         GL.Uniforms.Set_Single (Light_Position_ID, Light_Position (GL.X),
                                                    Light_Position (GL.Y),
                                                    Light_Position (GL.Z));

         --  First attribute buffer : Vertices
         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         Array_Buffer.Bind (Vertex_Buffer);
         GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

         --  Second attribute buffer : Colours
         GL.Attributes.Enable_Vertex_Attrib_Array (1);
         Array_Buffer.Bind (Colour_Buffer);
         GL.Attributes.Set_Vertex_Attrib_Pointer (1, 3, Single_Type, 0, 0);

         --  Third attribute buffer : Normals
         GL.Attributes.Enable_Vertex_Attrib_Array (2);
         Array_Buffer.Bind (Normal_Buffer);
         GL.Attributes.Set_Vertex_Attrib_Pointer (2, 3, Single_Type, 0, 0);

         GL.Objects.Vertex_Arrays.Draw_Arrays (Mode => Triangles, First => 0, Count => 3 * num_triangles);

         GL.Attributes.Disable_Vertex_Attrib_Array (0);
         GL.Attributes.Disable_Vertex_Attrib_Array (1);
         GL.Attributes.Disable_Vertex_Attrib_Array (2);

      exception
         when others =>
            Put_Line ("An exception occurred in Render.");
            raise;

      end Render;

      procedure Setup (Window : in out Main_Window_Type'Class) is
         use GL.Types.Singles;
         use GL.Objects.Buffers;
         use GL.Objects.Shaders;
         use Glfw.Input;

      begin

         Window.Set_Input_Toggle (Glfw.Input.Sticky_Keys, True);

         Window.Set_Cursor_Mode (Mouse.Normal);

         Centre_Mouse_Cursor (Window);

         GL.Toggles.Enable (GL.Toggles.Depth_Test);
         GL.Buffers.Set_Depth_Function (GL.Types.Less);
         Utilities.Clear_Background_Colour_And_Depth (Background_Colour);

         Vertices_Array_Object.Initialize_Id;
         Vertices_Array_Object.Bind;

         Render_Program :=
           Program_Loader.Program_From
             ((Program_Loader.Src ("src/shaders/MVP_Vertex_Shader.glsl", Vertex_Shader),
               Program_Loader.Src ("src/shaders/Colour_Fragment_Shader.glsl", Fragment_Shader)));

         MVP_Matrix_ID     := GL.Objects.Programs.Uniform_Location (Render_Program, "MVP");
         View_Matrix_ID    := GL.Objects.Programs.Uniform_Location (Render_Program, "V");
         Model_Matrix_ID   := GL.Objects.Programs.Uniform_Location (Render_Program, "M");
         Light_Position_ID := GL.Objects.Programs.Uniform_Location (Render_Program, "LightPosition_worldspace");

         Initial_MVP_Matrix (Window, Render_Program);

         --------------------------------------------------------------------
         -- read a mesh from an obj file

         libOBJ.Preview_Obj_File (num_vertices, num_textures, num_normals, num_triangles, mesh_file_name);

         num_data := 3 * num_triangles;

         declare

            type vertex_data_ptr is access Singles.Vector3_Array;
            type colour_data_ptr is access Singles.Vector3_Array;
            type normal_data_ptr is access Singles.Vector3_Array;

            vertex_data_acc : vertex_data_ptr := new Singles.Vector3_Array (1 .. num_data);
            colour_data_acc : colour_data_ptr := new Singles.Vector3_Array (1 .. num_data);
            normal_data_acc : normal_data_ptr := new Singles.Vector3_Array (1 .. num_data);

            vertex_data : Singles.Vector3_Array renames vertex_data_acc.all;
            colour_data : Singles.Vector3_Array renames colour_data_acc.all;
            normal_data : Singles.Vector3_Array renames normal_data_acc.all;

            procedure FreeVertices is new Ada.Unchecked_Deallocation (Singles.Vector3_Array, vertex_data_ptr);
            procedure FreeColours  is new Ada.Unchecked_Deallocation (Singles.Vector3_Array, colour_data_ptr);
            procedure FreeNormals  is new Ada.Unchecked_Deallocation (Singles.Vector3_Array, normal_data_ptr);

         begin

            libOBJ.Load_Obj_File
              (vertex_data,
               colour_data,
               normal_data,
               num_vertices,
               num_textures,
               num_normals,
               num_triangles,
               mesh_file_name,
               texture_file_name);

            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, vertex_data, Static_Draw);

            Colour_Buffer.Initialize_Id;
            Array_Buffer.Bind (Colour_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, colour_data, Static_Draw);

            Normal_Buffer.Initialize_Id;
            Array_Buffer.Bind (Normal_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, normal_data, Static_Draw);

            FreeVertices (vertex_data_acc);
            FreeColours  (colour_data_acc);
            FreeNormals  (normal_data_acc);

         end;
         --------------------------------------------------------------------

         -- enable callbacks to handle mouse events

         -- Window.Enable_Callback (Glfw.Windows.Callbacks.Key);
         -- Window.Enable_Callback (Glfw.Windows.Callbacks.Char);
         Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Button);
         Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Position);
         Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Scroll);

      exception
         when others =>
            Put_Line ("An exception occurred in Setup.");
            raise;
      end Setup;

      ------------------------------------------------------------------------

      use Glfw.Input;

      Running : Boolean := True;

   begin

      Setup (Main_Window);

      while Running loop
         new_time := Single (Glfw.Time);
         del_time := new_time - old_time;
         Render (Main_Window);
         Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
         old_time := new_time;
         Glfw.Input.Poll_Events;
         Running := Running and then not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
         Running := Running and then not Main_Window.Should_Close;
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Main_Loop.");
         raise;

   end Main_Loop;

end meshviewlib;
