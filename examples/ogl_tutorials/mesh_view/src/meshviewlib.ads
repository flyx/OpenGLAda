with GL;
with GL.Types;

with Glfw.Windows;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;

with libCLI;

package meshviewlib is

   type Main_Window_Type is new Glfw.Windows.Window with record

      Left_Button_Down : Boolean := False;

      old_pos : GL.Types.Doubles.Vector2;
      new_pos : GL.Types.Doubles.Vector2;

      rotation_axis  : GL.Types.Doubles.Vector3;
      rotation_angle : GL.Types.Double;

      camera_shift : GL.Types.Single;

   end record;

   overriding procedure Mouse_Position_Changed
     (Object : not null access Main_Window_Type;
      X, Y   : Glfw.Input.Mouse.Coordinate);

   overriding procedure Mouse_Button_Changed
     (Object : not null access Main_Window_Type;
      Button : Glfw.Input.Mouse.Button;
      State  : Glfw.Input.Button_State;
      Mods   : Glfw.Input.Keys.Modifiers);

   overriding procedure Mouse_Scrolled
     (Object : not null access Main_Window_Type;
      X, Y   : Glfw.Input.Mouse.Scroll_Offset);

   overriding procedure Key_Changed
     (Object   : not null access Main_Window_Type;
      Key      : Glfw.Input.Keys.Key;
      Scancode : Glfw.Input.Keys.Scancode;
      Action   : Glfw.Input.Keys.Action;
      Mods     : Glfw.Input.Keys.Modifiers);

   overriding procedure Character_Entered
     (Object : not null access Main_Window_Type;
      Char   : Wide_Wide_Character);

   procedure Main_Loop (Main_Window : in out Main_Window_Type'Class);

   Main_Window : Main_Window_Type;

   mesh_file_name    : String := libCLI.get_mesh_name;
   texture_file_name : String := libCLI.get_texture_name;

end meshviewlib;
