
with Ada.Text_IO; use Ada.Text_IO;

with GL.Toggles;

with Glfw;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;
with Glfw.Windows.Hints;

with Utilities;

procedure Initialize (Main_Window  : in out Glfw.Windows.Window;
                      Window_Title : String; Show_Data : Boolean := True) is

   procedure Enable_Callbacks is
   begin
      Main_Window.Enable_Callback (Glfw.Windows.Callbacks.Size);
      Main_Window.Enable_Callback (Glfw.Windows.Callbacks.Key);
   end Enable_Callbacks;

   --  ------------------------------------------------------------------------

   procedure Set_Window_Hints is
      Min_Major_Version : constant Integer := 3;
      Minor_Version     : constant Integer := 2;
   begin
      Glfw.Windows.Hints.Set_Minimum_OpenGL_Version
          (Min_Major_Version, Minor_Version);
      Glfw.Windows.Hints.Set_Forward_Compat (True);
      Glfw.Windows.Hints.Set_Profile (Glfw.Windows.Context.Core_Profile);
      Glfw.Windows.Hints.Set_Debug_Context (True);
      --  Set samples to 16 before taking screen shots.
      Glfw.Windows.Hints.Set_Samples (4);
   end Set_Window_Hints;

   --  ------------------------------------------------------------------------

   Window_Width  : constant Glfw.Size := 800;
   Window_Height : constant Glfw.Size := 600;
   Cursor        : constant Glfw.Input.Mouse.Cursor_Mode :=
     Glfw.Input.Mouse.Hidden;
begin
   Set_Window_Hints;
   Main_Window.Init (Window_Width, Window_Height, Window_Title);
   Glfw.Windows.Context.Make_Current (Main_Window'Access);
   Enable_Callbacks;
   --  The following default settings can be overriden by th user's
   --  setup code if necessary
   Main_Window.Set_Cursor_Mode (Cursor);
   GL.Toggles.Disable (GL.Toggles.Blend);
   if Show_Data then
      Utilities.Show_GL_Data;
   end if;

exception
   when others =>
      Put_Line ("An exception occurred in Initialize.");
      raise;

end Initialize;
