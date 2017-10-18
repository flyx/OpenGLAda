--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.Windows;
with Glfw.Monitors;
with Glfw.Input.Keys;

procedure Glfw_Test.Windows is

   type My_Window is new Glfw.Windows.Window with null record;

   overriding
   procedure Init (Object : not null access My_Window;
                   Width, Height : Glfw.Size;
                   Title   : String;
                   Monitor : Glfw.Monitors.Monitor := Glfw.Monitors.No_Monitor;
                   Share   : access Glfw.Windows.Window'Class := null);

   overriding
   procedure Key_Changed (Object   : not null access My_Window;
                          Key      : Glfw.Input.Keys.Key;
                          Scancode : Glfw.Input.Keys.Scancode;
                          Action   : Glfw.Input.Keys.Action;
                          Mods     : Glfw.Input.Keys.Modifiers);

   procedure Init (Object : not null access My_Window;
                   Width, Height : Glfw.Size;
                   Title   : String;
                   Monitor : Glfw.Monitors.Monitor := Glfw.Monitors.No_Monitor;
                   Share   : access Glfw.Windows.Window'Class := null) is
      Upcast : constant Glfw.Windows.Window_Reference
        := Glfw.Windows.Window (Object.all)'Access;
   begin
      Upcast.Init (Width, Height, Title, Monitor, Share);
      Object.Enable_Callback (Glfw.Windows.Callbacks.Key);
   end Init;

   procedure Key_Changed (Object   : not null access My_Window;
                          Key      : Glfw.Input.Keys.Key;
                          Scancode : Glfw.Input.Keys.Scancode;
                          Action   : Glfw.Input.Keys.Action;
                          Mods     : Glfw.Input.Keys.Modifiers) is
      pragma Unreferenced (Scancode);
      pragma Unreferenced (Action);
      pragma Unreferenced (Mods);
      use type Glfw.Input.Keys.Key;
   begin
      If Key = Glfw.Input.Keys.Escape then
         Object.Set_Should_Close (True);
      end if;
   end Key_Changed;


   W1 : aliased Glfw.Windows.Window;
   W2 : aliased My_Window;
begin
   Glfw.Init;
   Enable_Print_Errors;

   W1'Access.Init (640, 480, "Window 1");
   W2'Access.Init (640, 480, "Window 2");

   while not W2'Access.Should_Close loop
      Glfw.Input.Wait_For_Events;
   end loop;

   Glfw.Shutdown;
end Glfw_Test.Windows;
