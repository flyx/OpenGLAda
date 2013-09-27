--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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
--------------------------------------------------------------------------------

with Glfw.Windows;
with Glfw.Monitors;
with Glfw.Input.Keys;

procedure Glfw_Test.Windows is

   Stop : Boolean := False;

   type My_Window is new Glfw.Windows.Window with null record;

   overriding
   procedure Init (Object : not null access My_Window;
                   Width, Height : Natural;
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
                   Width, Height : Natural;
                   Title   : String;
                   Monitor : Glfw.Monitors.Monitor := Glfw.Monitors.No_Monitor;
                   Share   : access Glfw.Windows.Window'Class := null) is
      Upcast : Glfw.Windows.Window_Reference
        := Glfw.Windows.Window (Object.all)'Access;
   begin
      Upcast.Init (Width, Height, Title, Monitor, Share);
      Object.Enable_Callback (Glfw.Windows.Key);
   end Init;

   procedure Key_Changed (Object   : not null access My_Window;
                          Key      : Glfw.Input.Keys.Key;
                          Scancode : Glfw.Input.Keys.Scancode;
                          Action   : Glfw.Input.Keys.Action;
                          Mods     : Glfw.Input.Keys.Modifiers) is
      use type Glfw.Input.Keys.Key;
   begin
      If Key = Glfw.Input.Keys.Escape then
         Stop := True;
      end if;
   end Key_Changed;


   W1 : aliased Glfw.Windows.Window;
   W2 : aliased My_Window;
begin
   Glfw.Init;
   Enable_Print_Errors;

   W1'Access.Init (640, 480, "Window 1");
   W2'Access.Init (640, 480, "Window 2");

   while not Stop loop
      Glfw.Input.Poll_Events;
   end loop;

   Glfw.Terminate_Glfw;
end Glfw_Test.Windows;
