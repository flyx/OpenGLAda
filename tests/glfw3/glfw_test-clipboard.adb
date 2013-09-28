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

with Glfw.Windows.Clipboard;
with Glfw.Monitors;
with Glfw.Input;

procedure Glfw_Test.Clipboard is

   type My_Window is new Glfw.Windows.Window with null record;

   overriding
   procedure Init (Object : not null access My_Window;
                   Width, Height : Glfw.Size;
                   Title   : String;
                   Monitor : Glfw.Monitors.Monitor := Glfw.Monitors.No_Monitor;
                   Share   : access Glfw.Windows.Window'Class := null);

   overriding
   procedure Focus_Changed (Object : not null access My_Window;
                            Focused : Boolean);

   procedure Init (Object : not null access My_Window;
                   Width, Height : Glfw.Size;
                   Title   : String;
                   Monitor : Glfw.Monitors.Monitor := Glfw.Monitors.No_Monitor;
                   Share   : access Glfw.Windows.Window'Class := null) is
      Upcast : Glfw.Windows.Window_Reference
        := Glfw.Windows.Window (Object.all)'Access;
   begin
      Upcast.Init (Width, Height, Title, Monitor, Share);
      Glfw.Windows.Clipboard.Set
        (Object, "Unfocus to change clipboard contents to window title");
      Object.Enable_Callback (Glfw.Windows.Callbacks.Focus);
   end Init;

   procedure Focus_Changed (Object : not null access My_Window;
                            Focused : Boolean) is
   begin
      if Focused then
         Object.Set_Title (Glfw.Windows.Clipboard.Get (Object));
      else
         Glfw.Windows.Clipboard.Set
           (Object, Glfw.Windows.Clipboard.Get (Object) & " | focus lost");
      end if;
   end Focus_Changed;

   W : aliased My_Window;
begin
   Glfw.Init;
   Enable_Print_Errors;

   W'Access.Init (640, 480, "");

   while not W'Access.Should_Close loop
      Glfw.Input.Wait_For_Events;
   end loop;

   Glfw.Shutdown;
end Glfw_Test.Clipboard;
