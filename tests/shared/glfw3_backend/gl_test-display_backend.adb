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

with Ada.Text_IO;

with Glfw.Windows.Context;
with Glfw.Windows.Hints;
with Glfw.Input.Keys;
with Glfw.Errors;

package body GL_Test.Display_Backend is
   type Test_Window is new Glfw.Windows.Window with null record;

   overriding
   procedure Close_Requested (Object : not null access Test_Window) is
   begin
      Object.Destroy;
   end Close_Requested;

   Main_Window : constant not null access Test_Window := new Test_Window;

   procedure Print_Error (Code : Glfw.Errors.Kind; Description : String) is
   begin
      Ada.Text_IO.Put_Line ("Error occured (" & Code'Img & "): " & Description);
   end Print_Error;

   procedure Enable_Print_Errors is
   begin
      Glfw.Errors.Set_Callback (Print_Error'Access);
   end Enable_Print_Errors;

   procedure Init is
   begin
      Enable_Print_Errors;
      Glfw.Init;
   end Init;

   procedure Open_Window (Width, Height : Natural; Depth_Bits : Natural := 0) is
   begin
      if not Main_Window.Initialized then
         Glfw.Windows.Hints.Set_Depth_Bits (Depth_Bits);
         Main_Window.Init (Glfw.Size (Width), Glfw.Size (Height),
                           "Test Window");
      end if;
      Main_Window.Show;
      Main_Window.Enable_Callback (Glfw.Windows.Callbacks.Close);
      Glfw.Windows.Context.Make_Current (Main_Window);
   end Open_Window;

   procedure Swap_Buffers is
   begin
      Glfw.Windows.Context.Swap_Buffers (Main_Window);
   end Swap_Buffers;

   procedure Poll_Events is
   begin
      Glfw.Input.Poll_Events;
   end Poll_Events;

   procedure Set_Window_Title (Value : String) is
   begin
      Main_Window.Set_Title (Value);
   end Set_Window_Title;

   function Escape_Pressed return Boolean is
      use type Glfw.Input.Button_State;
   begin
      return Main_Window.Initialized and then
        Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed;
   end Escape_Pressed;

   function Window_Opened return Boolean is
   begin
      return Main_Window.Initialized and then Main_Window.Visible;
   end Window_Opened;

   procedure Close_Window is
   begin
      Main_Window.Destroy;
   end Close_Window;

   procedure Shutdown renames Glfw.Shutdown;

   procedure Configure_Minimum_OpenGL_Version (Major, Minor : Natural) is
   begin
      Glfw.Windows.Hints.Set_Minimum_OpenGL_Version (Major, Minor);
      -- needed for OSX
      if Major >= 3 then
         Glfw.Windows.Hints.Set_Forward_Compat (True);
         Glfw.Windows.Hints.Set_Profile (Glfw.Windows.Context.Core_Profile);
      end if;
   end Configure_Minimum_OpenGL_Version;

end GL_Test.Display_Backend;
