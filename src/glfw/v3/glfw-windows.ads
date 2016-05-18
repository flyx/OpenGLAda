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

with System;

with Glfw.Input.Mouse;
with Glfw.Input.Keys;
with Glfw.Monitors;

private with Ada.Finalization;

package Glfw.Windows is

   type Window is tagged private;
   type Window_Reference is not null access all Window;

   Creation_Error : exception;

   package Callbacks is
      -- avoid pollution of Glfw.Windows package with symbols

      type Kind is (Position, Size, Close, Refresh, Focus, Iconify,
                    Framebuffer_Size, Mouse_Button, Mouse_Position,
                    Mouse_Scroll, Mouse_Enter, Key, Char);
   end Callbacks;

   subtype Coordinate is Interfaces.C.int;

   -- throws Creation_Error if the window cannot be created
   procedure Init (Object        : not null access Window;
                   Width, Height : Size;
                   Title         : String; -- interpreted as UTF-8
                   Monitor       : Monitors.Monitor := Monitors.No_Monitor;
                   Share_Resources_With : access Window'Class := null);
   function Initialized (Object : not null access Window) return Boolean;
   procedure Destroy (Object : not null access Window);

   procedure Show (Object : not null access Window);
   procedure Hide (Object : not null access Window);

   procedure Set_Title (Object : not null access Window; Value : String);

   procedure Get_OpenGL_Version (Object : not null access Window;
                                 Major, Minor, Revision : out Natural);

   function Key_State (Object : not null access Window; Key : Input.Keys.Key)
                       return Input.Button_State;

   function Mouse_Button_State (Object : not null access Window;
                                Button : Input.Mouse.Button)
                                return Input.Button_State;

   procedure Set_Cursor_Mode (Object : not null access Window;
                              Mode   : Input.Mouse.Cursor_Mode);

   procedure Get_Cursor_Pos (Object : not null access Window;
                             X, Y   : out Input.Mouse.Coordinate);
   procedure Set_Cursor_Pos (Object : not null access Window;
                             X, Y   : Input.Mouse.Coordinate);

   procedure Get_Position (Object : not null access Window;
                           X, Y : out Coordinate);
   procedure Set_Position (Object : not null access Window;
                           X, Y : Coordinate);

   procedure Get_Size (Object : not null access Window;
                       Width, Height : out Size);
   procedure Set_Size (Object : not null access Window;
                       Width, Height : Size);

   procedure Get_Framebuffer_Size (Object : not null access Window;
                                   Width, Height : out Size);

   function Visible   (Object : not null access Window) return Boolean;
   function Iconified (Object : not null access Window) return Boolean;
   function Focused   (Object : not null access Window) return Boolean;

   function Should_Close (Object : not null access Window) return Boolean;
   procedure Set_Should_Close (Object : not null access Window;
                               Value  : Boolean);

   -----------------------------------------------------------------------------
   -- Event API
   -----------------------------------------------------------------------------

   procedure Enable_Callback (Object : not null access Window;
                              Subject : Callbacks.Kind);
   
   procedure Disable_Callback (Object : not null access Window;
                              Subject : Callbacks.Kind);

   
   procedure Position_Changed (Object : not null access Window;
                               X, Y : Integer) is null;
   procedure Size_Changed (Object : not null access Window;
                           Width, Height : Natural) is null;
   procedure Close_Requested (Object : not null access Window) is null;
   procedure Refresh (Object : not null access Window) is null;
   procedure Focus_Changed (Object : not null access Window;
                            Focused : Boolean) is null;
   procedure Iconification_Changed (Object : not null access Window;
                                    Iconified : Boolean) is null;
   procedure Framebuffer_Size_Changed (Object : not null access Window;
                                       Width, Height : Natural) is null;
   procedure Mouse_Button_Changed (Object  : not null access Window;
                                   Button  : Input.Mouse.Button;
                                   State   : Input.Button_State;
                                   Mods    : Input.Keys.Modifiers) is null;
   procedure Mouse_Position_Changed (Object : not null access Window;
                                     X, Y   : Input.Mouse.Coordinate) is null;
   procedure Mouse_Scrolled (Object : not null access Window;
                             X, Y   : Input.Mouse.Scroll_Offset) is null;
   procedure Mouse_Entered (Object : not null access Window;
                            Action : Input.Mouse.Enter_Action) is null;
   procedure Key_Changed (Object   : not null access Window;
                          Key      : Input.Keys.Key;
                          Scancode : Input.Keys.Scancode;
                          Action   : Input.Keys.Action;
                          Mods     : Input.Keys.Modifiers) is null;
   procedure Character_Entered (Object : not null access Window;
                                Char   : Wide_Wide_Character) is null;

private
   type Window is new Ada.Finalization.Controlled with record
      Handle : System.Address := System.Null_Address;
   end record;
end Glfw.Windows;
