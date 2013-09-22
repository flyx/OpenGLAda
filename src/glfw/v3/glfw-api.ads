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

with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with System;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Monitors;
with Glfw.Errors;
with Glfw.Enums;

private package Glfw.API is

   -----------------------------------------------------------------------------
   -- Types
   -----------------------------------------------------------------------------

   type Address_List is array (Positive range <>) of aliased System.Address;
   pragma Convention (C, Address_List);

   package Address_List_Pointers is new Interfaces.C.Pointers
     (Positive, System.Address, Address_List, System.Null_Address);

   package VMode_List_Pointers is new Interfaces.C.Pointers
     (Positive, Monitors.Video_Mode, Monitors.Video_Mode_List, (others => 0));

   type Unsigned_Short_List is array (Positive range <>) of aliased
     Interfaces.C.unsigned_short;

   package Unsigned_Short_List_Pointers is new Interfaces.C.Pointers
     (Positive, Interfaces.C.unsigned_short, Unsigned_Short_List, 0);

   type Raw_Gamma_Ramp is record
      Red, Green, Blue : Unsigned_Short_List_Pointers.Pointer;
      Size : Interfaces.C.unsigned;
   end record;
   pragma Convention (C, Raw_Gamma_Ramp);

   -----------------------------------------------------------------------------
   -- Callbacks
   -----------------------------------------------------------------------------

   type Error_Callback is access procedure (Code : Errors.Kind;
                                            Description : C.Strings.chars_ptr);
   type Window_Position_Callback is access procedure
     (Window : System.Address; X, Y : C.int);
   type Window_Size_Callback is access procedure
     (Window : System.Address; Width, Height : C.int);
   type Window_Close_Callback is access procedure
     (Window : System.Address);
   type Window_Refresh_Callback is access procedure
     (Window : System.Address);
   type Window_Focus_Callback is access procedure
     (Window : System.Address; Focussed : Bool);
   type Window_Iconify_Callback is access procedure
     (Window : System.Address; Iconified : Bool);
   type Framebuffer_Size_Callback is access procedure
     (Window : System.Address; Width, Height : C.int);
   type Mouse_Button_Callback is access procedure (Window : System.Address;
                                                   Button : Input.Mouse.Button;
                                                   Action : Input.Mouse.Button_Action;
                                                   Mods   : Input.Keys.Modifiers);
   type Cursor_Position_Callback is access procedure
     (Window : System.Address; X, Y : Input.Mouse.Coordinate);
   type Cursor_Enter_Callback is access procedure
     (Window : System.Address; Action : Input.Mouse.Enter_Action);
   type Key_Callback is access procedure (Window   : System.Address;
                                          Key      : Input.Keys.Key;
                                          Scancode : Input.Keys.Scancode;
                                          Action   : Input.Keys.Action;
                                          Mods     : Input.Keys.Modifiers);
   type Character_Callback is access procedure
     (Window : System.Address; Unicode_Char : Interfaces.C.unsigned);
   type Monitor_Callback is access procedure
     (Monitor : System.Address; Event : Monitors.Event);

   pragma Convention (C, Error_Callback);
   pragma Convention (C, Window_Position_Callback);
   pragma Convention (C, Window_Size_Callback);
   pragma Convention (C, Window_Close_Callback);
   pragma Convention (C, Window_Refresh_Callback);
   pragma Convention (C, Window_Focus_Callback);
   pragma Convention (C, Window_Iconify_Callback);
   pragma Convention (C, Framebuffer_Size_Callback);
   pragma Convention (C, Mouse_Button_Callback);
   pragma Convention (C, Cursor_Position_Callback);
   pragma Convention (C, Cursor_Enter_Callback);
   pragma Convention (C, Key_Callback);
   pragma Convention (C, Character_Callback);
   pragma Convention (C, Monitor_Callback);

   -----------------------------------------------------------------------------
   -- Basics
   -----------------------------------------------------------------------------

   function Init return C.int;
   pragma Import (Convention => StdCall, Entity => Init,
                  External_Name => "glfwInit");

   procedure Glfw_Terminate;
   pragma Import (Convention => StdCall, Entity => Glfw_Terminate,
                  External_Name => "glfwTerminate");

   procedure Get_Version (Major, Minor, Revision : out C.int);
   pragma Import (Convention => StdCall, Entity => Get_Version,
                  External_Name => "glfwGetVersion");

   function Get_Time return C.double;
   pragma Import (Convention => StdCall, Entity => Get_Time,
                  External_Name => "glfwGetTime");

   procedure Set_Time (Value : C.double);
   pragma Import (Convention => StdCall, Entity => Set_Time,
                  External_Name => "glfwSetTime");

   procedure Sleep (Time : C.double);
   pragma Import (Convention => StdCall, Entity => Sleep,
                  External_Name => "glfwSleep");

   function Extension_Supported (Name : C.Strings.chars_ptr) return Bool;
   pragma Import (Convention => StdCall, Entity => Extension_Supported,
                  External_Name => "glfwExtensionSupported");

   function Set_Error_Callback (CB_Fun : Error_Callback) return Error_Callback;
   pragma Import (Convention => StdCall, Entity => Set_Error_Callback,
                  External_Name => "glfwSetErrorCallback");

   -----------------------------------------------------------------------------
   -- Monitors
   -----------------------------------------------------------------------------

   function Get_Monitors (Count : access Interfaces.C.int)
                          return Address_List_Pointers.Pointer;
   pragma Import (Convention => StdCall, Entity => Get_Monitors,
                  External_Name => "glfwGetMonitors");

   function Get_Primary_Monitor return System.Address;
   pragma Import (Convention => StdCall, Entity => Get_Primary_Monitor,
                  External_Name => "glfwGetPrimaryMonitor");

   procedure Get_Monitor_Pos (Monitor : System.Address;
                              XPos, YPos : out Interfaces.C.int);
   pragma Import (Convention => StdCall, Entity => Get_Monitor_Pos,
                  External_Name => "glfwGetMonitorPos");

   procedure Get_Monitor_Physical_Size (Monitor : System.Address;
                                        Width, Height : out Interfaces.C.int);
   pragma Import (Convention => StdCall, Entity => Get_Monitor_Physical_Size,
                  External_Name => "glfwGetMonitorPhysicalSize");

   function Get_Monitor_Name (Monitor : System.Address)
                              return Interfaces.C.Strings.chars_ptr;
   pragma Import (Convention => StdCall, Entity => Get_Monitor_Name,
                  External_Name => "glfwGetMonitorName");

   function Set_Monitor_Callback (Monitor : System.Address;
                                  CB_Fun  : Monitor_Callback)
                                  return Monitor_Callback;
   pragma Import (Convention => StdCall, Entity => Set_Monitor_Callback,
                  External_Name => "glfwSetMonitorCallback");

   function Get_Video_Modes (Monitor : System.Address;
                             Count : access Interfaces.C.int)
                             return VMode_List_Pointers.Pointer;
   pragma Import (Convention => StdCall, Entity => Get_Video_Modes,
                  External_Name => "glfwGetVideoModes");

   function Get_Video_Mode (Monitor : System.Address)
                            return access constant Monitors.Video_Mode;
   pragma Import (Convention => StdCall, Entity => Get_Video_Mode,
                  External_Name => "glfwGetVideoMode");

   procedure Set_Gamma (Monitor : System.Address; Gamma : Interfaces.C.C_float);
   pragma Import (Convention => StdCall, Entity => Set_Gamma,
                  External_Name => "glfwSetGamma");

   function Get_Gamma_Ramp (Monitor : System.Address)
                            return access constant Raw_Gamma_Ramp;
   pragma Import (Convention => StdCall, Entity => Get_Gamma_Ramp,
                  External_Name => "glfwGetGammaRamp");

   procedure Set_Gamma_Ramp (Monitor : System.Address;
                             Value   : access constant Raw_Gamma_Ramp);
   pragma Import (Convention => StdCall, Entity => Set_Gamma_Ramp,
                  External_Name => "glfwSetGammaRamp");

end Glfw.API;
