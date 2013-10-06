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
with Glfw.Input.Joysticks;
with Glfw.Monitors;
with Glfw.Errors;
with Glfw.Enums;
with Glfw.Windows.Context;

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

   package Axis_Position_List_Pointers is new Interfaces.C.Pointers
     (Positive, Input.Joysticks.Axis_Position, Input.Joysticks.Axis_Positions,
      0.0);

   package Joystick_Button_State_List_Pointers is new Interfaces.C.Pointers
     (Positive, Input.Joysticks.Joystick_Button_State,
      Input.Joysticks.Joystick_Button_States, Input.Joysticks.Released);

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
                                                   State  : Input.Button_State;
                                                   Mods   : Input.Keys.Modifiers);
   type Cursor_Position_Callback is access procedure
     (Window : System.Address; X, Y : Input.Mouse.Coordinate);
   type Cursor_Enter_Callback is access procedure
     (Window : System.Address; Action : Input.Mouse.Enter_Action);
   type Scroll_Callback is access procedure
     (Window : System.Address; X, Y : Input.Mouse.Scroll_Offset);
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
   pragma Convention (C, Scroll_Callback);
   pragma Convention (C, Key_Callback);
   pragma Convention (C, Character_Callback);
   pragma Convention (C, Monitor_Callback);

   -----------------------------------------------------------------------------
   -- Basics
   -----------------------------------------------------------------------------

   function Init return C.int;
   pragma Import (Convention => C, Entity => Init,
                  External_Name => "glfwInit");

   procedure Glfw_Terminate;
   pragma Import (Convention => C, Entity => Glfw_Terminate,
                  External_Name => "glfwTerminate");

   procedure Get_Version (Major, Minor, Revision : out C.int);
   pragma Import (Convention => C, Entity => Get_Version,
                  External_Name => "glfwGetVersion");

   function Get_Version_String return C.Strings.chars_ptr;
   pragma Import (Convention => C, Entity => Get_Version_String,
                  External_Name => "glfwGetVersionString");

   function Get_Time return C.double;
   pragma Import (Convention => C, Entity => Get_Time,
                  External_Name => "glfwGetTime");

   procedure Set_Time (Value : C.double);
   pragma Import (Convention => C, Entity => Set_Time,
                  External_Name => "glfwSetTime");

   procedure Sleep (Time : C.double);
   pragma Import (Convention => C, Entity => Sleep,
                  External_Name => "glfwSleep");

   function Extension_Supported (Name : C.char_array) return Bool;
   pragma Import (Convention => C, Entity => Extension_Supported,
                  External_Name => "glfwExtensionSupported");

   function Set_Error_Callback (CB_Fun : Error_Callback) return Error_Callback;
   pragma Import (Convention => C, Entity => Set_Error_Callback,
                  External_Name => "glfwSetErrorCallback");

   -----------------------------------------------------------------------------
   -- Monitors
   -----------------------------------------------------------------------------

   function Get_Monitors (Count : access Interfaces.C.int)
                          return Address_List_Pointers.Pointer;
   pragma Import (Convention => C, Entity => Get_Monitors,
                  External_Name => "glfwGetMonitors");

   function Get_Primary_Monitor return System.Address;
   pragma Import (Convention => C, Entity => Get_Primary_Monitor,
                  External_Name => "glfwGetPrimaryMonitor");

   procedure Get_Monitor_Pos (Monitor : System.Address;
                              XPos, YPos : out Interfaces.C.int);
   pragma Import (Convention => C, Entity => Get_Monitor_Pos,
                  External_Name => "glfwGetMonitorPos");

   procedure Get_Monitor_Physical_Size (Monitor : System.Address;
                                        Width, Height : out Interfaces.C.int);
   pragma Import (Convention => C, Entity => Get_Monitor_Physical_Size,
                  External_Name => "glfwGetMonitorPhysicalSize");

   function Get_Monitor_Name (Monitor : System.Address)
                              return Interfaces.C.Strings.chars_ptr;
   pragma Import (Convention => C, Entity => Get_Monitor_Name,
                  External_Name => "glfwGetMonitorName");

   function Set_Monitor_Callback (Monitor : System.Address;
                                  CB_Fun  : Monitor_Callback)
                                  return Monitor_Callback;
   pragma Import (Convention => C, Entity => Set_Monitor_Callback,
                  External_Name => "glfwSetMonitorCallback");

   function Get_Video_Modes (Monitor : System.Address;
                             Count : access Interfaces.C.int)
                             return VMode_List_Pointers.Pointer;
   pragma Import (Convention => C, Entity => Get_Video_Modes,
                  External_Name => "glfwGetVideoModes");

   function Get_Video_Mode (Monitor : System.Address)
                            return access constant Monitors.Video_Mode;
   pragma Import (Convention => C, Entity => Get_Video_Mode,
                  External_Name => "glfwGetVideoMode");

   procedure Set_Gamma (Monitor : System.Address; Gamma : Interfaces.C.C_float);
   pragma Import (Convention => C, Entity => Set_Gamma,
                  External_Name => "glfwSetGamma");

   function Get_Gamma_Ramp (Monitor : System.Address)
                            return access constant Raw_Gamma_Ramp;
   pragma Import (Convention => C, Entity => Get_Gamma_Ramp,
                  External_Name => "glfwGetGammaRamp");

   procedure Set_Gamma_Ramp (Monitor : System.Address;
                             Value   : access constant Raw_Gamma_Ramp);
   pragma Import (Convention => C, Entity => Set_Gamma_Ramp,
                  External_Name => "glfwSetGammaRamp");

   -----------------------------------------------------------------------------
   -- Windows
   -----------------------------------------------------------------------------

   procedure Default_Window_Hints;
   pragma Import (Convention => C, Entity => Default_Window_Hints,
                  External_Name => "glfwDefaultWindowHints");

   procedure Window_Hint (Target : Glfw.Enums.Window_Hint; Info : C.int);
   procedure Window_Hint (Target : Glfw.Enums.Window_Hint; Info : Bool);
   procedure Window_Hint (Target : Glfw.Enums.Window_Hint;
                          Info   : Windows.Context.OpenGL_Profile_Kind);
   procedure Window_Hint (Target : Glfw.Enums.Window_Hint;
                          Info   : Windows.Context.API_Kind);
   procedure Window_Hint (Target : Glfw.Enums.Window_Hint;
                          Info   : Glfw.Windows.Context.Robustness_Kind);
   pragma Import (Convention => C, Entity => Window_Hint,
                  External_Name => "glfwWindowHint");

   function Create_Window (Width, Height : Interfaces.C.int;
                           Title         : Interfaces.C.char_array;
                           Monitor       : System.Address;
                           Share         : System.Address)
                           return System.Address;
   pragma Import (Convention => C, Entity => Create_Window,
                  External_Name => "glfwCreateWindow");

   procedure Destroy_Window (Window : System.Address);
   pragma Import (Convention => C, Entity => Destroy_Window,
                  External_Name => "glfwDestroyWindow");

   function Window_Should_Close (Window : System.Address)
                                 return Bool;
   pragma Import (Convention => C, Entity => Window_Should_Close,
                  External_Name => "glfwWindowShouldClose");

   procedure Set_Window_Should_Close (Window : System.Address;
                                      Value  : Bool);
   pragma Import (Convention => C, Entity => Set_Window_Should_Close,
                  External_Name => "glfwSetWindowShouldClose");

   procedure Set_Window_Title (Window : System.Address;
                               Title  : Interfaces.C.char_array);
   pragma Import (Convention => C, Entity => Set_Window_Title,
                  External_Name => "glfwSetWindowTitle");

   procedure Get_Window_Pos (Window : System.Address;
                             Xpos, Ypos : out Windows.Coordinate);
   pragma Import (Convention => C, Entity => Get_Window_Pos,
                  External_Name => "glfwGetWindowPos");

   procedure Set_Window_Pos (Window : System.Address;
                             Xpos, Ypos : Windows.Coordinate);
   pragma Import (Convention => C, Entity => Set_Window_Pos,
                  External_Name => "glfwSetWindowPos");

   procedure Get_Window_Size (Window : System.Address;
                              Width, Height : out Size);
   pragma Import (Convention => C, Entity => Get_Window_Size,
                  External_Name => "glfwGetWindowSize");

   procedure Set_Window_Size (Window : System.Address;
                              Width, Height : Size);
   pragma Import (Convention => C, Entity => Set_Window_Size,
                  External_Name => "glfwSetWindowSize");

   procedure Get_Framebuffer_Size (Window : System.Address;
                                   Width, Height : out Size);
   pragma Import (Convention => C, Entity => Get_Framebuffer_Size,
                  External_Name => "glfwGetFramebufferSize");

   procedure Iconify_Window (Window : System.Address);
   pragma Import (Convention => C, Entity => Iconify_Window,
                  External_Name => "glfwIconifyWindow");

   procedure Restore_Window (Window : System.Address);
   pragma Import (Convention => C, Entity => Restore_Window,
                  External_Name => "glfwRestoreWindow");

   procedure Show_Window (Window : System.Address);
   pragma Import (Convention => C, Entity => Show_Window,
                  External_Name => "glfwShowWindow");

   procedure Hide_Window (Window : System.Address);
   pragma Import (Convention => C, Entity => Hide_Window,
                  External_Name => "glfwHideWindow");

   function Get_Window_Monitor (Window : System.Address) return System.Address;
   pragma Import (Convention => C, Entity => Get_Window_Monitor,
                  External_Name => "glfwGetWindowMonitor");

   function Get_Window_Attrib (Window : System.Address;
                               Attrib : Enums.Window_Info) return C.int;
   function Get_Window_Attrib (Window : System.Address;
                               Attrib : Enums.Window_Info) return Bool;
   function Get_Window_Attrib (Window : System.Address;
                               Attrib : Enums.Window_Info)
                               return Windows.Context.API_Kind;
   function Get_Window_Attrib (Window : System.Address;
                               Attrib : Enums.Window_Info)
                               return Windows.Context.OpenGL_Profile_Kind;
   function Get_Window_Attrib (Window : System.Address;
                               Attrib : Enums.Window_Info)
                               return Windows.Context.Robustness_Kind;
   pragma Import (Convention => C, Entity => Get_Window_Attrib,
                  External_Name => "glfwGetWindowAttrib");

   procedure Set_Window_User_Pointer (Window  : System.Address;
                                      Pointer : not null access Windows.Window'Class);
   pragma Import (Convention => C, Entity => Set_Window_User_Pointer,
                  External_Name => "glfwSetWindowUserPointer");

   function Get_Window_User_Pointer (Window  : System.Address)
                                     return not null access Windows.Window'Class;
   pragma Import (Convention => C, Entity => Get_Window_User_Pointer,
                  External_Name => "glfwGetWindowUserPointer");

   -- The callback setters in the C header are defined as returning the
   -- previous callback pointer. This is rather low-level and not applicable
   -- in the object-oriented interface of this binding. So we define the setters
   -- as procedures, the return value will just get thrown away.

   procedure Set_Window_Pos_Callback (Window : System.Address;
                                     CB_Fun : Window_Position_Callback);
                                     --return Window_Position_Callback;
   pragma Import (Convention => C, Entity => Set_Window_Pos_Callback,
                  External_Name => "glfwSetWindowPosCallback");

   procedure Set_Window_Size_Callback (Window : System.Address;
                                      CB_Fun : Window_Size_Callback);
                                      --return Window_Size_Callback;
   pragma Import (Convention => C, Entity => Set_Window_Size_Callback,
                  External_Name => "glfwSetWindowSizeCallback");

   procedure Set_Window_Close_Callback (Window : System.Address;
                                       CB_Fun : Window_Close_Callback);
                                       --return Window_Close_Callback;
   pragma Import (Convention => C, Entity => Set_Window_Close_Callback,
                  External_Name => "glfwSetWindowCloseCallback");

   procedure Set_Window_Refresh_Callback (Window : System.Address;
                                         CB_Fun : Window_Refresh_Callback);
                                         --return Window_Refresh_Callback;
   pragma Import (Convention => C, Entity => Set_Window_Refresh_Callback,
                  External_Name => "glfwSetWindowRefreshCallback");

   procedure Set_Window_Focus_Callback (Window : System.Address;
                                       CB_Fun : Window_Focus_Callback);
                                       --return Window_Focus_Callback;
   pragma Import (Convention => C, Entity => Set_Window_Focus_Callback,
                  External_Name => "glfwSetWindowFocusCallback");

   procedure Set_Window_Iconify_Callback (Window : System.Address;
                                         CB_Fun : Window_Iconify_Callback);
                                         --return Window_Iconify_Callback;
   pragma Import (Convention => C, Entity => Set_Window_Iconify_Callback,
                  External_Name => "glfwSetWindowIconifyCallback");

   procedure Set_Framebuffer_Size_Callback (Window : System.Address;
                                           CB_Fun : Framebuffer_Size_Callback);
                                           --return Framebuffer_Size_Callback;
   pragma Import (Convention => C, Entity => Set_Framebuffer_Size_Callback,
                  External_Name => "glfwSetFramebufferSizeCallback");

   -----------------------------------------------------------------------------
   -- Input
   -----------------------------------------------------------------------------

   function Get_Input_Mode (Window : System.Address;
                            Mode : Enums.Input_Toggle) return Bool;
   function Get_Input_Mode (Window : System.Address;
                            Mode : Enums.Input_Toggle)
                            return Input.Mouse.Cursor_Mode;
   pragma Import (Convention => C, Entity => Get_Input_Mode,
                  External_Name => "glfwGetInputMode");

   procedure Set_Input_Mode (Window : System.Address;
                             Mode   : Enums.Input_Toggle;
                             Value  : Bool);
   procedure Set_Input_Mode (Window : System.Address;
                             Mode   : Enums.Input_Toggle;
                             Value  : Input.Mouse.Cursor_Mode);
   pragma Import (Convention => C, Entity => Set_Input_Mode,
                  External_Name => "glfwSetInputMode");

   function Get_Key (Window : System.Address; Key : Input.Keys.Key)
                     return Input.Button_State;
   pragma Import (Convention => C, Entity => Get_Key,
                  External_Name => "glfwGetKey");

   function Get_Mouse_Button (Window : System.Address;
                              Button : Input.Mouse.Button)
                              return Input.Button_State;
   pragma Import (Convention => C, Entity => Get_Mouse_Button,
                  External_Name => "glfwGetMouseButton");

   procedure Get_Cursor_Pos (Window : System.Address;
                             Xpos, Ypos : out Input.Mouse.Coordinate);
   pragma Import (Convention => C, Entity => Get_Cursor_Pos,
                  External_Name => "glfwGetCursorPos");

   procedure Set_Cursor_Pos (Window : System.Address;
                             Xpos, Ypos : Input.Mouse.Coordinate);
   pragma Import (Convention => C, Entity => Set_Cursor_Pos,
                  External_Name => "glfwSetCursorPos");

   procedure Set_Key_Callback (Window : System.Address;
                               CB_Fun : Key_Callback);
                               --return Key_Callback
   pragma Import (Convention => C, Entity => Set_Key_Callback,
                  External_Name => "glfwSetKeyCallback");

   procedure Set_Char_Callback (Window : System.Address;
                                CB_Fun : Character_Callback);
                                --return Character_Callback;
   pragma Import (Convention => C, Entity => Set_Char_Callback,
                  External_Name => "glfwSetCharCallback");

   procedure Set_Mouse_Button_Callback (Window : System.Address;
                                        CB_Fun : Mouse_Button_Callback);
                                        --return Mouse_Button_Callback;
   pragma Import (Convention => C, Entity => Set_Mouse_Button_Callback,
                  External_Name => "glfwSetMouseButtonCallback");

   procedure Set_Cursor_Pos_Callback (Window : System.Address;
                                      CB_Fun : Cursor_Position_Callback);
                                      --return Cursor_Position_Callback;
   pragma Import (Convention => C, Entity => Set_Cursor_Pos_Callback,
                  External_Name => "glfwSetCursorPosCallback");

   procedure Set_Cursor_Enter_Callback (Window : System.Address;
                                        CB_Fun : Cursor_Enter_Callback);
                                        --return Cursor_Enter_Callback;
   pragma Import (Convention => C, Entity => Set_Cursor_Enter_Callback,
                  External_Name => "glfwSetCursorEnterCallback");

   procedure Set_Scroll_Callback (Window : System.Address;
                                  CB_Fun : Scroll_Callback);
                                  --return Scroll_Callback;
   pragma Import (Convention => C, Entity => Set_Scroll_Callback,
                  External_Name => "glfwSetScrollCallback");

   function Joystick_Present (Joy : Enums.Joystick_ID) return Bool;
   pragma Import (Convention => C, Entity => Joystick_Present,
                  External_Name => "glfwJoystickPresent");

   function Get_Joystick_Axes (Joy : Enums.Joystick_ID;
                               Count : access Interfaces.C.int)
                               return Axis_Position_List_Pointers.Pointer;
   pragma Import (Convention => C, Entity => Get_Joystick_Axes,
                  External_Name => "glfwGetJoystickAxes");

   function Get_Joystick_Buttons (Joy : Enums.Joystick_ID;
                                  Count : access Interfaces.C.int)
                                  return Joystick_Button_State_List_Pointers.Pointer;
   pragma Import (Convention => C, Entity => Get_Joystick_Buttons,
                  External_Name => "glfwGetJoystickButtons");

   function Get_Joystick_Name (Joy : Enums.Joystick_ID)
                               return Interfaces.C.Strings.chars_ptr;
   pragma Import (Convention => C, Entity => Get_Joystick_Name,
                  External_Name => "glfwGetJoystickName");

   procedure Poll_Events;
   pragma Import (Convention => C, Entity => Poll_Events,
                  External_Name => "glfwPollEvents");

   procedure Wait_Events;
   pragma Import (Convention => C, Entity => Wait_Events,
                  External_Name => "glfwWaitEvents");

   -----------------------------------------------------------------------------
   -- Context
   -----------------------------------------------------------------------------

   procedure Make_Context_Current (Window : System.Address);
   pragma Import (Convention => C, Entity => Make_Context_Current,
                  External_Name => "glfwMakeContextCurrent");

   function Get_Current_Context return System.Address;
   pragma Import (Convention => C, Entity => Get_Current_Context,
                  External_Name => "glfwGetCurrentContext");

   procedure Swap_Buffers (Window : System.Address);
   pragma Import (Convention => C, Entity => Swap_Buffers,
                  External_Name => "glfwSwapBuffers");

   procedure Swap_Interval (Value : Windows.Context.Swap_Interval);
   pragma Import (Convention => C, Entity => Swap_Interval,
                  External_Name => "glfwSwapInterval");

   -----------------------------------------------------------------------------
   -- Clipboard
   -----------------------------------------------------------------------------

   function Get_Clipboard_String (Window : System.Address)
                                  return Interfaces.C.Strings.chars_ptr;
   pragma Import (Convention => C, Entity => Get_Clipboard_String,
                  External_Name => "glfwGetClipboardString");

   procedure Set_Clipboard_String (Window : System.Address;
                                   Value  : Interfaces.C.char_array);
   pragma Import (Convention => C, Entity => Set_Clipboard_String,
                  External_Name => "glfwSetClipboardString");

end Glfw.API;
