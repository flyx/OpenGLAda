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

with Glfw.Enums;
with Glfw.Display;

with Glfw.Events.Keys;
with Glfw.Events.Mouse;
with Glfw.Events.Joysticks;

with Interfaces.C.Strings;
with System;

private package Glfw.Api is

   type Raw_Video_Mode is array (1 .. 5) of aliased C.int;
   pragma Convention (C, Raw_Video_Mode);

   type Video_Mode_List is array (Positive range <>) of Raw_Video_Mode;
   pragma Convention (C, Video_Mode_List);
   
   type Window_Size_Callback is access procedure (Width, Height : C.int);
   type Window_Close_Callback is access function return Bool;
   type Window_Refresh_Callback is access procedure;

   type Key_Callback is access procedure (Subject : Events.Keys.Key;
                                          Action : Events.Button_State);
   type Character_Callback is access procedure
     (Unicode_Char : Events.Keys.Unicode_Character; Action : Events.Button_State);
   type Button_Callback is access procedure (Subject : Events.Mouse.Button;
                                             Action  : Events.Button_State);
   type Position_Callback is access procedure (X, Y : Events.Mouse.Coordinate);
   type Wheel_Callback is access procedure (Pos : Events.Mouse.Wheel_Position);

   pragma Convention (C, Window_Size_Callback);
   pragma Convention (C, Window_Close_Callback);
   pragma Convention (C, Window_Refresh_Callback);
   pragma Convention (C, Key_Callback);
   pragma Convention (C, Character_Callback);
   pragma Convention (C, Button_Callback);
   pragma Convention (C, Position_Callback);
   pragma Convention (C, Wheel_Callback);

   function Init return C.int;
   pragma Import (Convention => C, Entity => Init,
                  External_Name => "glfwInit");

   procedure Glfw_Terminate;
   pragma Import (Convention => C, Entity => Glfw_Terminate,
                  External_Name => "glfwTerminate");

   procedure Get_Version (Major, Minor, Revision : out C.int);
   pragma Import (Convention => C, Entity => Get_Version,
                  External_Name => "glfwGetVersion");

   function Open_Window (Width, Height, Red_Bits, Green_Bits, Blue_Bits,
                         Alpha_Bits, Depth_Bits, Stencil_Bits : C.int;
                         Mode : Display.Display_Mode) return Bool;
   pragma Import (Convention => C, Entity => Open_Window,
                  External_Name => "glfwOpenWindow");

   procedure Open_Window_Hint (Target : Glfw.Enums.Window_Hint; Info : C.int);
   procedure Open_Window_Hint (Target : Glfw.Enums.Window_Hint; Info : Bool);
   procedure Open_Window_Hint (Target : Glfw.Enums.Window_Hint;
                               Info : Display.OpenGL_Profile_Kind);
   pragma Import (Convention => C, Entity => Open_Window_Hint,
                  External_Name => "glfwOpenWindowHint");

   procedure Close_Window;
   pragma Import (Convention => C, Entity => Close_Window,
                  External_Name => "glfwCloseWindow");

   procedure Set_Window_Title (Title : C.Strings.chars_ptr);
   pragma Import (Convention => C, Entity => Set_Window_Title,
                  External_Name => "glfwSetWindowTitle");

   procedure Get_Window_Size (Width, Height : out C.int);
   pragma Import (Convention => C, Entity => Get_Window_Size,
                  External_Name => "glfwGetWindowSize");

   procedure Set_Window_Size (Width, Height : C.int);
   pragma Import (Convention => C, Entity => Set_Window_Size,
                  External_Name => "glfwSetWindowSize");

   procedure Set_Window_Pos (X, Y : C.int);
   pragma Import (Convention => C, Entity => Set_Window_Pos,
                  External_Name => "glfwSetWindowPos");

   procedure Iconify_Window;
   pragma Import (Convention => C, Entity => Iconify_Window,
                  External_Name => "glfwIconifyWindow");

   procedure Restore_Window;
   pragma Import (Convention => C, Entity => Restore_Window,
                  External_Name => "glfwRestoreWindow");

   procedure Swap_Buffers;
   pragma Import (Convention => C, Entity => Swap_Buffers,
                  External_Name => "glfwSwapBuffers");

   procedure Swap_Interval (Interval : C.int);
   pragma Import (Convention => C, Entity => Swap_Interval,
                  External_Name => "glfwSwapInterval");

   function Get_Window_Param (Param : Enums.Window_Info) return C.int;
   function Get_Window_Param (Param : Enums.Window_Info) return Bool;
   function Get_Window_Param (Param : Enums.Window_Info) return Display.OpenGL_Profile_Kind;
   pragma Import (Convention => C, Entity => Get_Window_Param,
                  External_Name => "glfwGetWindowParam");

   procedure Set_Window_Size_Callback (Cb : Window_Size_Callback);
   pragma Import (Convention => C, Entity => Set_Window_Size_Callback,
                  External_Name => "glfwSetWindowSizeCallback");

   procedure Set_Window_Close_Callback (Cb : Window_Close_Callback);
   pragma Import (Convention => C, Entity => Set_Window_Close_Callback,
                  External_Name => "glfwSetWindowCloseCallback");

   procedure Set_Window_Refresh_Callback (Cb : Window_Refresh_Callback);
   pragma Import (Convention => C, Entity => Set_Window_Refresh_Callback,
                  External_Name => "glfwSetWindowCloseCallback");

   function Get_Video_Modes (List : System.Address;
                             Max_Count : C.int) return C.int;
   pragma Import (Convention => C, Entity => Get_Video_Modes,
                  External_Name => "glfwGetVideoModes");

   procedure Get_Desktop_Mode (Mode : access C.int);
   pragma Import (Convention => C, Entity => Get_Desktop_Mode,
                  External_Name => "glfwGetDesktopMode");

   procedure Poll_Events;
   pragma Import (Convention => C, Entity => Poll_Events,
                  External_Name => "glfwPollEvents");

   procedure Wait_Events;
   pragma Import (Convention => C, Entity => Wait_Events,
                  External_Name => "glfwWaitEvents");

   function Get_Key (Key : Glfw.Events.Keys.Key) return Glfw.Events.Button_State;
   pragma Import (Convention => C, Entity => Get_Key,
                  External_Name => "glfwGetKey");

   function Get_Mouse_Button (Button : Glfw.Events.Mouse.Button)
                              return Glfw.Events.Button_State;
   pragma Import (Convention => C, Entity => Get_Mouse_Button,
                  External_Name => "glfwGetMouseButton");

   procedure Get_Mouse_Pos (XPos, YPos : out C.int);
   pragma Import (Convention => C, Entity => Get_Mouse_Pos,
                  External_Name => "glfwGetMousePos");

   procedure Set_Mouse_Pos (XPos, YPos : C.int);
   pragma Import (Convention => C, Entity => Set_Mouse_Pos,
                  External_Name => "glfwSetMousePos");

   function Get_Mouse_Wheel return Events.Mouse.Wheel_Position;
   pragma Import (Convention => C, Entity => Get_Mouse_Wheel,
                  External_Name => "glfwGetMouseWheel");

   procedure Set_Mouse_Wheel (Position : C.int);
   pragma Import (Convention => C, Entity => Set_Mouse_Wheel,
                  External_Name => "glfwSetMouseWheel");

   procedure Set_Key_Callback (CbFun : Key_Callback);
   pragma Import (Convention => C, Entity => Set_Key_Callback,
                  External_Name => "glfwSetKeyCallback");

   procedure Set_Char_Callback (CbFun : Character_Callback);
   pragma Import (Convention => C, Entity => Set_Char_Callback,
                  External_Name => "glfwSetCharCallback");

   procedure Set_Mouse_Button_Callback (CbFun : Button_Callback);
   pragma Import (Convention => C, Entity => Set_Mouse_Button_Callback,
                  External_Name => "glfwSetMouseButtonCallback");

   procedure Set_Mouse_Pos_Callback (CbFun : Position_Callback);
   pragma Import (Convention => C, Entity => Set_Mouse_Pos_Callback,
                  External_Name => "glfwSetMousePosCallback");

   procedure Set_Mouse_Wheel_Callback (CbFun : Wheel_Callback);
   pragma Import (Convention => C, Entity => Set_Mouse_Wheel_Callback,
                  External_Name => "glfwSetMouseWheelCallback");
   
   function Get_Joystick_Param (Joy   : Enums.Joystick_ID;
                                Param : Enums.Joystick_Param)
                               return Interfaces.C.int;
   pragma Import (Convention => C, Entity => Get_Joystick_Param,
                  External_Name => "glfwGetJoystickParam");
   
   -- Pos will be modified on the C side. It should be declared
   -- as 'in out' but Ada 2005 forbids that. Since is works fine this
   -- way, we don't bother using C pointers instead that wouldn't
   -- make the API any clearer.
   function Get_Joystick_Pos (Joy : Enums.Joystick_ID;
                              Pos : Events.Joysticks.Axis_Positions;
                              Num_Axis : Interfaces.C.int)
                             return Interfaces.C.int;
   pragma Import (Convention => C, Entity => Get_Joystick_Pos,
                  External_Name => "glfwGetJoystickPos");
   
   -- See comment on Get_Joystick_Pos
   function Get_Joystick_Buttons (Joy : Enums.Joystick_ID;
                                  Pos : Events.Button_States;
                                  Num_Buttons : Interfaces.C.int)
                                 return Interfaces.C.int;
   pragma Import (Convention => C, Entity => Get_Joystick_Buttons,
                  External_Name => "glfwGetJoystickButtons");

   function Get_Time return C.double;
   pragma Import (Convention => C, Entity => Get_Time,
                  External_Name => "glfwGetTime");

   procedure Set_Time (Value : C.double);
   pragma Import (Convention => C, Entity => Set_Time,
                  External_Name => "glfwSetTime");

   procedure Sleep (Time : C.double);
   pragma Import (Convention => C, Entity => Sleep,
                  External_Name => "glfwSleep");

   function Extension_Supported (Name : C.Strings.chars_ptr) return Bool;
   pragma Import (Convention => C, Entity => Extension_Supported,
                  External_Name => "glfwExtensionSupported");

   procedure Get_GL_Version (Major, Minor, Rev : out C.int);
   pragma Import (Convention => C, Entity => Get_GL_Version,
                  External_Name => "glfwGetGLVersion");

   procedure Enable (Target : Enums.Feature);
   pragma Import (Convention => C, Entity => Enable,
                  External_Name => "glfwEnable");

   procedure Disable (Target : Enums.Feature);
   pragma Import (Convention => C, Entity => Disable,
                  External_Name => "glfwDisable");

end Glfw.Api;
