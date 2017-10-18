--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.API;
with Glfw.Enums;
with Interfaces.C.Strings;

package body Glfw.Display is

   -----------------------------------------------------------------------------
   --                        Display operations                               --
   -----------------------------------------------------------------------------

   procedure Open (Width        : Natural := 0; -- 0 = 4/3 Height, or 640
                   Height       : Natural := 0; -- 0 = 3/4 Width, or 480
                   Red_Bits     : Natural := 0; -- 0 = default color depth
                   Green_Bits   : Natural := 0; -- 0 = default color depth
                   Blue_Bits    : Natural := 0; -- 0 = default color depth
                   Alpha_Bits   : Natural := 0; -- 0 = no alpha channel
                   Depth_Bits   : Natural := 0; -- 0 = no depth buffer
                   Stencil_Bits : Natural := 0; -- 0 = no stencil buffer
                   Mode         : Display_Mode) is
   begin
      if API.Open_Window (C.int (Width), C.int (Height), C.int (Red_Bits),
                          C.int (Green_Bits), C.int (Blue_Bits), C.int (Alpha_Bits),
                          C.int (Depth_Bits), C.int (Stencil_Bits), Mode)
        = False then
         raise Display_Exception;
      end if;
   end Open;

   procedure Close is
   begin
      API.Close_Window;
   end Close;

   procedure Iconify is
   begin
      API.Iconify_Window;
   end Iconify;

   procedure Restore is
   begin
      API.Restore_Window;
   end Restore;

   procedure Swap_Buffers is
   begin
      API.Swap_Buffers;
   end Swap_Buffers;

   -----------------------------------------------------------------------------
   --                        Display Hints:                                   --
   --          These values will be applied at the next call to Open.         --
   -----------------------------------------------------------------------------

   procedure Hint_Refresh_Rate            (Value : Natural) is
   begin
      API.Open_Window_Hint (Enums.Refresh_Rate, C.int (Value));
   end Hint_Refresh_Rate;

   procedure Hint_Accum_Buffer_Red_Bits   (Value : Natural) is
   begin
      API.Open_Window_Hint (Enums.Accum_Red_Bits, C.int (Value));
   end Hint_Accum_Buffer_Red_Bits;

   procedure Hint_Accum_Buffer_Green_Bits (Value : Natural) is
   begin
      API.Open_Window_Hint (Enums.Accum_Green_Bits, C.int (Value));
   end Hint_Accum_Buffer_Green_Bits;

   procedure Hint_Accum_Buffer_Blue_Bits  (Value : Natural) is
   begin
      API.Open_Window_Hint (Enums.Accum_Blue_Bits, C.int (Value));
   end Hint_Accum_Buffer_Blue_Bits;

   procedure Hint_Accum_Buffer_Alpha_Bits (Value : Natural) is
   begin
      API.Open_Window_Hint (Enums.Accum_Alpha_Bits, C.int (Value));
   end Hint_Accum_Buffer_Alpha_Bits;

   procedure Hint_Auxiliary_Buffer_Count  (Value : Natural) is
   begin
      API.Open_Window_Hint (Enums.Aux_Buffers, C.int (Value));
   end Hint_Auxiliary_Buffer_Count;

   procedure Hint_Stereo_Rendering        (Value : Boolean) is
   begin
      API.Open_Window_Hint (Enums.Stereo, Bool (Value));
   end Hint_Stereo_Rendering;

   procedure Hint_Resizable               (Value : Boolean) is
   begin
      API.Open_Window_Hint (Enums.Window_No_Resize, Bool (not Value));
   end Hint_Resizable;

   procedure Hint_FSAA_Sample_Count       (Value : Natural) is
   begin
      API.Open_Window_Hint (Enums.Fsaa_Samples, C.int (Value));
   end Hint_FSAA_Sample_Count;

   procedure Hint_Minimum_OpenGL_Version  (Major, Minor : Natural) is
   begin
      API.Open_Window_Hint (Enums.OpenGL_Version_Major, C.int (Major));
      API.Open_Window_Hint (Enums.OpenGL_Version_Minor, C.int (Minor));
   end Hint_Minimum_OpenGL_Version;

   procedure Hint_Forward_Compatible      (Value : Boolean) is
   begin
      API.Open_Window_Hint (Enums.OpenGL_Forward_Compat, Bool (Value));
   end Hint_Forward_Compatible;

   procedure Hint_Debug_Context           (Value : Boolean) is
   begin
      API.Open_Window_Hint (Enums.OpenGL_Debug_Context, Bool (Value));
   end Hint_Debug_Context;

   procedure Hint_OpenGL_Profile          (Value : OpenGL_Profile_Kind) is
   begin
      API.Open_Window_Hint (Enums.OpenGL_Profile, Value);
   end Hint_OpenGL_Profile;

   -----------------------------------------------------------------------------
   --                         Raw callback handlers                           --
   -----------------------------------------------------------------------------

   procedure Raw_Size_Callback (Width, Height : C.int);
   function Raw_Close_Callback return Bool;
   procedure Raw_Refresh_Callback;

   pragma Convention (C, Raw_Size_Callback);
   pragma Convention (C, Raw_Close_Callback);
   pragma Convention (C, Raw_Refresh_Callback);

   User_Size_Callback    : Size_Callback    := null;
   User_Close_Callback   : Close_Callback   := null;
   User_Refresh_Callback : Refresh_Callback := null;

   procedure Raw_Size_Callback (Width, Height : C.int) is
   begin
      if (User_Size_Callback /= null) then
         User_Size_Callback (Natural (Width), Natural (Height));
      end if;
   end Raw_Size_Callback;

   function Raw_Close_Callback return Bool is
   begin
      if (User_Close_Callback /= null) then
         return Bool (User_Close_Callback.all);
      else
         return True;
      end if;
   end Raw_Close_Callback;

   procedure Raw_Refresh_Callback is
   begin
      if (User_Refresh_Callback /= null) then
         User_Refresh_Callback.all;
      end if;
   end Raw_Refresh_Callback;

   -----------------------------------------------------------------------------
   --        Display property setters, will be applied immediately.           --
   -----------------------------------------------------------------------------

   procedure Set_Title          (Title : String) is
   begin
      API.Set_Window_Title (C.Strings.New_String (Title));
   end Set_Title;

   procedure Set_Size           (Width, Height : Natural) is
   begin
      API.Set_Window_Size (C.int (Width), C.int (Height));
   end Set_Size;

   procedure Set_Position       (X, Y : Natural) is
   begin
      API.Set_Window_Pos (C.int (X), C.int (Y));
   end Set_Position;

   procedure Set_Swap_Interval  (Interval : Natural) is
   begin
      API.Swap_Interval (C.int (Interval));
   end Set_Swap_Interval;

   procedure Set_Size_Callback    (Callback : Size_Callback) is
   begin
      User_Size_Callback := Callback;
      if (Callback /= null) then
         API.Set_Window_Size_Callback (Raw_Size_Callback'Access);
      else
         API.Set_Window_Size_Callback (null);
      end if;
   end Set_Size_Callback;

   procedure Set_Close_Callback   (Callback : Close_Callback) is
   begin
      User_Close_Callback := Callback;
      if (Callback /= null) then
         API.Set_Window_Close_Callback (Raw_Close_Callback'Access);
      else
         API.Set_Window_Close_Callback (null);
      end if;
   end Set_Close_Callback;

   procedure Set_Refresh_Callback (Callback : Refresh_Callback) is
   begin
      User_Refresh_Callback := Callback;
      if (Callback /= null) then
         API.Set_Window_Refresh_Callback (Raw_Refresh_Callback'Access);
      else
         API.Set_Window_Refresh_Callback (null);
      end if;
   end Set_Refresh_Callback;

   -----------------------------------------------------------------------------
   --                      Display property getters                           --
   -----------------------------------------------------------------------------

   function Width  return Natural is
      Width, Height : C.int;
   begin
      API.Get_Window_Size (Width, Height);
      return Natural (Width);
   end Width;

   function Height return Natural is
      Width, Height : C.int;
   begin
      API.Get_Window_Size (Width, Height);
      return Natural (Height);
   end Height;

   function Opened      return Boolean is
      Result : constant Bool := API.Get_Window_Param (Enums.Opened);
   begin
      return Boolean (Result);
   end Opened;

   function Active      return Boolean is
      Result : constant Bool := API.Get_Window_Param (Enums.Active);
   begin
      return Boolean (Result);
   end Active;

   function Iconified   return Boolean is
      Result : constant Bool := API.Get_Window_Param (Enums.Iconified);
   begin
      return Boolean (Result);
   end Iconified;

   function Accelerated return Boolean is
      Result : constant Bool := API.Get_Window_Param (Enums.Accelerated);
   begin
      return Boolean (Result);
   end Accelerated;

   function Red_Bits     return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Red_Bits);
   begin
      return Natural (Result);
   end Red_Bits;

   function Green_Bits   return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Green_Bits);
   begin
      return Natural (Result);
   end Green_Bits;

   function Blue_Bits    return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Blue_Bits);
   begin
      return Natural (Result);
   end Blue_Bits;

   function Alpha_Bits   return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Alpha_Bits);
   begin
      return Natural (Result);
   end Alpha_Bits;

   function Depth_Bits   return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Depth_Bits);
   begin
      return Natural (Result);
   end Depth_Bits;

   function Stencil_Bits return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Stencil_Bits);
   begin
      return Natural (Result);
   end Stencil_Bits;

   function Refresh_Rate return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Refresh_Rate);
   begin
      return Natural (Result);
   end Refresh_Rate;

   function Accum_Red_Bits   return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Accum_Red_Bits);
   begin
      return Natural (Result);
   end Accum_Red_Bits;

   function Accum_Green_Bits return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Accum_Green_Bits);
   begin
      return Natural (Result);
   end Accum_Green_Bits;

   function Accum_Blue_Bits  return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Accum_Blue_Bits);
   begin
      return Natural (Result);
   end Accum_Blue_Bits;

   function Accum_Alpha_Bits return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Accum_Alpha_Bits);
   begin
      return Natural (Result);
   end Accum_Alpha_Bits;

   function Auxiliary_Buffer_Count return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Aux_Buffers);
   begin
      return Natural (Result);
   end Auxiliary_Buffer_Count;

   function Stereo_Rendering_Supported return Boolean is
      Result : constant Bool := API.Get_Window_Param (Enums.Stereo);
   begin
      return Boolean (Result);
   end Stereo_Rendering_Supported;

   function Resizable return Boolean is
      Result : constant Bool := API.Get_Window_Param (Enums.Window_No_Resize);
   begin
      return not Boolean (Result);
   end Resizable;

   function FSAA_Sample_Count return Natural is
      Result : constant C.int := API.Get_Window_Param (Enums.Fsaa_Samples);
   begin
      return Natural (Result);
   end FSAA_Sample_Count;

   function OpenGL_Version_Major return Natural is
      Result : constant C.int :=
        API.Get_Window_Param (Enums.OpenGL_Version_Major);
   begin
      return Natural (Result);
   end OpenGL_Version_Major;

   function OpenGL_Version_Minor return Natural is
      Result : constant C.int
        := API.Get_Window_Param (Enums.OpenGL_Version_Minor);
   begin
      return Natural (Result);
   end OpenGL_Version_Minor;

   function OpenGL_Forward_Compatible return Boolean is
      Result : constant Bool
        := API.Get_Window_Param (Enums.OpenGL_Forward_Compat);
   begin
      return Boolean (Result);
   end OpenGL_Forward_Compatible;

   function OpenGL_Debug_Context return Boolean is
      Result : constant Bool
        := API.Get_Window_Param (Enums.OpenGL_Debug_Context);
   begin
      return Boolean (Result);
   end OpenGL_Debug_Context;

   function OpenGL_Profile return OpenGL_Profile_Kind is
   begin
      return API.Get_Window_Param (Enums.OpenGL_Profile);
   end OpenGL_Profile;

end Glfw.Display;
