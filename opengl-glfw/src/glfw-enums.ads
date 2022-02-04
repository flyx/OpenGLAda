--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C;

private package Glfw.Enums is

   type Window_Info is
     (Focused, Iconified, Resizable, Visible, Decorated, Auto_Iconify, Floating,
      Maximized, Center_Cursor, Transparent_Framebuffer, Hovered, Focus_On_Show,
      Red_Bits, Green_Bits, Blue_Bits, Alpha_Bits, Depth_Bits, Stencil_Bits,
      Accum_Red_Bits, Accum_Green_Bits, Accum_Blue_Bits, Accum_Alpha_Bits,
      Aux_Buffers, Stereo, Samples, SRGB_Capable, Refresh_Rate, Doublebuffer,
      Client_API, Context_Version_Major, Context_Version_Minor,
      Context_Revision, Context_Robustness, OpenGL_Forward_Compat,
      OpenGL_Debug_Context, OpenGL_Profile, Scale_To_Monitor);
   for Window_Info use (Focused       => 16#20001#,
                        Iconified     => 16#20002#,
                        Resizable     => 16#20003#,
                        Visible       => 16#20004#,
                        Decorated     => 16#20005#,
                        Auto_Iconify  => 16#20006#,
                        Floating      => 16#20007#,
                        Maximized     => 16#20008#,
                        Center_Cursor => 16#20009#,
                        Transparent_Framebuffer => 16#2000A#,
                        Hovered       => 16#2000B#,
                        Focus_On_Show => 16#2000C#,

                        Red_Bits      => 16#21001#,
                        Green_Bits    => 16#21002#,
                        Blue_Bits     => 16#21003#,
                        Alpha_Bits    => 16#21004#,
                        Depth_Bits    => 16#21005#,
                        Stencil_Bits  => 16#21006#,

                        Accum_Red_Bits   => 16#21007#,
                        Accum_Green_Bits => 16#21008#,
                        Accum_Blue_Bits  => 16#21009#,
                        Accum_Alpha_Bits => 16#2100A#,
                        Aux_Buffers      => 16#2100B#,
                        Stereo           => 16#2100C#,
                        Samples          => 16#2100D#,
                        SRGB_Capable     => 16#2100E#,
                        Refresh_Rate     => 16#2100F#,
                        Doublebuffer     => 16#21010#,

                        Client_API             => 16#22001#,
                        Context_Version_Major  => 16#22002#,
                        Context_Version_Minor  => 16#22003#,
                        Context_Revision       => 16#22004#,
                        Context_Robustness     => 16#22005#,
                        OpenGL_Forward_Compat  => 16#22006#,
                        OpenGL_Debug_Context   => 16#22007#,
                        OpenGL_Profile         => 16#22008#,
                        Scale_To_Monitor       => 16#2200C#);
   for Window_Info'Size use Interfaces.C.int'Size;

   type Cursor_Input_Toggle is (Mouse_Cursor);
   for Cursor_Input_Toggle use (Mouse_Cursor   => 16#33001#);
   for Cursor_Input_Toggle'Size use Interfaces.C.int'Size;

   type Bool_Input_Toggle is (Raw_Mouse_Motion);
   for Bool_Input_Toggle use (Raw_Mouse_Motion => 16#33005#);
   for Bool_Input_Toggle'Size use Interfaces.C.int'Size;

   type Joystick_ID is (
      Joystick_1, Joystick_2, Joystick_3, Joystick_4, Joystick_5,
      Joystick_6, Joystick_7, Joystick_8, Joystick_9, Joystick_10,
      Joystick_11, Joystick_12, Joystick_13, Joystick_14,
      Joystick_15, Joystick_16);
   for Joystick_ID use (
      Joystick_1  =>  0, Joystick_2  =>  1, Joystick_3  =>  2,
      Joystick_4  =>  3, Joystick_5  =>  4, Joystick_6  =>  5,
      Joystick_7  =>  6, Joystick_8  =>  7, Joystick_9  =>  8,
      Joystick_10 =>  9, Joystick_11 => 10, Joystick_12 => 11,
      Joystick_13 => 12, Joystick_14 => 13, Joystick_15 => 14,
      Joystick_16 => 15
   );
   for Joystick_ID'Size use Interfaces.C.int'Size;

   type Joystick_Param is (Present, Axis, Buttons);
   for Joystick_Param use (Present => 16#50001#,
                           Axis    => 16#50002#,
                           Buttons => 16#50003#);
   for Joystick_Param'Size use C.int'Size;

end Glfw.Enums;
