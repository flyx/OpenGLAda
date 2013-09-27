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

with Interfaces.C;

private package Glfw.Enums is

   type Window_Info is (Focused, Iconified, Resizable, Visible, Decorated,
                        Red_Bits, Green_Bits, Blue_Bits, Alpha_Bits, Depth_Bits,
                        Stencil_Bits, Accum_Red_Bits,
                        Accum_Green_Bits, Accum_Blue_Bits, Accum_Alpha_Bits,
                        Aux_Buffers, Stereo, Samples, SRGB_Capable,
                        Refresh_Rate, Client_API, Context_Version_Major,
                        Context_Version_Minor, Context_Revision,
                        Context_Robustness, OpenGL_Forward_Compat,
                        OpenGL_Debug_Context, OpenGL_Profile);
   for Window_Info use (Focused      => 16#20001#,
                        Iconified    => 16#20002#,
                        Resizable    => 16#20003#,
                        Visible      => 16#20004#,
                        Decorated    => 16#20005#,
                        Red_Bits     => 16#21001#,
                        Green_Bits   => 16#21002#,
                        Blue_Bits    => 16#21003#,
                        Alpha_Bits   => 16#21004#,
                        Depth_Bits   => 16#21005#,
                        Stencil_Bits => 16#21006#,

                        Accum_Red_Bits   => 16#21007#,
                        Accum_Green_Bits => 16#21008#,
                        Accum_Blue_Bits  => 16#21009#,
                        Accum_Alpha_Bits => 16#2100A#,
                        Aux_Buffers      => 16#2100B#,
                        Stereo           => 16#2100C#,
                        Samples          => 16#2100D#,
                        SRGB_Capable     => 16#2100E#,
                        Refresh_Rate     => 16#2100F#,
                        
                        Client_API             => 16#22001#,
                        Context_Version_Major  => 16#22002#,
                        Context_Version_Minor  => 16#22003#,
                        Context_Revision       => 16#22004#,
                        Context_Robustness     => 16#22005#,
                        OpenGL_Forward_Compat  => 16#22006#,
                        OpenGL_Debug_Context   => 16#22007#,
                        OpenGL_Profile         => 16#22008#);
   for Window_Info'Size use Interfaces.C.int'Size;

   subtype Window_Hint is Window_Info range Resizable .. OpenGL_Profile;

   type Input_Toggle is (Mouse_Cursor, Sticky_Keys, Sticky_Mouse_Buttons);
   for Input_Toggle use (Mouse_Cursor         => 16#33001#,
                         Sticky_Keys          => 16#33002#,
                         Sticky_Mouse_Buttons => 16#33003#);
   for Input_Toggle'Size use Interfaces.C.int'Size;
   
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
