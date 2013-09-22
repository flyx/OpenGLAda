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

   type Window_Info is (Opened, Active, Iconified, Accelerated, Red_Bits,
                        Green_Bits, Blue_Bits, Alpha_Bits, Depth_Bits,
                        Stencil_Bits, Refresh_Rate, Accum_Red_Bits,
                        Accum_Green_Bits, Accum_Blue_Bits, Accum_Alpha_Bits,
                        Aux_Buffers, Stereo, Window_No_Resize, Fsaa_Samples,
                        OpenGL_Version_Major, OpenGL_Version_Minor,
                        OpenGL_Forward_Compat, OpenGL_Debug_Context,
                        OpenGL_Profile);
   for Window_Info use (Opened       => 16#20001#,
                        Active       => 16#20002#,
                        Iconified    => 16#20003#,
                        Accelerated  => 16#20004#,
                        Red_Bits     => 16#20005#,
                        Green_Bits   => 16#20006#,
                        Blue_Bits    => 16#20007#,
                        Alpha_Bits   => 16#20008#,
                        Depth_Bits   => 16#20009#,
                        Stencil_Bits => 16#2000A#,

                        Refresh_Rate     => 16#2000B#,
                        Accum_Red_Bits   => 16#2000C#,
                        Accum_Green_Bits => 16#2000D#,
                        Accum_Blue_Bits  => 16#2000E#,
                        Accum_Alpha_Bits => 16#2000F#,
                        Aux_Buffers      => 16#20010#,
                        Stereo           => 16#20011#,
                        Window_No_Resize => 16#20012#,
                        Fsaa_Samples     => 16#20013#,

                        OpenGL_Version_Major  => 16#20014#,
                        OpenGL_Version_Minor  => 16#20015#,
                        OpenGL_Forward_Compat => 16#20016#,
                        OpenGL_Debug_Context  => 16#20017#,
                        OpenGL_Profile        => 16#20018#);
   for Window_Info'Size use Interfaces.C.int'Size;

   subtype Window_Hint is Window_Info range Refresh_Rate .. OpenGL_Profile;

   type Feature is (Mouse_Cursor, Sticky_Keys, Sticky_Mouse_Buttons, System_Keys,
                    Key_Repeat, Auto_Poll_Events);
   for Feature use (Mouse_Cursor         => 16#30001#,
                    Sticky_Keys          => 16#30002#,
                    Sticky_Mouse_Buttons => 16#30003#,
                    System_Keys          => 16#30004#,
                    Key_Repeat           => 16#30005#,
                    Auto_Poll_Events     => 16#30006#);
   for Feature'Size use Interfaces.C.int'Size;
   
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
