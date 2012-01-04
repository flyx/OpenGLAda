--------------------------------------------------------------------------------
--  Copyright (c) 2011, Felix Krause
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED.
--  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
--  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY OUT OF THE USE OF
--  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

end Glfw.Enums;
