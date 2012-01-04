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

package body Glfw.Events.Keys is

   function Name (Query : Key) return String is
   begin
      case Query is
         when 32 .. 256 => return (1 => Character'Val (Query));

         when Esc => return "Esc";
         when F1  => return "F1";
         when F2  => return "F2";
         when F3  => return "F3";
         when F4  => return "F4";
         when F5  => return "F5";
         when F6  => return "F6";
         when F7  => return "F7";
         when F8  => return "F8";
         when F9  => return "F9";
         when F10 => return "F10";
         when F11 => return "F11";
         when F12 => return "F12";
         when F13 => return "F13";
         when F14 => return "F14";
         when F15 => return "F15";
         when F16 => return "F16";
         when F17 => return "F17";
         when F18 => return "F18";
         when F19 => return "F19";
         when F20 => return "F20";
         when F21 => return "F21";
         when F22 => return "F22";
         when F23 => return "F23";
         when F24 => return "F24";
         when F25 => return "F25";

         when Up        => return "Up";
         when Down      => return "Down";
         when Left      => return "Left";
         when Right     => return "Right";
         when L_Shift   => return "Left Shift";
         when R_Shift   => return "Right Shift";
         when L_Ctrl    => return "Left Ctrl";
         when R_Ctrl    => return "Right Ctrl";
         when L_Alt     => return "Left Alt";
         when R_Alt     => return "Right Alt";
         when Tab       => return "Tab";
         when Enter     => return "Enter";
         when Backspace => return "Backspace";
         when Insert    => return "Insert";
         when Del       => return "Delete";
         when Page_Up   => return "Page Up";
         when Page_Down => return "Page Down";
         when Home      => return "Home";
         when End_Key   => return "End";

         when KP_0 => return "0 (Numpad)";
         when KP_1 => return "1 (Numpad)";
         when KP_2 => return "2 (Numpad)";
         when KP_3 => return "3 (Numpad)";
         when KP_4 => return "4 (Numpad)";
         when KP_5 => return "5 (Numpad)";
         when KP_6 => return "6 (Numpad)";
         when KP_7 => return "7 (Numpad)";
         when KP_8 => return "8 (Numpad)";
         when KP_9 => return "9 (Numpad)";

         when KP_Divide    => return "/ (Numpad)";
         when KP_Multiply  => return "* (Numpad)";
         when KP_Substract => return "- (Numpad)";
         when KP_Add       => return "+ (Numpad)";
         when KP_Decimal   => return ". (Numpad)";
         when KP_Equal     => return "= (Numpad)";
         when KP_Enter     => return "Enter (Numpad)";
         when KP_Num_Lock  => return "Numlock";

         when Caps_Lock   => return "Caps Lock";
         when Scroll_Lock => return "Scroll Lock";
         when Pause       => return "Pause";
         when L_Super     => return "Left Super";
         when R_Super     => return "Right Super";
         when Menu        => return "Menu";
      end case;
   end Name;

end Glfw.Events.Keys;
