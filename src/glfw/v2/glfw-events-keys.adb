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

with Glfw.Api;
with Glfw.Enums;

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

   function Pressed (Query : Key) return Boolean is
   begin
      return Api.Get_Key (Query) = Press;
   end Pressed;

   procedure Raw_Key_Callback (Subject : Key; Action : Button_State);
   procedure Raw_Character_Callback (Unicode_Char : Unicode_Character;
                                     Action : Glfw.Events.Button_State);
   pragma Convention (C, Raw_Key_Callback);
   pragma Convention (C, Raw_Character_Callback);

   User_Key_Callback       : Key_Callback       := null;
   User_Character_Callback : Character_Callback := null;

   procedure Raw_Key_Callback (Subject : Key; Action : Button_State) is
   begin
      if User_Key_Callback /= null then
         User_Key_Callback (Subject, Action);
      end if;
   end Raw_Key_Callback;

   procedure Raw_Character_Callback (Unicode_Char : Unicode_Character;
                                     Action : Glfw.Events.Button_State) is
   begin
      if User_Character_Callback /= null then
         User_Character_Callback (Unicode_Char, Action);
      end if;
   end Raw_Character_Callback;

   procedure Set_Key_Callback (Callback : Key_Callback) is
   begin
      User_Key_Callback := Callback;
      if Callback /= null then
         Api.Set_Key_Callback (Raw_Key_Callback'Access);
      else
         Api.Set_Key_Callback (null);
      end if;
   end Set_Key_Callback;

   procedure Set_Character_Callback (Callback : Character_Callback) is
   begin
      User_Character_Callback := Callback;
      if Callback /= null then
         Api.Set_Char_Callback (Raw_Character_Callback'Access);
      else
         Api.Set_Char_Callback (null);
      end if;
   end Set_Character_Callback;

   procedure Toggle_Key_Repeat  (Enable  : Boolean) is
   begin
      if Enable then
         Api.Enable (Enums.Key_Repeat);
      else
         Api.Disable (Enums.Key_Repeat);
      end if;
   end Toggle_Key_Repeat;

   procedure Toggle_Sticky_Keys (Enable  : Boolean) is
   begin
      if Enable then
         Api.Enable (Enums.Sticky_Keys);
      else
         Api.Disable (Enums.Sticky_Keys);
      end if;
   end Toggle_Sticky_Keys;

   procedure Toggle_System_Keys (Enable  : Boolean) is
   begin
      if Enable then
         Api.Enable (Enums.System_Keys);
      else
         Api.Disable (Enums.System_Keys);
      end if;
   end Toggle_System_Keys;

end Glfw.Events.Keys;
