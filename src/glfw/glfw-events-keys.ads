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

package Glfw.Events.Keys is

   -- Not all values are actually used.
   -- For non-special keys, the ASCII capital representation is used as value.
   -- For special keys, the values defined as constants below are used.
   type Key is range 32 .. 325;

   Esc : constant := 257;
   F1  : constant := 258;
   F2  : constant := 259;
   F3  : constant := 260;
   F4  : constant := 261;
   F5  : constant := 262;
   F6  : constant := 263;
   F7  : constant := 264;
   F8  : constant := 265;
   F9  : constant := 266;
   F10 : constant := 267;
   F11 : constant := 268;
   F12 : constant := 269;
   F13 : constant := 270;
   F14 : constant := 271;
   F15 : constant := 272;
   F16 : constant := 273;
   F17 : constant := 274;
   F18 : constant := 275;
   F19 : constant := 276;
   F20 : constant := 277;
   F21 : constant := 278;
   F22 : constant := 279;
   F23 : constant := 280;
   F24 : constant := 281;
   F25 : constant := 282;

   Up        : constant := 283;
   Down      : constant := 284;
   Left      : constant := 285;
   Right     : constant := 286;
   L_Shift   : constant := 287;
   R_Shift   : constant := 288;
   L_Ctrl    : constant := 289;
   R_Ctrl    : constant := 290;
   L_Alt     : constant := 291;
   R_Alt     : constant := 292;
   Tab       : constant := 293;
   Enter     : constant := 294;
   Backspace : constant := 295;
   Insert    : constant := 296;
   Del       : constant := 297;
   Page_Up   : constant := 298;
   Page_Down : constant := 299;
   Home      : constant := 300;
   End_Key   : constant := 301;

   KP_0 : constant := 302;
   KP_1 : constant := 303;
   KP_2 : constant := 304;
   KP_3 : constant := 305;
   KP_4 : constant := 306;
   KP_5 : constant := 307;
   KP_6 : constant := 308;
   KP_7 : constant := 309;
   KP_8 : constant := 310;
   KP_9 : constant := 311;

   KP_Divide    : constant := 312;
   KP_Multiply  : constant := 313;
   KP_Substract : constant := 314;
   KP_Add       : constant := 315;
   KP_Decimal   : constant := 316;
   KP_Equal     : constant := 317;
   KP_Enter     : constant := 318;
   KP_Num_Lock  : constant := 319;

   Caps_Lock   : constant := 320;
   Scroll_Lock : constant := 321;
   Pause       : constant := 322;
   L_Super     : constant := 323;
   R_Super     : constant := 324;
   Menu        : constant := 325;

   type Unicode_Character is range 0 .. Interfaces.C.int'Last;

   type Key_Callback is access procedure (Subject : Key; Action : Key_State);
   type Character_Callback is access procedure (Unicode_Char : Unicode_Character;
                                                Action : Glfw.Events.Key_State);

   -- Get a String representation of the key. Can be used for displaying the
   -- key's name on the screen. The key name will be in English.
   function Name (Query : Key) return String;

private
   for Key'Size use C.int'Size;
   for Unicode_Character'Size use C.int'Size;

   pragma Convention (C, Key_Callback);
   pragma Convention (C, Character_Callback);
end Glfw.Events.Keys;
