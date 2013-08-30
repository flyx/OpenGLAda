--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <flyx@isobeef.org>
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


package Glfw.API.V2 is
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


end Glfw.API.V2;
