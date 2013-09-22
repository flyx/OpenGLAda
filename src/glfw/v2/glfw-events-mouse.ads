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

package Glfw.Events.Mouse is

   type Button is new Interfaces.C.int range 0 .. 7;

   Left_Button   : constant := 0;
   Right_Button  : constant := 1;
   Middle_Button : constant := 2;

   subtype Coordinate is Interfaces.C.int;
   subtype Wheel_Position is Interfaces.C.int;

   type Button_Callback is access procedure (Subject : Button; Action : Button_State);
   type Position_Callback is access procedure (X, Y : Coordinate);
   type Wheel_Callback is access procedure (Pos : Wheel_Position);

   function Pressed (Query : Button) return Boolean;

   procedure Get_Position (X, Y : out Coordinate);

   procedure Set_Position (X, Y : Coordinate);

   function Wheel return Wheel_Position;

   procedure Set_Wheel (Value : Wheel_Position);

   procedure Set_Button_Callback (Callback : Button_Callback);

   procedure Set_Position_Callback (Callback : Position_Callback);

   procedure Set_Wheel_Callback (Callback : Wheel_Callback);

   procedure Toggle_Mouse_Cursor         (Visible : Boolean);
   procedure Toggle_Sticky_Mouse_Buttons (Enable : Boolean);

private
   for Button'Size use Interfaces.C.int'Size;


end Glfw.Events.Mouse;
