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

package Glfw.Events.Mouse is

   type Button is new Interfaces.C.int range 0 .. 7;

   Left_Button   : constant := 0;
   Right_Button  : constant := 1;
   Middle_Button : constant := 2;

   subtype Coordinate is Interfaces.C.int;
   subtype Wheel_Position is Interfaces.C.int;

   type Button_Callback is access procedure (Subject : Button; Action : Key_State);
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
