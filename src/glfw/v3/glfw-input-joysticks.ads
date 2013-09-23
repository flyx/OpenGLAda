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

private with Glfw.Enums;

package Glfw.Input.Joysticks is
   use type Interfaces.C.C_float;
   
   -- GLFW supports up to 16 joysticks; they are indexed from 1 to 16.
   type Joystick_Index is range 1 .. 16;
   
   -- A Joystick object will link to the first joystick by default.
   type Joystick is tagged private;
   
   type Axis_Position is new Interfaces.C.C_float range -1.0 .. 1.0;
   type Axis_Positions is array (Positive range <>) of aliased Axis_Position;
   
   type Joystick_Button_State is new Button_State;
   type Joystick_Button_States is array (Positive range <>) of
     aliased Joystick_Button_State;
   
   function Index (Source : Joystick) return Joystick_Index;
   procedure Set_Index (Target : in out Joystick; Value : Joystick_Index);
   
   function Present (Source : Joystick) return Boolean;
   
   function Positions (Source : Joystick) return Axis_Positions;
   function Button_States (Source : Joystick) return Joystick_Button_States;
   
private
   type Joystick is tagged record
      Raw_Index : Enums.Joystick_ID := Enums.Joystick_1;
   end record;
   
   for Joystick_Button_State'Size use Interfaces.C.char'Size;
end Glfw.Input.Joysticks;
