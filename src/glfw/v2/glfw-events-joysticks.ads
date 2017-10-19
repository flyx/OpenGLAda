--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

private with Glfw.Enums;

package Glfw.Events.Joysticks is
   use type Interfaces.C.C_float;

   -- GLFW supports up to 16 joysticks; they are indexed from 1 to 16.
   type Joystick_Index is range 1 .. 16;

   -- A Joystick object will link to the first joystick by default.
   type Joystick is tagged private;

   type Axis_Position is new Interfaces.C.C_float range -1.0 .. 1.0;
   type Axis_Positions is array (Positive range <>) of Axis_Position;

   function Index (Source : Joystick) return Joystick_Index;
   procedure Set_Index (Target : in out Joystick; Value : Joystick_Index);

   function Present (Source : Joystick) return Boolean;
   function Num_Axis (Source : Joystick) return Natural;
   function Num_Buttons (Source : Joystick) return Natural;

   procedure Get_Positions (Source : Joystick; Values : in out Axis_Positions);
   procedure Get_Buttons   (Source : Joystick; Values : in out Button_States);

private
   type Joystick is tagged record
      Raw_Index : Enums.Joystick_ID := Enums.Joystick_1;
   end record;
end Glfw.Events.Joysticks;
