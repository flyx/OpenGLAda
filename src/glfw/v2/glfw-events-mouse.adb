--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.API;
with Glfw.Enums;

package body Glfw.Events.Mouse is

   function Pressed (Query : Button) return Boolean is
   begin
      return API.Get_Mouse_Button (Query) = Events.Press;
   end Pressed;

   procedure Get_Position (X, Y : out Coordinate) is
   begin
      API.Get_Mouse_Pos (X, Y);
   end Get_Position;

   procedure Set_Position (X, Y : Coordinate) is
   begin
      API.Set_Mouse_Pos (X, Y);
   end Set_Position;

   function Wheel return Wheel_Position is
   begin
      return API.Get_Mouse_Wheel;
   end Wheel;

   procedure Set_Wheel (Value : Wheel_Position) is
   begin
      API.Set_Mouse_Wheel (Value);
   end Set_Wheel;

   procedure Raw_Button_Callback (Subject : Button; Action : Button_State);
   procedure Raw_Position_Callback (X, Y : Coordinate);
   procedure Raw_Wheel_Callback (Pos : Wheel_Position);

   pragma Convention (C, Raw_Button_Callback);
   pragma Convention (C, Raw_Position_Callback);
   pragma Convention (C, Raw_Wheel_Callback);

   User_Button_Callback   : Button_Callback;
   User_Position_Callback : Position_Callback;
   User_Wheel_Callback    : Wheel_Callback;

   procedure Raw_Button_Callback (Subject : Button; Action : Button_State) is
   begin
      if User_Button_Callback /= null then
         User_Button_Callback (Subject, Action);
      end if;
   end Raw_Button_Callback;

   procedure Raw_Position_Callback (X, Y : Coordinate) is
   begin
      if User_Position_Callback /= null then
         User_Position_Callback (X, Y);
      end if;
   end Raw_Position_Callback;

   procedure Raw_Wheel_Callback (Pos : Wheel_Position) is
   begin
      if User_Wheel_Callback /= null then
         User_Wheel_Callback (Pos);
      end if;
   end Raw_Wheel_Callback;

   procedure Set_Button_Callback (Callback : Button_Callback) is
   begin
      User_Button_Callback := Callback;
      if Callback /= null then
         API.Set_Mouse_Button_Callback (Raw_Button_Callback'Access);
      else
         API.Set_Mouse_Button_Callback (null);
      end if;
   end Set_Button_Callback;

   procedure Set_Position_Callback (Callback : Position_Callback) is
   begin
      User_Position_Callback := Callback;
      if Callback /= null then
         API.Set_Mouse_Pos_Callback (Raw_Position_Callback'Access);
      else
         API.Set_Mouse_Pos_Callback (null);
      end if;
   end Set_Position_Callback;

   procedure Set_Wheel_Callback (Callback : Wheel_Callback) is
   begin
      User_Wheel_Callback := Callback;
      if Callback /= null then
         API.Set_Mouse_Wheel_Callback (Raw_Wheel_Callback'Access);
      else
         API.Set_Mouse_Wheel_Callback (null);
      end if;
   end Set_Wheel_Callback;

   procedure Toggle_Mouse_Cursor (Visible : Boolean) is
   begin
      if Visible then
         API.Enable (Enums.Mouse_Cursor);
      else
         API.Disable (Enums.Mouse_Cursor);
      end if;
   end Toggle_Mouse_Cursor;

   procedure Toggle_Sticky_Mouse_Buttons (Enable : Boolean) is
   begin
      if Enable then
         API.Enable (Enums.Sticky_Mouse_Buttons);
      else
         API.Disable (Enums.Sticky_Mouse_Buttons);
      end if;
   end Toggle_Sticky_Mouse_Buttons;

end Glfw.Events.Mouse;
