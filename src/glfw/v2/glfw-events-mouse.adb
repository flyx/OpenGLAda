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

package body Glfw.Events.Mouse is

   function Pressed (Query : Button) return Boolean is
   begin
      return Api.Get_Mouse_Button (Query) = Events.Press;
   end Pressed;

   procedure Get_Position (X, Y : out Coordinate) is
   begin
      Api.Get_Mouse_Pos (X, Y);
   end Get_Position;

   procedure Set_Position (X, Y : Coordinate) is
   begin
      Api.Set_Mouse_Pos (X, Y);
   end Set_Position;

   function Wheel return Wheel_Position is
   begin
      return Api.Get_Mouse_Wheel;
   end Wheel;

   procedure Set_Wheel (Value : Wheel_Position) is
   begin
      Api.Set_Mouse_Wheel (Value);
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
         Api.Set_Mouse_Button_Callback (Raw_Button_Callback'Access);
      else
         Api.Set_Mouse_Button_Callback (null);
      end if;
   end Set_Button_Callback;

   procedure Set_Position_Callback (Callback : Position_Callback) is
   begin
      User_Position_Callback := Callback;
      if Callback /= null then
         Api.Set_Mouse_Pos_Callback (Raw_Position_Callback'Access);
      else
         Api.Set_Mouse_Pos_Callback (null);
      end if;
   end Set_Position_Callback;

   procedure Set_Wheel_Callback (Callback : Wheel_Callback) is
   begin
      User_Wheel_Callback := Callback;
      if Callback /= null then
         Api.Set_Mouse_Wheel_Callback (Raw_Wheel_Callback'Access);
      else
         Api.Set_Mouse_Wheel_Callback (null);
      end if;
   end Set_Wheel_Callback;

   procedure Toggle_Mouse_Cursor (Visible : Boolean) is
   begin
      if Visible then
         Api.Enable (Enums.Mouse_Cursor);
      else
         Api.Disable (Enums.Mouse_Cursor);
      end if;
   end Toggle_Mouse_Cursor;

   procedure Toggle_Sticky_Mouse_Buttons (Enable : Boolean) is
   begin
      if Enable then
         Api.Enable (Enums.Sticky_Mouse_Buttons);
      else
         Api.Disable (Enums.Sticky_Mouse_Buttons);
      end if;
   end Toggle_Sticky_Mouse_Buttons;

end Glfw.Events.Mouse;
