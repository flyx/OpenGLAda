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

with Glfw.API;

package body Glfw.Events.Joysticks is

   function Index (Source : Joystick) return Joystick_Index is
   begin
      return Enums.Joystick_ID'Pos (Source.Raw_Index) + 1;
   end Index;
      
   procedure Set_Index (Target : in out Joystick; Value : Joystick_Index) is
   begin
      Target.Raw_Index := Enums.Joystick_ID'Val (Value - 1);
   end Set_Index;

   function Present (Source : Joystick) return Boolean is
   begin
      return API.Get_Joystick_Param (Source.Raw_Index, Enums.Present) /= 0;
   end Present;
      
   function Num_Axis (Source : Joystick) return Natural is
   begin
      return Natural (API.Get_Joystick_Param (Source.Raw_Index, Enums.Axis));
   end Num_Axis;
      
   function Num_Buttons (Source : Joystick) return Natural is
   begin 
      return Natural (API.Get_Joystick_Param (Source.Raw_Index, Enums.Buttons));
   end Num_Buttons;

   procedure Get_Positions (Source : Joystick; Values : in out Axis_Positions) is
      Unused        : Interfaces.C.int := API.Get_Joystick_Pos (
        Source.Raw_Index, Values,
        Interfaces.C.int (Values'Last - Values'First + 1));
   begin
      null;
   end Get_Positions;
   
   procedure Get_Buttons   (Source : Joystick; Values : in out Button_States) is
      Unused      : Interfaces.C.int := API.Get_Joystick_Buttons (
        Source.Raw_Index, Values,
        Interfaces.C.int (Values'Last - Values'First + 1));
   begin
      null;
   end Get_Buttons;

end Glfw.Events.Joysticks;
