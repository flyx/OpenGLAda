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

package body Glfw.Input.Joysticks is

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
      return Boolean (API.Joystick_Present (Source.Raw_Index));
   end Present;
   
   function Positions (Source : Joystick) return Axis_Positions is
      Count : aliased Interfaces.C.int;
      Raw : constant API.Axis_Position_List_Pointers.Pointer
        := API.Get_Joystick_Axes (Source.Raw_Index, Count'Access);
   begin
      return API.Axis_Position_List_Pointers.Value
        (Raw, Interfaces.C.ptrdiff_t (Count));
   end Positions;
      
   function Button_States (Source : Joystick) return Joystick_Button_States is
      Count : aliased Interfaces.C.int;
      Raw : constant API.Joystick_Button_State_List_Pointers.Pointer
        := API.Get_Joystick_Buttons (Source.Raw_Index, Count'Access);
   begin
      return API.Joystick_Button_State_List_Pointers.Value
        (Raw, Interfaces.C.ptrdiff_t (Count));
   end Button_States;

end Glfw.Input.Joysticks;
