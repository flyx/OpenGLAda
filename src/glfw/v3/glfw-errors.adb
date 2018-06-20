--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C.Strings;

with Glfw.API;

package body Glfw.Errors is

   Cur_Callback : Callback := null;

   procedure Raw_Handler (Code : Kind;
                          Description : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Raw_Handler);

   procedure Raw_Handler (Code : Kind;
                          Description : Interfaces.C.Strings.chars_ptr) is
   begin
      if Cur_Callback /= null then
         Cur_Callback.all (Code, Interfaces.C.Strings.Value (Description));
      end if;
   end Raw_Handler;

   procedure Set_Callback (Handler : Callback) is
      Previous : API.Error_Callback;
      pragma Warnings (Off, Previous);
   begin
      Cur_Callback := Handler;
      if Handler = null then
         Previous := API.Set_Error_Callback (null);
      else
         Previous := API.Set_Error_Callback (Raw_Handler'Access);
      end if;
   end Set_Callback;

end Glfw.Errors;
