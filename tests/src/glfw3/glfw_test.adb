--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.Errors;
with Ada.Text_IO;

package body Glfw_Test is

   procedure Print_Error (Code : Glfw.Errors.Kind; Description : String) is
   begin
      Ada.Text_IO.Put_Line ("Error occured (" & Code'Img & "): " & Description);
   end Print_Error;

   procedure Enable_Print_Errors is
   begin
      Glfw.Errors.Set_Callback (Print_Error'Access);
   end Enable_Print_Errors;
end Glfw_Test;
