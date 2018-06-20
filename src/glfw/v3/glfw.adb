--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.API;

with Interfaces.C.Strings;

package body Glfw is
   use type Interfaces.C.int;

   procedure Init is
   begin
      if API.Init = 0 then
         raise Initialization_Exception;
      end if;
   end Init;

   procedure Shutdown is
   begin
      API.Glfw_Terminate;
   end Shutdown;

   procedure Version (Major, Minor, Rev : out Natural) is
      Raw_Major, Raw_Minor, Raw_Rev : C.int;
   begin
      API.Get_Version (Raw_Major, Raw_Minor, Raw_Rev);
      Major := Natural (Raw_Major);
      Minor := Natural (Raw_Minor);
      Rev   := Natural (Raw_Rev);
   end Version;

   function Version_String return String is
   begin
      return Interfaces.C.Strings.Value (API.Get_Version_String);
   end Version_String;

   function Time return Seconds is
   begin
      return API.Get_Time;
   end Time;

   procedure Set_Time (Value : Seconds) is
   begin
      API.Set_Time (Value);
   end Set_Time;

   function Extension_Supported (Name : String) return Boolean is
   begin
      return Boolean (API.Extension_Supported (Interfaces.C.To_C (Name)));
   end Extension_Supported;

end Glfw;
