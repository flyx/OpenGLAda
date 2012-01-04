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

with Glfw.Api;
with Glfw.Enums;

with Interfaces.C.Strings;

package body Glfw is

   procedure Init is
   begin
      if Api.Init = 0 then
         raise Initialization_Exception;
      end if;
   end Init;

   procedure Terminate_Glfw is
   begin
      Api.Glfw_Terminate;
   end Terminate_Glfw;

   procedure Version (Major, Minor, Rev : out Natural) is
      Raw_Major, Raw_Minor, Raw_Rev : C.int;
   begin
      Api.Get_Version (Raw_Major, Raw_Minor, Raw_Rev);
      Major := Natural (Raw_Major);
      Minor := Natural (Raw_Minor);
      Rev   := Natural (Raw_Rev);
   end Version;

   function Time return Seconds is
   begin
      return Api.Get_Time;
   end Time;

   procedure Set_Time (Value : Seconds) is
   begin
      Api.Set_Time (Value);
   end Set_Time;

   function Extension_Supported (Name : String) return Boolean is
   begin
      return Boolean (Api.Extension_Supported (C.Strings.New_String (Name)));
   end Extension_Supported;

   procedure GL_Version (Major, Minor, Rev : out Natural) is
      Raw_Major, Raw_Minor, Raw_Rev : C.int;
   begin
      Api.Get_GL_Version (Raw_Major, Raw_Minor, Raw_Rev);
      Major := Natural (Raw_Major);
      Minor := Natural (Raw_Minor);
      Rev   := Natural (Raw_Rev);
   end GL_Version;

   procedure Toggle_Auto_Poll_Events (Enable  : Boolean) is
   begin
      if Enable then
         Api.Enable (Enums.Auto_Poll_Events);
      else
         Api.Disable (Enums.Auto_Poll_Events);
      end if;
   end Toggle_Auto_Poll_Events;

end Glfw;
