--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;

package body GL.Toggles is
   procedure Enable (Subject : Toggle) is
   begin
      API.Enable (Subject);
      Raise_Exception_On_OpenGL_Error;
   end Enable;

   procedure Disable (Subject : Toggle) is
   begin
      API.Disable (Subject);
      Raise_Exception_On_OpenGL_Error;
   end Disable;

   procedure Set (Subject : Toggle; Value : Toggle_State) is
   begin
      if Value = Disabled then
         API.Disable (Subject);
      else
         API.Enable (Subject);
      end if;
      Raise_Exception_On_OpenGL_Error;
   end Set;

   function State (Subject : Toggle) return Toggle_State is
      Value : constant Low_Level.Bool := API.Is_Enabled (Subject);
   begin
      Raise_Exception_On_OpenGL_Error;
      if Value then
         return Enabled;
      else
         return Disabled;
      end if;
   end State;
end GL.Toggles;