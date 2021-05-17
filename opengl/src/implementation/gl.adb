--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;
with GL.Load_Function_Pointers;

package body GL is

   procedure Init renames GL.Load_Function_Pointers;

   procedure Flush is
   begin
      API.Flush;
   end Flush;

   procedure Finish is
   begin
      API.Finish;
   end Finish;

   -- implementation depends on whether Auto_Exceptions has been enabled.
   procedure Raise_Exception_On_OpenGL_Error is separate;
end GL;
