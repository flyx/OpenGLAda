--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C;

package Glfw is

   subtype Seconds is Interfaces.C.double;

   Initialization_Exception : exception;

   -- for convenience, besides executing GLFW's init procedures, this also calls
   -- GL.Init.
   procedure Init;

   procedure Terminate_Glfw;

   procedure Version (Major, Minor, Rev : out Natural);


   function Time return Seconds;

   procedure Set_Time (Value : Seconds);


   function Extension_Supported (Name : String) return Boolean;

   procedure GL_Version (Major, Minor, Rev : out Natural);

   procedure Toggle_Auto_Poll_Events (Enable  : Boolean);

private
   package C renames Interfaces.C;

   use type Interfaces.C.int;

   type Bool is new Boolean;



   for Bool use (False => 0, True => 1);
   for Bool'Size use C.int'Size;
   pragma Convention (C, Bool);

end Glfw;
