--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C;

package Glfw is

   subtype Seconds is Interfaces.C.double;

   subtype Size is Interfaces.C.int range 0 .. Interfaces.C.int'Last;

   Initialization_Exception : exception;
   Operation_Exception      : exception;

   -- for convenience, besides executing GLFW's init procedures, this also calls
   -- GL.Init.
   procedure Init;

   -- because terminate is a keyword in Ada
   procedure Shutdown;

   procedure Version (Major, Minor, Rev : out Natural);

   function Version_String return String;


   function Time return Seconds;

   procedure Set_Time (Value : Seconds);


   function Extension_Supported (Name : String) return Boolean;

private
   package C renames Interfaces.C;

   type Bool is new Boolean;

   for Bool use (False => 0, True => 1);
   for Bool'Size use C.int'Size;
   pragma Convention (C, Bool);

end Glfw;
