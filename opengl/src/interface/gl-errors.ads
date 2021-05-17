--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

private with GL.Low_Level;

package GL.Errors is
   -- not Pure because Error_Flag can change with each call
   pragma Preelaborate;

   -- The behavior of this package depends on the scenario variable
   -- Auto_Exceptions. If enabled, every call to OpenGL will be followed by
   -- a call to glGetError, and if an error flag is set, the corresponding
   -- exception will be raised.
   --
   -- If disabled, the user has to check manually for the OpenGL error flag by
   -- calling Error_Flag.

   type Error_Code is (No_Error, Invalid_Enum, Invalid_Value, Invalid_Operation,
                       Stack_Overflow, Stack_Underflow, Out_Of_Memory,
                       Invalid_Framebuffer_Operation);

   Invalid_Operation_Error             : exception;
   Out_Of_Memory_Error                 : exception;
   Invalid_Value_Error                 : exception;
   Stack_Overflow_Error                : exception;
   Stack_Underflow_Error               : exception;
   Invalid_Framebuffer_Operation_Error : exception;

   Internal_Error                      : exception;

   function Error_Flag return Error_Code;

private
   for Error_Code use (No_Error          => 0,
                       Invalid_Enum      => 16#0500#,
                       Invalid_Value     => 16#0501#,
                       Invalid_Operation => 16#0502#,
                       Stack_Overflow    => 16#0503#,
                       Stack_Underflow   => 16#0504#,
                       Out_Of_Memory     => 16#0505#,
                       Invalid_Framebuffer_Operation => 16#0506#);
   for Error_Code'Size use Low_Level.Enum'Size;

   -- because we'll just use renames
   pragma Convention (StdCall, Error_Flag);
end GL.Errors;
