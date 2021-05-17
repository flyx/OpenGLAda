--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Errors;

separate (GL)
procedure Raise_Exception_On_OpenGL_Error is
begin
   case Errors.Error_Flag is
      when Errors.Invalid_Operation => raise Errors.Invalid_Operation_Error;
      when Errors.Invalid_Value => raise Errors.Invalid_Value_Error;
      when Errors.Invalid_Framebuffer_Operation =>
         raise Errors.Invalid_Framebuffer_Operation_Error;
      when Errors.Out_Of_Memory => raise Errors.Out_Of_Memory_Error;
      when Errors.Stack_Overflow => raise Errors.Stack_Overflow_Error;
      when Errors.Stack_Underflow => raise Errors.Stack_Underflow_Error;
      when Errors.Invalid_Enum => raise Errors.Internal_Error;
      when Errors.No_Error => null;
   end case;
exception
   when Constraint_Error => raise Errors.Internal_Error;
end Raise_Exception_On_OpenGL_Error;
