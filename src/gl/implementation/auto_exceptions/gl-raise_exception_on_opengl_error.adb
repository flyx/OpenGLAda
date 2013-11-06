--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

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
