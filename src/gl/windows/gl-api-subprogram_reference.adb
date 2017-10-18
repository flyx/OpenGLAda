--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.WGL;

function GL.API.Subprogram_Reference (Function_Name : String)
  return System.Address is
   GL_Function_Name_C : Interfaces.C.Strings.chars_ptr
      := Interfaces.C.Strings.New_String (Function_Name);

   Result : constant System.Address
      := GL.WGL.wglGetProcAddress (GL_Function_Name_C);
begin
   Interfaces.C.Strings.Free (GL_Function_Name_C);
   return Result;
end GL.API.Subprogram_Reference;

