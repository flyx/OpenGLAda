--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.GLX;

function GL.API.Subprogram_Reference (Function_Name : String)
  return System.Address is
begin
   return GL.GLX.Get_Proc_Address (Interfaces.C.To_C (Function_Name));
end GL.API.Subprogram_Reference;
