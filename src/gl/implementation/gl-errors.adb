--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;

package body GL.Errors is
   function Error_Flag return Error_Code renames API.Get_Error;
end GL.Errors;
