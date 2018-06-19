--  part of OpenGLAda, (c) 2018 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;

package body GL.Enums.Getter is
   use Types;

   function Get_Max (Getter_Param : Parameter) return Types.Int is
      Max : aliased Int;
   begin
      API.Get_Integer (Getter_Param, Max'Access);
      return Max - 1;
   end Get_Max;
end GL.Enums.Getter;