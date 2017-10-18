--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;

package body GL.Enums.Indexes is

   function Representation(Value : Index) return Int is
   begin
      return Min_Representation + Value;
   end Representation;

   function Value (Representation : Int) return Index is
   begin
      return Representation - Min_Representation;
   end Value;

   function Get_Max return Int is
      Max : aliased Int;
   begin
      API.Get_Integer (Getter_Param, Max'Access);
      return Max - 1;
   end Get_Max;

end GL.Enums.Indexes;
