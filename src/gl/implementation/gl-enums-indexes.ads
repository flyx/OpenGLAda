--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types;
with GL.Enums.Getter;

generic
   Min_Representation : Types.Int;
   Getter_Param : Enums.Getter.Parameter;
package GL.Enums.Indexes is
   pragma Preelaborate;

   use GL.Types;

   function Get_Max return Int;

   Max : constant Int := Get_Max;

   subtype Index is Int range 0 .. Max;

   function Representation (Value : Index) return Int;

   function Value (Representation : Int) return Index;

end GL.Enums.Indexes;