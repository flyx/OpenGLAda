--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package body GL.Enums.Indexes is
   function Representation (Value : Index) return Int is
   begin
      return Min_Representation + Value;
   end Representation;

   function Value (Representation : Int) return Index is
   begin
      return Representation - Min_Representation;
   end Value;
end GL.Enums.Indexes;
