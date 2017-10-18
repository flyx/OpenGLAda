--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package body GL.Helpers is

   function Float_Array (Value : Colors.Color) return Low_Level.Single_Array is
      use GL.Types.Colors;
   begin
      return Low_Level.Single_Array'(1 => Value (R),
                                     2 => Value (G),
                                     3 => Value (B),
                                     4 => Value (A));
   end Float_Array;

   function Color (Value : Low_Level.Single_Array) return Colors.Color is
      use GL.Types.Colors;
   begin
      return Colors.Color'(R => Value (1), G => Value (2), B => Value (3),
                           A => Value (4));
   end Color;

end GL.Helpers;
