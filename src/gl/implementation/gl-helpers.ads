--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Low_Level;
with GL.Types.Colors;

private package GL.Helpers is
   pragma Preelaborate;

   use GL.Types;

   function Float_Array (Value : Colors.Color) return Low_Level.Single_Array;
   function Color (Value : Low_Level.Single_Array) return Colors.Color;

end GL.Helpers;
