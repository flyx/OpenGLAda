--  part of OpenGLAda, (c) 2022 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Unchecked_Conversion;

with GL.API;

package body GL.Memory is
   function To_BitField is new Ada.Unchecked_Conversion
     (Barrier_Bits, GL.Low_Level.Bitfield);

   procedure Barrier (Bits : Barrier_Bits) is
   begin
      API.Memory_Barrier (To_BitField (Bits));
      Raise_Exception_On_OpenGL_Error;
   end Barrier;

   procedure Barrier_By_Region (Bits : Barrier_Bits) is
   begin
      API.Memory_Barrier_By_Region (To_BitField (Bits));
      Raise_Exception_On_OpenGL_Error;
   end Barrier_By_Region;
end GL.Memory;

