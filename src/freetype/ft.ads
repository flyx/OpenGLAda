
with Interfaces.C;

with GL.Low_Level;

package FT is
   pragma Preelaborate;

   subtype Bool is GL.Low_Level.Bool;
   subtype Fixed is Interfaces.C.long;
   subtype ULong is Interfaces.C.unsigned_long;

   FreeType_Exception : exception;
end FT;
