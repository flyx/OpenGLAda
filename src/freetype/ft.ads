
with System;

with Interfaces.C; use Interfaces.C;

with GL.Types;

package FT is
   pragma Preelaborate;

   type Memory is private;
   type Stream is private;

   subtype FT_Bool is unsigned_char;
   subtype FT_Error is GL.Types.int;
   subtype FT_String is char;
   subtype FT_ULong is unsigned_long;

   FT_Exception : exception;

private
   type Memory is new System.Address;
   type Stream is new System.Address;
end FT;
