
with System;

package FT is
   pragma Preelaborate;

   type Memory is private;
   type Stream is private;

private
   type Memory is new System.Address;
   type Stream is new System.Address;
end FT;
