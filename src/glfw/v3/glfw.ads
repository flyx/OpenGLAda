--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Interfaces.C;

package Glfw is

   subtype Seconds is Interfaces.C.double;

   subtype Size is Interfaces.C.int range 0 .. Interfaces.C.int'Last;

   Initialization_Exception : exception;
   Operation_Exception      : exception;

   procedure Init;

   -- because terminate is a keyword in Ada
   procedure Shutdown;

   procedure Version (Major, Minor, Rev : out Natural);

   function Version_String return String;


   function Time return Seconds;

   procedure Set_Time (Value : Seconds);


   function Extension_Supported (Name : String) return Boolean;

private
   package C renames Interfaces.C;

   use type Interfaces.C.int;

   type Bool is new Boolean;

   for Bool use (False => 0, True => 1);
   for Bool'Size use C.int'Size;
   pragma Convention (C, Bool);

end Glfw;
