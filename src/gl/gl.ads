--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

package GL is
   package C renames Interfaces.C;

   -- Used where high precision is wanted. If high precision is not required,
   -- Standard.Float is used for simplicity.
   subtype Real is Interfaces.C.double;

   type Float_Precision is (Single, Double);

   -- Enabled by default. If disabled, no exceptions will be raised when OpenGL
   -- encounters an error. Disabling might result in a speed-up because OpenGL
   -- errors have to be polled.
   procedure Toggle_Error_Checking (Enabled : Boolean);

   Invalid_Operation : exception;
   Out_Of_Memory     : exception;
   Invalid_Value     : exception;
   Internal_Error    : exception;

   -- Set the precision of floats passed to OpenGL. This will be used for all
   -- vertex transfers to OpenGL.
   procedure Toggle_Precision (Precision : Float_Precision);

   procedure Flush;

private
   use type Real;

   -----------------------------------------------------------------------------
   --                             Primitive types                             --
   -----------------------------------------------------------------------------

   subtype Clamp_F is C.C_float;
   subtype Clamp_D is C.double;

   -----------------------------------------------------------------------------
   --                              Global state                               --
   -----------------------------------------------------------------------------

   Error_Checking_Enabled   : Boolean := True;
   Error_Checking_Suspended : Boolean := False;
   Requested_Precision    : Float_Precision := Double;

   -----------------------------------------------------------------------------
   --                           Internal functions                            --
   -----------------------------------------------------------------------------

   procedure Check_OpenGL_Error;
   pragma Inline (Check_OpenGL_Error);

end GL;
