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
   pragma Preelaborate;
   
   package C renames Interfaces.C;
   
   -----------------------------------------------------------------------------
   --                                 Basics                                  --
   -----------------------------------------------------------------------------
   
   procedure Flush;
   procedure Finish;
   
   -- index types for vectors and matrices
   type Index_Homogeneous is (X, Y, Z, W);
   subtype Index_3D is Index_Homogeneous range X .. Z;
   subtype Index_2D is Index_Homogeneous range X .. Y;
   
   -----------------------------------------------------------------------------
   --                             Configuration                               --
   -----------------------------------------------------------------------------

   -- Enabled by default. If disabled, no exceptions will be raised when OpenGL
   -- encounters an error. Disabling might result in a speed-up because OpenGL
   -- errors have to be polled.
   procedure Toggle_Error_Checking (Enabled : Boolean);

   Invalid_Operation : exception;
   Out_Of_Memory     : exception;
   Invalid_Value     : exception;
   Internal_Error    : exception;

private

   -----------------------------------------------------------------------------
   --                           Internal functions                            --
   -----------------------------------------------------------------------------

   procedure Check_OpenGL_Error;
   pragma Inline (Check_OpenGL_Error);
   
   procedure Suspend_Error_Checking;
   procedure Resume_Error_Checking;
   
   pragma Inline (Suspend_Error_Checking);
   pragma Inline (Resume_Error_Checking);

end GL;
