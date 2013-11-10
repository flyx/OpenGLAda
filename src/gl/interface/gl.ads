--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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
   
   -- raised when a function that is not available for the current context
   -- is called.
   Feature_Not_Supported_Exception : exception;
   
   -- raised when OpenGLAda does not support a certain OpenGL feature
   -- (either because it's too new and has not yet been wrapped, or because
   -- it's so deprecated that you shouldn't use it anyway)
   Not_Implemented_Exception : exception;
private

   -----------------------------------------------------------------------------
   --                           Internal functions                            --
   -----------------------------------------------------------------------------

   procedure Raise_Exception_On_OpenGL_Error;
   pragma Inline (Raise_Exception_On_OpenGL_Error);

end GL;
