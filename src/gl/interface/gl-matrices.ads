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

generic
   type Index_Type is (<>);
   type Element_Type is private;
   with function "+" (Left, Right : Element_Type) return Element_Type is <>;
   with function "-" (Left, Right : Element_Type) return Element_Type is <>;
   with function "-" (Left        : Element_Type) return Element_Type is <>;
   with function "*" (Left, Right : Element_Type) return Element_Type is <>;
   -- not needed currently
   --with function "/" (Left, Right : Element_Type) return Element_Type is <>;
   type Vector_Type is array (Index_Type) of aliased Element_Type;
package GL.Matrices is
   pragma Preelaborate;
   
   type Matrix is array (Index_Type, Index_Type) of aliased Element_Type;
   
   function "+" (Left, Right : Matrix) return Matrix;
   function "-" (Left, Right : Matrix) return Matrix;
   function "-" (Left : Matrix) return Matrix;
   
   -- This is not element-wise but mathematical matrix multiplication.
   function "*" (Left, Right : Matrix) return Matrix;
   
   function "*" (Left : Matrix; Right : Vector_Type) return Vector_Type;
   
   function "*" (Left : Matrix; Right : Element_Type) return Matrix;
   function "*" (Left : Element_Type; Right : Matrix) return Matrix;
   
   function Transpose (Subject : Matrix) return Matrix;
   
   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline ("*");
   pragma Inline (Transpose);
   
   pragma Convention (C, Matrix);
end GL.Matrices;
