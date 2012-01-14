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

with GL.Low_Level;
with GL.Vectors;

private with GL.Enums;

-- Matrix stack API. This API is deprecated as of OpenGL 3.0.
-- (but the Matrix type can be useful anyway, especially with its operators.)
package GL.Matrices is

   type Matrix_Stack (<>) is tagged private;

   Modelview  : constant Matrix_Stack;
   Projection : constant Matrix_Stack;
   Texture    : constant Matrix_Stack;
   Color      : constant Matrix_Stack;

   type Matrix is array (1 .. 4, 1 .. 4) of Real;

   Identity : constant Matrix;

   function "+" (Left, Right : Matrix) return Matrix;
   function "-" (Left, Right : Matrix) return Matrix;

   -- This is not element-wise but mathematical matrix multiplication.
   function "*" (Left, Right : Matrix) return Matrix;

   function "*" (Left : Matrix; Right : Vectors.Vector) return Vectors.Vector;

   function "*" (Left : Matrix; Right : Real) return Matrix;
   function "*" (Left : Real; Right : Matrix) return Matrix;

   function Create_Matrix (Col1, Col2, Col3, Col4 : Vectors.Vector)
                           return Matrix;

   procedure Apply_Frustum (Stack : Matrix_Stack;
                            Left, Right, Bottom, Top, Near, Far : Real);
   procedure Apply_Orthogonal (Stack : Matrix_Stack;
                               Left, Right, Bottom, Top, Near, Far : Real);

   procedure Load_Identity (Stack : Matrix_Stack);
   procedure Load_Matrix (Stack : Matrix_Stack; Value : Matrix);

   procedure Apply_Multiplication (Stack : Matrix_Stack; Factor : Matrix);
   procedure Apply_Multiplication (Stack : Matrix_Stack; Factor : Real);

   procedure Push (Stack : Matrix_Stack);
   procedure Pop  (Stack : Matrix_Stack);

   -- W component has no effect and therefore is ignored.
   procedure Apply_Rotation (Stack : Matrix_Stack; Angle : Real;
                             Axis : Vectors.Vector);
   procedure Apply_Rotation (Stack : Matrix_Stack; Angle, X, Y, Z : Real);

   procedure Apply_Scaling  (Stack : Matrix_Stack; X, Y, Z : Real);

   procedure Apply_Translation (Stack : Matrix_Stack; Along : Vectors.Vector);
   procedure Apply_Translation (Stack : Matrix_Stack; X, Y, Z : Real);

private
   pragma Convention (C, Matrix);

   type Matrix_Stack (Mode : Enums.Matrix_Mode) is tagged null record;

   Modelview  : constant Matrix_Stack := Matrix_Stack'(Mode => Enums.Modelview);
   Projection : constant Matrix_Stack := Matrix_Stack'(Mode => Enums.Projection);
   Texture    : constant Matrix_Stack := Matrix_Stack'(Mode => Enums.Texture);
   Color      : constant Matrix_Stack := Matrix_Stack'(Mode => Enums.Color);

   Identity : constant Matrix := ((1.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.0),
                                  (0.0, 0.0, 1.0, 0.0), (0.0, 0.0, 0.0, 1.0));

end GL.Matrices;
