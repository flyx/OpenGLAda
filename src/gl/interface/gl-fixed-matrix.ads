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

with GL.Types;

private with GL.Enums;

-- Matrix stack API. This API is deprecated as of OpenGL 3.0.
package GL.Fixed.Matrix is
   
   use GL.Types.Doubles;

   type Matrix_Stack (<>) is tagged private;

   Modelview  : constant Matrix_Stack;
   Projection : constant Matrix_Stack;
   Texture    : constant Matrix_Stack;
   Color      : constant Matrix_Stack;

   procedure Apply_Frustum (Stack : Matrix_Stack;
                            Left, Right, Bottom, Top, Near, Far : Double);
   procedure Apply_Orthogonal (Stack : Matrix_Stack;
                               Left, Right, Bottom, Top, Near, Far : Double);

   procedure Load_Identity (Stack : Matrix_Stack);
   procedure Load_Matrix (Stack : Matrix_Stack; Value : Matrix4);

   procedure Apply_Multiplication (Stack : Matrix_Stack; Factor : Matrix4);
   procedure Apply_Multiplication (Stack : Matrix_Stack; Factor : Double);

   procedure Push (Stack : Matrix_Stack);
   procedure Pop  (Stack : Matrix_Stack);

   procedure Apply_Rotation (Stack : Matrix_Stack; Angle : Double;
                             Axis : Vector3);
   procedure Apply_Rotation (Stack : Matrix_Stack; Angle, X, Y, Z : Double);

   procedure Apply_Scaling  (Stack : Matrix_Stack; X, Y, Z : Double);

   procedure Apply_Translation (Stack : Matrix_Stack; Along : Vector3);
   procedure Apply_Translation (Stack : Matrix_Stack; X, Y, Z : Double);

private
   type Matrix_Stack (Mode : Enums.Matrix_Mode) is tagged null record;

   Modelview  : constant Matrix_Stack := Matrix_Stack'(Mode => Enums.Modelview);
   Projection : constant Matrix_Stack := Matrix_Stack'(Mode => Enums.Projection);
   Texture    : constant Matrix_Stack := Matrix_Stack'(Mode => Enums.Texture);
   Color      : constant Matrix_Stack := Matrix_Stack'(Mode => Enums.Color);
end GL.Fixed.Matrix;
