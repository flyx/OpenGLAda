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

with GL.API;

package body GL.Fixed.Matrix is

   procedure Apply_Frustum (Stack : Matrix_Stack;
                            Left, Right, Bottom, Top, Near, Far : Real) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Frustum (Left, Right, Bottom, Top, Near, Far);
      Check_OpenGL_Error;
   end Apply_Frustum;

   procedure Apply_Orthogonal (Stack : Matrix_Stack;
                               Left, Right, Bottom, Top, Near, Far : Real) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Ortho (Left, Right, Bottom, Top, Near, Far);
      Check_OpenGL_Error;
   end Apply_Orthogonal;

   procedure Load_Identity (Stack : Matrix_Stack) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Load_Identity;
      Check_OpenGL_Error;
   end Load_Identity;

   procedure Load_Matrix (Stack : Matrix_Stack; Value : Matrix4) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Load_Matrix (Value);
      Check_OpenGL_Error;
   end Load_Matrix;

   procedure Apply_Multiplication (Stack : Matrix_Stack; Factor : Matrix4) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Mult_Matrix (Factor);
      Check_OpenGL_Error;
   end Apply_Multiplication;

   procedure Apply_Multiplication (Stack : Matrix_Stack; Factor : Real) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Mult_Matrix (Identity4 * Factor);
      Check_OpenGL_Error;
   end Apply_Multiplication;

   procedure Push (Stack : Matrix_Stack) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Push_Matrix;
      Check_OpenGL_Error;
   end Push;

   procedure Pop  (Stack : Matrix_Stack) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Pop_Matrix;
      Check_OpenGL_Error;
   end Pop;

   -- W component has no effect and therefore is ignored.
   procedure Apply_Rotation (Stack : Matrix_Stack; Angle : Real;
                             Axis : Vector3) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Rotate (Angle, Axis (X), Axis (Y), Axis (Z));
      Check_OpenGL_Error;
   end Apply_Rotation;

   procedure Apply_Rotation (Stack : Matrix_Stack; Angle, X, Y, Z : Real) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Rotate (Angle, X, Y, Z);
      Check_OpenGL_Error;
   end Apply_Rotation;

   procedure Apply_Scaling  (Stack : Matrix_Stack; X, Y, Z : Real) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Scale (X, Y, Z);
      Check_OpenGL_Error;
   end Apply_Scaling;

   procedure Apply_Translation (Stack : Matrix_Stack; Along : Vector3) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Translate (Along (X), Along (Y), Along (Z));
      Check_OpenGL_Error;
   end Apply_Translation;

   procedure Apply_Translation (Stack : Matrix_Stack; X, Y, Z : Real) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Translate (X, Y, Z);
      Check_OpenGL_Error;
   end Apply_Translation;
end GL.Fixed.Matrix;
