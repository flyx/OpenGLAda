--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;

package body GL.Fixed.Matrix is

   procedure Apply_Frustum (Stack : Matrix_Stack;
                            Left, Right, Bottom, Top, Near, Far : Double) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Frustum (Left, Right, Bottom, Top, Near, Far);
      Raise_Exception_On_OpenGL_Error;
   end Apply_Frustum;

   procedure Apply_Orthogonal (Stack : Matrix_Stack;
                               Left, Right, Bottom, Top, Near, Far : Double) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Ortho (Left, Right, Bottom, Top, Near, Far);
      Raise_Exception_On_OpenGL_Error;
   end Apply_Orthogonal;

   procedure Load_Identity (Stack : Matrix_Stack) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Load_Identity;
      Raise_Exception_On_OpenGL_Error;
   end Load_Identity;

   procedure Load_Matrix (Stack : Matrix_Stack; Value : Matrix4) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Load_Matrix (Value);
      Raise_Exception_On_OpenGL_Error;
   end Load_Matrix;

   procedure Apply_Multiplication (Stack : Matrix_Stack; Factor : Matrix4) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Mult_Matrix (Factor);
      Raise_Exception_On_OpenGL_Error;
   end Apply_Multiplication;

   procedure Apply_Multiplication (Stack : Matrix_Stack; Factor : Double) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Mult_Matrix (Identity4 * Factor);
      Raise_Exception_On_OpenGL_Error;
   end Apply_Multiplication;

   procedure Push (Stack : Matrix_Stack) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Push_Matrix;
      Raise_Exception_On_OpenGL_Error;
   end Push;

   procedure Pop  (Stack : Matrix_Stack) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Pop_Matrix;
      Raise_Exception_On_OpenGL_Error;
   end Pop;

   procedure Apply_Rotation (Stack : Matrix_Stack; Angle : Double;
                             Axis : Vector3) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Rotate (Angle, Axis (X), Axis (Y), Axis (Z));
      Raise_Exception_On_OpenGL_Error;
   end Apply_Rotation;

   procedure Apply_Rotation (Stack : Matrix_Stack; Angle, X, Y, Z : Double) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Rotate (Angle, X, Y, Z);
      Raise_Exception_On_OpenGL_Error;
   end Apply_Rotation;

   procedure Apply_Scaling  (Stack : Matrix_Stack; X, Y, Z : Double) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Scale (X, Y, Z);
      Raise_Exception_On_OpenGL_Error;
   end Apply_Scaling;

   procedure Apply_Translation (Stack : Matrix_Stack; Along : Vector3) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Translate (Along (X), Along (Y), Along (Z));
      Raise_Exception_On_OpenGL_Error;
   end Apply_Translation;

   procedure Apply_Translation (Stack : Matrix_Stack; X, Y, Z : Double) is
   begin
      API.Matrix_Mode (Stack.Mode);
      API.Translate (X, Y, Z);
      Raise_Exception_On_OpenGL_Error;
   end Apply_Translation;
end GL.Fixed.Matrix;
