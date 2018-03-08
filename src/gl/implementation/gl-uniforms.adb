--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API.Singles;
with GL.API.Ints;
with GL.API.UInts;
with GL.Low_Level;

package body GL.Uniforms is

   procedure Set_Single (Location : Uniform; Value : Single) is
   begin
      API.Singles.Uniform1 (Location, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; V1, V2 : Single) is
   begin
      API.Singles.Uniform2 (Location, V1, V2);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector2) is
   begin
      API.Singles.Uniform2v (Location, 1, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; V1, V2, V3 : Single) is
   begin
      API.Singles.Uniform3 (Location, V1, V2, V3);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector3) is
   begin
      API.Singles.Uniform3v (Location, 1, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; V1, V2, V3, V4 : Single) is
   begin
      API.Singles.Uniform4 (Location, V1, V2, V3, V4);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector4) is
   begin
      API.Singles.Uniform4v (Location, 1, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Single_Array) is
   begin
      API.Singles.Uniform1v (Location, Value'Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector2_Array) is
   begin
      API.Singles.Uniform2v (Location, Value'Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector3_Array) is
   begin
      API.Singles.Uniform3v (Location, Value'Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector4_Array) is
   begin
      API.Singles.Uniform4v (Location, Value'Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix2) is
   begin
      API.Singles.Uniform_Matrix2 (Location, 1, Low_Level.False, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix3) is
   begin
      API.Singles.Uniform_Matrix3 (Location, 1, Low_Level.False, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix4) is
   begin
      API.Singles.Uniform_Matrix4 (Location, 1, Low_Level.False, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix2_Array) is
   begin
      API.Singles.Uniform_Matrix2
        (Location, Value'Length, Low_Level.False, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix3_Array) is
   begin
      API.Singles.Uniform_Matrix3
        (Location, Value'Length, Low_Level.False, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix4_Array) is
   begin
      API.Singles.Uniform_Matrix4
        (Location, Value'Length, Low_Level.False, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Single;

   procedure Set_Int (Location : Uniform; Value : Int) is
   begin
      API.Ints.Uniform1 (Location, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; V1, V2 : Int) is
   begin
      API.Ints.Uniform2 (Location, V1, V2);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector2) is
   begin
      API.Ints.Uniform2v (Location, 1, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; V1, V2, V3 : Int) is
   begin
      API.Ints.Uniform3 (Location, V1, V2, V3);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector3) is
   begin
      API.Ints.Uniform3v (Location, 1, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; V1, V2, V3, V4 : Int) is
   begin
      API.Ints.Uniform4 (Location, V1, V2, V3, V4);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector4) is
   begin
      API.Ints.Uniform4v (Location, 1, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Int_Array) is
   begin
      API.Ints.Uniform1v (Location, Value'Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector2_Array) is
   begin
      API.Ints.Uniform2v (Location, Value'Length * 2, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector3_Array) is
   begin
      API.Ints.Uniform3v (Location, Value'Length * 3, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector4_Array) is
   begin
      API.Ints.Uniform4v (Location, Value'Length * 4, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Matrix2) is
   begin
      API.Ints.Uniform_Matrix2 (Location, 1, Low_Level.False, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Matrix3) is
   begin
      API.Ints.Uniform_Matrix3 (Location, 1, Low_Level.False, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Matrix4) is
   begin
      API.Ints.Uniform_Matrix4 (Location, 1, Low_Level.False, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Matrix2_Array) is
   begin
      API.Ints.Uniform_Matrix2
        (Location, Value'Length, Low_Level.False, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Matrix3_Array) is
   begin
      API.Ints.Uniform_Matrix3
        (Location, Value'Length, Low_Level.False, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Matrix4_Array) is
   begin
      API.Ints.Uniform_Matrix4
        (Location, Value'Length, Low_Level.False, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Int;

   procedure Set_UInt (Location : Uniform; Value : UInt) is
   begin
      API.UInts.Uniform1 (Location, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; V1, V2 : UInt) is
   begin
      API.UInts.Uniform2 (Location, V1, V2);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector2) is
   begin
      API.UInts.Uniform2v (Location, 1, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; V1, V2, V3 : UInt) is
   begin
      API.UInts.Uniform3 (Location, V1, V2, V3);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector3) is
   begin
      API.UInts.Uniform3v (Location, 1, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; V1, V2, V3, V4 : UInt) is
   begin
      API.UInts.Uniform4 (Location, V1, V2, V3, V4);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector4) is
   begin
      API.UInts.Uniform4v (Location, 1, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInt_Array) is
   begin
      API.UInts.Uniform1v (Location, Value'Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector2_Array) is
   begin
      API.UInts.Uniform2v (Location, Value'Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector3_Array) is
   begin
      API.UInts.Uniform3v (Location, Value'Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector4_Array) is
   begin
      API.UInts.Uniform4v (Location, Value'Length, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix2) is
   begin
      API.UInts.Uniform_Matrix2 (Location, 1, Low_Level.False, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix3) is
   begin
      API.UInts.Uniform_Matrix3 (Location, 1, Low_Level.False, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix4) is
   begin
      API.UInts.Uniform_Matrix4 (Location, 1, Low_Level.False, (1 => Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix2_Array) is
   begin
      API.UInts.Uniform_Matrix2
        (Location, Value'Length, Low_Level.False, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix3_Array) is
   begin
      API.UInts.Uniform_Matrix3
        (Location, Value'Length, Low_Level.False, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix4_Array) is
   begin
      API.UInts.Uniform_Matrix4
        (Location, Value'Length, Low_Level.False, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_UInt;

end GL.Uniforms;
