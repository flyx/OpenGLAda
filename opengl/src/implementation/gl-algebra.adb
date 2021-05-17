--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package body GL.Algebra is

   function To_Vector2 (Vector : Vector3) return Vector2 is
   begin
      return Vector2'(Vector (X), Vector (Y));
   end To_Vector2;

   function To_Vector2 (Vector : Vector4) return Vector2 is
   begin
      return Vector2'(Vector (X), Vector (Y));
   end To_Vector2;

   function To_Vector3 (Vector : Vector2) return Vector3 is
   begin
      return Vector3'(Vector (X), Vector (Y), Null_Value);
   end To_Vector3;

   function To_Vector3 (Vector : Vector4) return Vector3 is
   begin
      return Vector3'(Vector (X), Vector (Y), Vector (Z));
   end To_Vector3;

   function To_Vector4 (Vector : Vector2) return Vector4 is
   begin
      return Vector4'(Vector (X), Vector (Y), Null_Value, One_Value);
   end To_Vector4;

   function To_Vector4 (Vector : Vector3) return Vector4 is
   begin
      return Vector4'(Vector (X), Vector (Y), Vector (Z), One_Value);
   end To_Vector4;

   function Cross_Product (Left, Right : Vector3) return Vector3 is
   begin
      return (Left (Y) * Right (Z) - Left (Z) * Right (Y),
              Left (Z) * Right (X) - Left (X) * Right (Z),
              Left (X) * Right (Y) - Left (Y) * Right (X));
   end Cross_Product;

end GL.Algebra;
