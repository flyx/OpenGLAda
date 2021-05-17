--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package body GL.Vectors is
   function "+" (Left, Right : Vector) return Vector is
      Ret : Vector;
   begin
      for I in Index_Type'Range loop
         Ret (I) := Left (I) + Right (I);
      end loop;
      return Ret;
   end "+";

   function "-" (Left, Right : Vector) return Vector is
      Ret : Vector;
   begin
      for I in Index_Type'Range loop
         Ret (I) := Left (I) - Right (I);
      end loop;
      return Ret;
   end "-";

   function "-" (Left : Vector) return Vector is
      Ret : Vector;
   begin
      for I in Index_Type loop
         Ret (I) := -Left (I);
      end loop;
      return Ret;
   end "-";

   function "*" (Left : Vector; Right : Element_Type) return Vector is
      Ret : Vector;
   begin
      for I in Index_Type'Range loop
         Ret (I) := Left (I) * Right;
      end loop;
      return Ret;
   end "*";

   function "*" (Left : Element_Type; Right : Vector) return Vector is
   begin
      return Right * Left;
   end "*";

   function "/" (Left : Vector; Right : Element_Type) return Vector is
      Ret : Vector;
   begin
      for I in Index_Type'Range loop
         Ret (I) := Left (I) / Right;
      end loop;
      return Ret;
   end "/";

   function Dot_Product (Left, Right : Vector) return Element_Type is
      Ret : Element_Type;
   begin
      Ret := Left (Left'First) * Right (Right'First);
      for Index in Index_Type'Succ (Index_Type'First) .. Index_Type'Last loop
          Ret := Ret + Left (Index) * Right  (Index);
      end loop;
      return Ret;
   end Dot_Product;

end GL.Vectors;
