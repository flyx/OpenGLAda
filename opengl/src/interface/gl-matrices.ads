--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

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

   -- this matrix is column-major (i.e. the first index defines the column,
   -- the second index defines the row).
   -- this is important for interoperability with GLSL.
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
