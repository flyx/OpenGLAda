--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C.Pointers;

with GL.Vectors;
with GL.Matrices;

generic
   type Element_Type is private;
   type Index_Type   is (<>);
   with function "+" (Left, Right : Element_Type) return Element_Type is <>;
   with function "-" (Left, Right : Element_Type) return Element_Type is <>;
   with function "-" (Left        : Element_Type) return Element_Type is <>;
   with function "*" (Left, Right : Element_Type) return Element_Type is <>;
   with function "/" (Left, Right : Element_Type) return Element_Type is <>;
   Null_Value, One_Value : Element_Type;
package GL.Algebra is
   pragma Preelaborate;

   -----------------------------------------------------------------------------
   --                              Vector types                               --
   -----------------------------------------------------------------------------

   package Vectors2 is new Vectors (Index_Type   => Index_2D,
                                    Element_Type => Element_Type);
   package Vectors3 is new Vectors (Index_Type   => Index_3D,
                                    Element_Type => Element_Type);
   package Vectors4 is new Vectors (Index_Type   => Index_Homogeneous,
                                    Element_Type => Element_Type);

   type Vector2 is new Vectors2.Vector;
   type Vector3 is new Vectors3.Vector;
   type Vector4 is new Vectors4.Vector;

   -- Conversion functions between vectors. Default values are:
   -- Z = 0, W = 1
   function To_Vector2 (Vector : Vector3) return Vector2;
   function To_Vector2 (Vector : Vector4) return Vector2;
   function To_Vector3 (Vector : Vector2) return Vector3;
   function To_Vector3 (Vector : Vector4) return Vector3;
   function To_Vector4 (Vector : Vector2) return Vector4;
   function To_Vector4 (Vector : Vector3) return Vector4;

   pragma Inline (To_Vector2);
   pragma Inline (To_Vector3);
   pragma Inline (To_Vector4);

   -----------------------------------------------------------------------------
   --                              Matrix types                               --
   -----------------------------------------------------------------------------

   package Matrices2 is new Matrices (Index_Type   => Index_2D,
                                      Element_Type => Element_Type,
                                      Vector_Type  => Vector2);
   package Matrices3 is new Matrices (Index_Type   => Index_3D,
                                      Element_Type => Element_Type,
                                      Vector_Type  => Vector3);
   package Matrices4 is new Matrices (Index_Type   => Index_Homogeneous,
                                      Element_Type => Element_Type,
                                      Vector_Type  => Vector4);

   type Matrix2 is new Matrices2.Matrix;
   type Matrix3 is new Matrices3.Matrix;
   type Matrix4 is new Matrices4.Matrix;

   Identity2 : constant Matrix2 := ((One_Value, Null_Value), (Null_Value, One_Value));
   Identity3 : constant Matrix3 := ((One_Value, Null_Value, Null_Value),
                                    (Null_Value, One_Value, Null_Value),
                                    (Null_Value, Null_Value, One_Value));
   Identity4 : constant Matrix4 := ((One_Value, Null_Value, Null_Value, Null_Value),
                                    (Null_Value, One_Value, Null_Value, Null_Value),
                                    (Null_Value, Null_Value, One_Value, Null_Value),
                                    (Null_Value, Null_Value, Null_Value, One_Value));

   -----------------------------------------------------------------------------
   --                               Array types                               --
   -----------------------------------------------------------------------------

   type Vector2_Array is array (Index_Type range <>) of aliased Vector2;
   type Vector3_Array is array (Index_Type range <>) of aliased Vector3;
   type Vector4_Array is array (Index_Type range <>) of aliased Vector4;

   type Matrix2_Array is array (Index_Type range <>) of aliased Matrix2;
   type Matrix3_Array is array (Index_Type range <>) of aliased Matrix3;
   type Matrix4_Array is array (Index_Type range <>) of aliased Matrix4;

   pragma Convention (C, Vector2_Array);
   pragma Convention (C, Vector3_Array);
   pragma Convention (C, Vector4_Array);
   pragma Convention (C, Matrix2_Array);
   pragma Convention (C, Matrix3_Array);
   pragma Convention (C, Matrix4_Array);

   -----------------------------------------------------------------------------
   --                              Pointer types                              --
   -- note: These instances of the Pointers package do not have a usable      --
   --       default terminator. Only use the size-based subroutines.          --
   -----------------------------------------------------------------------------

   package Vector2_Pointers is new Interfaces.C.Pointers
     (Index_Type, Vector2, Vector2_Array, Vector2'(others => <>));
   package Vector3_Pointers is new Interfaces.C.Pointers
     (Index_Type, Vector3, Vector3_Array, Vector3'(others => <>));
   package Vector4_Pointers is new Interfaces.C.Pointers
     (Index_Type, Vector4, Vector4_Array, Vector4'(others => <>));

   package Matrix2_Pointers is new Interfaces.C.Pointers
     (Index_Type, Matrix2, Matrix2_Array, Matrix2'(others => (others => <>)));
   package Matrix3_Pointers is new Interfaces.C.Pointers
     (Index_Type, Matrix3, Matrix3_Array, Matrix3'(others => (others => <>)));
   package Matrix4_Pointers is new Interfaces.C.Pointers
     (Index_Type, Matrix4, Matrix4_Array, Matrix4'(others => (others => <>)));

   function Cross_Product (Left, Right : Vector3) return Vector3;
end GL.Algebra;
