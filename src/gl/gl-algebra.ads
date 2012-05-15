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

with GL.Vectors;
with GL.Matrices;

package GL.Algebra is
   pragma Preelaborate;
   
   type Index_Homogeneous is (X, Y, Z, W);
   subtype Index_3D is Index_Homogeneous range X .. Z;
   subtype Index_2D is Index_Homogeneous range X .. Y;
   
   -----------------------------------------------------------------------------
   --                              Vector types                               --
   -----------------------------------------------------------------------------
   
   package Vectors2 is new Vectors (Index_Type => Index_2D);
   package Vectors3 is new Vectors (Index_Type => Index_3D);
   package Vectors4 is new Vectors (Index_Type => Index_Homogeneous);
   
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
   
   package Matrices2 is new Matrices (Index_Type  => Index_2D,
                                      Vector_Type => Vector2);
   package Matrices3 is new Matrices (Index_Type  => Index_3D,
                                      Vector_Type => Vector3);
   package Matrices4 is new Matrices (Index_Type  => Index_Homogeneous,
                                      Vector_Type => Vector4);
   
   type Matrix2 is new Matrices2.Matrix;
   type Matrix3 is new Matrices3.Matrix;
   type Matrix4 is new Matrices4.Matrix;
   
   Identity2 : constant Matrix2 := ((1.0, 0.0), (0.0, 1.0));
   Identity3 : constant Matrix3 := ((1.0, 0.0, 0.0), (0.0, 1.0, 0.0),
                                    (0.0, 0.0, 1.0));
   Identity4 : constant Matrix4 := ((1.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.0),
                                    (0.0, 0.0, 1.0, 0.0), (0.0, 0.0, 0.0, 1.0));

end GL.Algebra;