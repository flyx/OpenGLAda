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

with GL.Matrices;

package body GL.Vectors is
   use type Matrices.Matrix;

   function "+" (Left, Right : Vector) return Vector is
      Transformation : Matrices.Matrix := Matrices.Create_Matrix
        (Vector'(1.0, 0.0, 0.0, 0.0), Vector'(0.0, 1.0, 0.0, 0.0),
         Vector'(0.0, 0.0, 1.0, 0.0), Left);
   begin
      return Transformation * Right;
   end "+";

   function "-" (Left, Right : Vector) return Vector is
      Transformation : Matrices.Matrix := Matrices.Create_Matrix
        (Vector'(1.0, 0.0, 0.0, 0.0), Vector'(0.0, 1.0, 0.0, 0.0),
         Vector'(0.0, 0.0, 1.0, 0.0), (-1) * Left);
   begin
      return Transformation * Right;
   end "-";

   function "*" (Left : Vector;  Right : Integer) return Vector is
      Real_Right : Real := Real (Right);
   begin
      return Vector'(X => Left (X) * Real_Right,
                     Y => Left (Y) * Real_Right,
                     Z => Left (Z) * Real_Right,
                     W => Left (W));
   end "*";

   function "*" (Left : Vector;  Right : Real)    return Vector is
   begin
      return Vector'(X => Left (X) * Right,
                     Y => Left (Y) * Right,
                     Z => Left (Z) * Right,
                     W => Left (W));
   end "*";

   function "*" (Left : Integer; Right : Vector)  return Vector is
   begin
      return Right * Left;
   end "*";

   function "*" (Left : Real;    Right : Vector)  return Vector is
   begin
      return Right * Left;
   end "*";

   function "/" (Left : Vector; Right : Integer) return Vector is
      Real_Right : Real := Real (Right);
   begin
      if Right = 0 then
         return Vector'(X => Left (X),
                        Y => Left (Y),
                        Z => Left (Z),
                        W => 0.0);
      else
         return Vector'(X => Left (X) / Real_Right,
                        Y => Left (Y) / Real_Right,
                        Z => Left (Z) / Real_Right,
                        W => Left (W));
      end if;
   end "/";

   function "/" (Left : Vector; Right : Real) return Vector is
   begin
      if Right = 0.0 then
         return Vector'(X => Left (X),
                        Y => Left (Y),
                        Z => Left (Z),
                        W => 0.0);
      else
         return Vector'(X => Left (X) / Right,
                        Y => Left (Y) / Right,
                        Z => Left (Z) / Right,
                        W => Left (W));
      end if;
   end "/";

   function Normalize (Source : Vector) return Vector is
   begin
      if Source (W) = 0.0 then
         raise Constraint_Error;
      end if;
      return Vector'(X => Source (X) / Source (W),
                     Y => Source (Y) / Source (W),
                     Z => Source (Z) / Source (W),
                     W => 1.0);
   end Normalize;


end GL.Vectors;
