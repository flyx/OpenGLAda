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

with GL.Low_Level;

package GL.Vectors is
   type Vector_Index is (X, Y, Z, W);

   -- This is a vector in homogeneous coordinates. All operations on the vector
   -- consider (X, Y, Z) as the actual 3D-vector and W as the homogeneous
   -- component. In other words: If W = 1, the operations will work exactly as
   -- if the input were pure 3D vectors and the W component of the result will
   -- be 1.
   type Vector is array (Vector_Index) of aliased Real;

   function "+" (Left, Right : Vector) return Vector;

   function "-" (Left, Right : Vector) return Vector;

   function "*" (Left : Vector;  Right : Integer) return Vector;
   function "*" (Left : Vector;  Right : Real)    return Vector;
   function "*" (Left : Integer; Right : Vector)  return Vector;
   function "*" (Left : Real;    Right : Vector)  return Vector;

   function "/" (Left : Vector; Right : Integer) return Vector;
   function "/" (Left : Vector; Right : Real) return Vector;

   -- Will raise Constraint_Error when W = 0.
   function Normalize (Source : Vector) return Vector;

private
   pragma Convention (C, Vector);
end GL.Vectors;
