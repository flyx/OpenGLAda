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
use GL.Vectors;

package body GL.Normals is

   function "+" (Left, Right : Normal) return Normal is
   begin
      return Normal'(X => Left (X) + Right (X),
                     Y => Left (Y) + Right (Y),
                     Z => Left (Z) + Right (Z));
   end "+";

   function "-" (Left, Right : Normal) return Normal is
   begin
      return Normal'(X => Left (X) - Right (X),
                     Y => Left (Y) - Right (Y),
                     Z => Left (Z) - Right (Z));
   end "-";

   function "*" (Left : Normal;  Right : Integer) return Normal is
   begin
      return Left * Real (Right);
   end "*";

   function "*" (Left : Normal;  Right : Real)    return Normal is
   begin
      return Normal'(X => Left (X) * Right,
                     Y => Left (Y) * Right,
                     Z => Left (Z) * Right);
   end "*";

   function "*" (Left : Real;    Right : Normal)  return Normal is
   begin
      return Right * Left;
   end "*";

   function "/" (Left : Normal; Right : Real)    return Normal is
   begin
      return Normal'(X => Left (X) / Right,
                     Y => Left (Y) / Right,
                     Z => Left (Z) / Right);
   end "/";
end GL.Normals;
