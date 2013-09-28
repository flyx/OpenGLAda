--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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
   
end GL.Algebra;