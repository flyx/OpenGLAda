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

package body GL.Matrices is
   function "+" (Left, Right : Matrix) return Matrix is
      Return_Matrix : Matrix;
   begin
      for Column in Index_Type loop
         for Row in Index_Type loop
            Return_Matrix (Column, Row) :=
              Left (Column, Row) + Right (Column, Row);
         end loop;
      end loop;
      return Return_Matrix;
   end "+";

   function "-" (Left, Right : Matrix) return Matrix is
      Return_Matrix : Matrix;
   begin
      for Column in Index_Type loop
         for Row in Index_Type loop
            Return_Matrix (Column, Row) :=
              Left (Column, Row) - Right (Column, Row);
         end loop;
      end loop;
      return Return_Matrix;
   end "-";

   function "-" (Left : Matrix) return Matrix is
      Ret : Matrix;
   begin
      for Column in Index_Type loop
         for Row in Index_Type loop
            Ret (Column, Row) := -Left (Column, Row);
         end loop;
      end loop;
      return Ret;
   end "-";

   function "*" (Left, Right : Matrix) return Matrix is
      Element : Element_Type;
      Return_Matrix : Matrix;
   begin
      for Column in Index_Type loop
         for Row in Index_Type loop
            for X in Index_Type loop
               if X = Index_Type'First then
                  Element := Left (Column, X) * Right (X, Row);
               else
                  Element := Element + Left (Column, X) * Right (X, Row);
               end if;
            end loop;
            Return_Matrix (Column, Row) := Element;
         end loop;
      end loop;
      return Return_Matrix;
   end "*";

   function "*" (Left : Matrix; Right : Vector_Type) return Vector_Type is
      Return_Vector : Vector_Type;
   begin
      for Row in Index_Type loop
         for Column in Index_Type loop
            if Row = Index_Type'First then
               Return_Vector (Row) := Left (Column, Row) * Right (Column);
            else
               Return_Vector (Row)
                 := Return_Vector (Row) + Left (Column, Row) * Right (Column);
            end if;
         end loop;
      end loop;
      return Return_Vector;
   end "*";

   function "*" (Left : Matrix; Right : Element_Type) return Matrix is
      Return_Matrix : Matrix;
   begin
      for Column in Index_Type loop
         for Row in Index_Type loop
            Return_Matrix (Column, Row) := Left (Column, Row) * Right;
         end loop;
      end loop;
      return Return_Matrix;
   end "*";

   function "*" (Left : Element_Type; Right : Matrix) return Matrix is
   begin
      return Right * Left;
   end "*";

   function Transpose (Subject : Matrix) return Matrix is
      Ret : Matrix;
   begin
      for Column in Index_Type loop
         for Row in Index_Type loop
            Ret (Column, Row) := Subject (Row, Column);
         end loop;
      end loop;
      return Ret;
   end Transpose;
end GL.Matrices;
