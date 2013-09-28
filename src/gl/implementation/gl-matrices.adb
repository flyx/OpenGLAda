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
      for I in Index_Type loop
         for J in Index_Type loop
            Return_Matrix (I, J) := Left (I, J) + Right (I, J);
         end loop;
      end loop;
      return Return_Matrix;
   end "+";
   
   function "-" (Left, Right : Matrix) return Matrix is
      Return_Matrix : Matrix;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            Return_Matrix (I, J) := Left (I, J) - Right (I, J);
         end loop;
      end loop;
      return Return_Matrix;
   end "-";
   
   function "-" (Left : Matrix) return Matrix is
      Ret : Matrix;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            Ret (I, J) := -Left (I, J);
         end loop;
      end loop;
      return Ret;
   end "-";
   
   function "*" (Left, Right : Matrix) return Matrix is
      Element : Element_Type;
      Return_Matrix : Matrix;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            for X in Index_Type loop
               if I = Index_Type'First and J = Index_Type'First
                 and X = Index_Type'First then
                  Element := Left (I, X) * Right (X, J);
               else
                  Element := Element + Left (I, X) * Right (X, J);
               end if;
            end loop;
            Return_Matrix (I, J) := Element;
         end loop;
      end loop;
      return Return_Matrix;
   end "*";
   
   function "*" (Left : Matrix; Right : Vector_Type) return Vector_Type is
      Return_Vector : Vector_Type;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            if J = Index_Type'First then
               Return_Vector (I) := Left (I, J) * Right (J);
            else
               Return_Vector (I)
                 := Return_Vector (I) + Left (I, J) * Right (J);
            end if;
         end loop;
      end loop;
      return Return_Vector;
   end "*";
   
   function "*" (Left : Matrix; Right : Element_Type) return Matrix is
      Return_Matrix : Matrix;
   begin
      for I in Index_Type loop
         for J in Index_Type loop
            Return_Matrix (I, J) := Left (I, J) * Right;
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
      for I in Index_Type loop
         for J in Index_Type loop
            Ret (I, J) := Subject (J, I);
         end loop;
      end loop;
      return Ret;
   end Transpose;
end GL.Matrices;