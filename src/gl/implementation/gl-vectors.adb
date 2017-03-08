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
   
   function Cross_Product (Left, Right : Vector) return Vector is
      V1          : array (1 .. 3) of Element_Type;
      V2          : array (1 .. 3) of Element_Type;
      Array_Index : Positive := 1;
    begin
      if Left'Length /= 3 or Right'Length /= 3 then
          raise Constraint_Error with "The vector cross-product is only" &
                                      " defined for vectors of length three.";
      else
            for Index in Left'Range loop
                V1 (Array_Index) := Left (Index);
                V2 (Array_Index) := Right (Index);
                Array_Index := Array_Index + 1;
            end loop;
      
            return (V1 (2) * V2 (3) - V2 (3) * V1 (2),
                    V1 (3) * V2 (1) - V2 (1) * V1 (3),
                    V1 (1) * V2 (2) - V2 (2) * V1 (1));
      end if;
    end Cross_Product;
   
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
