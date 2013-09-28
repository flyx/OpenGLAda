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

with GL.Algebra;

package GL.Types is
   pragma Preelaborate;
   
   -- These are the types you can and should use with OpenGL functions
   -- (particularly when dealing with buffer objects).
   -- Types that are only used internally, but may be needed when interfacing
   -- with OpenGL-related library APIs can be found in GL.Low_Level.
   
   -- signed integer types
   type Byte  is new C.signed_char;
   type Short is new C.short;
   type Int   is new C.int;
   type Long  is new C.long;
   
   -- unsigned integer types
   type UByte  is new C.unsigned_char;
   type UShort is new C.unsigned_short;
   type UInt   is new C.unsigned;
   
   -- floating point types ("Single" is used to avoid conflicts with Float)
   type Single is new C.C_float;
   type Double is new C.double;
   
   -- array types
   type Int_Array    is array (Positive range <>) of Int;
   type UInt_Array   is array (Positive range <>) of UInt;
   type Single_Array is array (Positive range <>) of Single;
   
   -- type descriptors
   type Numeric_Type is (Byte_Type, UByte_Type, Short_Type,
                         UShort_Type, Int_Type, UInt_Type,
                         Single_Type, Double_Type);
   type Signed_Numeric_Type is (Byte_Type, Short_Type, Int_Type,
                                Single_Type, Double_Type);
   type Unsigned_Numeric_Type is (UByte_Type, UShort_Type, UInt_Type);
   
   -- doesn't really fit here, but there's no other place it fits better
   type Connection_Mode is (Points, Lines, Line_Loop, Line_Strip, Triangles,
                            Triangle_Strip, Triangle_Fan, Quads, Quad_Strip,
                            Polygon);
   
   -- counts the number of components for vertex attributes
   subtype Component_Count is Int range 1 .. 4;
   
   package Bytes is new GL.Algebra (Element_Type => Byte,
                                    Null_Value   => 0,
                                    One_Value    => 1);
   
   package Shorts is new GL.Algebra (Element_Type => Short,
                                     Null_Value   => 0,
                                     One_Value    => 1);
   
   package Ints is new GL.Algebra (Element_Type => Int,
                                   Null_Value   => 0,
                                   One_Value    => 1);
   
   package Longs is new GL.Algebra (Element_Type => Long,
                                    Null_Value   => 0,
                                    One_Value    => 1);
   
   package UBytes is new GL.Algebra (Element_Type => UByte,
                                     Null_Value   => 0,
                                     One_Value    => 1);
   
   package UShorts is new GL.Algebra (Element_Type => UShort,
                                      Null_Value   => 0,
                                      One_Value    => 1);
   
   package UInts is new GL.Algebra (Element_Type => UInt,
                                    Null_Value   => 0,
                                    One_Value    => 1);
   
   package Singles is new GL.Algebra (Element_Type => Single,
                                      Null_Value   => 0.0,
                                      One_Value    => 1.0);
   
   package Doubles is new GL.Algebra (Element_Type => Double,
                                      Null_Value => 0.0,
                                      One_Value    => 1.0);
   
private
   for Numeric_Type use (Byte_Type   => 16#1400#,
                         UByte_Type  => 16#1401#,
                         Short_Type  => 16#1402#,
                         UShort_Type => 16#1403#,
                         Int_Type    => 16#1404#,
                         UInt_Type   => 16#1405#,
                         Single_Type => 16#1406#,
                         Double_Type => 16#140A#);
   for Numeric_Type'Size use UInt'Size;
      
   for Signed_Numeric_Type use (Byte_Type   => 16#1400#,
                                Short_Type  => 16#1402#,
                                Int_Type    => 16#1404#,
                                Single_Type => 16#1406#,
                                Double_Type => 16#140A#);
   for Signed_Numeric_Type'Size use UInt'Size;
   
   for Unsigned_Numeric_Type use (UByte_Type  => 16#1401#,
                                  UShort_Type => 16#1403#,
                                  UInt_Type   => 16#1405#);
   for Unsigned_Numeric_Type'Size use UInt'Size;
   
   for Connection_Mode use (Points         => 16#0000#,
                            Lines          => 16#0001#,
                            Line_Loop      => 16#0002#,
                            Line_Strip     => 16#0003#,
                            Triangles      => 16#0004#,
                            Triangle_Strip => 16#0005#,
                            Triangle_Fan   => 16#0006#,
                            Quads          => 16#0007#,
                            Quad_Strip     => 16#0008#,
                            Polygon        => 16#0009#);
   for Connection_Mode'Size use UInt'Size;
end GL.Types;