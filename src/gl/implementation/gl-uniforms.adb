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

with GL.API.Singles;
with GL.API.Ints;
with GL.API.UInts;
with GL.Low_Level;

package body GL.Uniforms is

   procedure Set_Single (Location : Uniform; Value : Single) renames
     API.Singles.Uniform1;
      
   procedure Set_Single (Location : Uniform; V1, V2 : Single) renames
     API.Singles.Uniform2;
   
   procedure Set_Single (Location : Uniform; Value : Singles.Vector2) is
   begin
      API.Singles.Uniform2v (Location, 2, (1 => Value));
   end Set_Single;
   
   procedure Set_Single (Location : Uniform; V1, V2, V3 : Single) renames
     API.Singles.Uniform3;
   
   procedure Set_Single (Location : Uniform; Value : Singles.Vector3) is
   begin
      API.Singles.Uniform3v (Location, 3, (1 => Value));
   end Set_Single;
   
   procedure Set_Single (Location : Uniform; V1, V2, V3, V4 : Single) renames
     API.Singles.Uniform4;
   
   procedure Set_Single (Location : Uniform; Value : Singles.Vector4) is
   begin
      API.Singles.Uniform4v (Location, 4, (1 => Value));
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Single_Array) is
   begin
      API.Singles.Uniform1v (Location, Value'Length, Value);
   end Set_Single;
      
   procedure Set_Single (Location : Uniform; Value : Singles.Vector2_Array) is
   begin
      API.Singles.Uniform2v (Location, Value'Length, Value);
   end Set_Single;
   
   procedure Set_Single (Location : Uniform; Value : Singles.Vector3_Array) is
   begin
      API.Singles.Uniform3v (Location, Value'Length, Value);
   end Set_Single;
   
   procedure Set_Single (Location : Uniform; Value : Singles.Vector4_Array) is
   begin
      API.Singles.Uniform4v (Location, Value'Length, Value);
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix2) is
   begin
      API.Singles.Uniform_Matrix2 (Location, 1, Low_Level.False, (1 => Value));
   end Set_Single;
   
   procedure Set_Single (Location : Uniform; Value : Singles.Matrix3) is
   begin
      API.Singles.Uniform_Matrix3 (Location, 1, Low_Level.False, (1 => Value));
   end Set_Single;
   
   procedure Set_Single (Location : Uniform; Value : Singles.Matrix4) is
   begin
      API.Singles.Uniform_Matrix4 (Location, 1, Low_Level.False, (1 => Value));
   end Set_Single;
   
   procedure Set_Single (Location : Uniform; Value : Singles.Matrix2_Array) is
   begin
      API.Singles.Uniform_Matrix2 (Location, Value'Length, Low_Level.False, Value);
   end Set_Single;
   
   procedure Set_Single (Location : Uniform; Value : Singles.Matrix3_Array) is
   begin
      API.Singles.Uniform_Matrix3 (Location, Value'Length, Low_Level.False, Value);
   end Set_Single;
   
   procedure Set_Single (Location : Uniform; Value : Singles.Matrix4_Array) is
   begin
      API.Singles.Uniform_Matrix4 (Location, Value'Length, Low_Level.False, Value);
   end Set_Single;

   procedure Set_Int (Location : Uniform; Value : Int) renames
     API.Ints.Uniform1;
   
   procedure Set_Int (Location : Uniform; V1, V2 : Int) renames
     API.Ints.Uniform2;
   
   procedure Set_Int (Location : Uniform; Value : Ints.Vector2) is
   begin
      API.Ints.Uniform2v (Location, 2, (1 => Value));
   end Set_Int;
   
   procedure Set_Int (Location : Uniform; V1, V2, V3 : Int) renames
     API.Ints.Uniform3;
      
   procedure Set_Int (Location : Uniform; Value : Ints.Vector3) is
   begin
      API.Ints.Uniform3v (Location, 3, (1 => Value));
   end Set_Int;
   
   procedure Set_Int (Location : Uniform; V1, V2, V3, V4 : Int) renames
     API.Ints.Uniform4;
   
   procedure Set_Int (Location : Uniform; Value : Ints.Vector4) is
   begin
      API.Ints.Uniform4v (Location, 4, (1 => Value));
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Int_Array) is
   begin
      API.Ints.Uniform1v (Location, Value'Length, Value);
   end Set_Int;
      
   procedure Set_Int (Location : Uniform; Value : Ints.Vector2_Array) is
   begin
      API.Ints.Uniform2v (Location, Value'Length * 2, Value);
   end Set_Int;
   
   procedure Set_Int (Location : Uniform; Value : Ints.Vector3_Array) is
   begin
      API.Ints.Uniform3v (Location, Value'Length * 3, Value);
   end Set_Int;
   
   procedure Set_Int (Location : Uniform; Value : Ints.Vector4_Array) is
   begin
      API.Ints.Uniform4v (Location, Value'Length * 4, Value);
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Matrix2) is
   begin
      API.Ints.Uniform_Matrix2 (Location, 4, Low_Level.False, (1 => Value));
   end Set_Int;
   
   procedure Set_Int (Location : Uniform; Value : Ints.Matrix3) is
   begin
      API.Ints.Uniform_Matrix3 (Location, 9, Low_Level.False, (1 => Value));
   end Set_Int;
   
   procedure Set_Int (Location : Uniform; Value : Ints.Matrix4) is
   begin
      API.Ints.Uniform_Matrix4 (Location, 16, Low_Level.False, (1 => Value));
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Matrix2_Array) is
   begin
      API.Ints.Uniform_Matrix2 (Location, Value'Length * 4, Low_Level.False, Value);
   end Set_Int;
   
   procedure Set_Int (Location : Uniform; Value : Ints.Matrix3_Array) is
   begin
      API.Ints.Uniform_Matrix3 (Location, Value'Length * 9, Low_Level.False, Value);
   end Set_Int;
   
   procedure Set_Int (Location : Uniform; Value : Ints.Matrix4_Array) is
   begin
      API.Ints.Uniform_Matrix4 (Location, Value'Length * 16, Low_Level.False, Value);
   end Set_Int;

   procedure Set_UInt (Location : Uniform; Value : UInt) renames
     API.UInts.Uniform1;
   
   procedure Set_UInt (Location : Uniform; V1, V2 : UInt) renames
     API.UInts.Uniform2;
   
   procedure Set_UInt (Location : Uniform; Value : UInts.Vector2) is
   begin
      API.UInts.Uniform2v (Location, 2, (1 => Value));
   end Set_UInt;
   
   procedure Set_UInt (Location : Uniform; V1, V2, V3 : UInt) renames
     API.UInts.Uniform3;
   
   procedure Set_UInt (Location : Uniform; Value : UInts.Vector3) is
   begin
      API.UInts.Uniform3v (Location, 3, (1 => Value));
   end Set_UInt;
   
   procedure Set_UInt (Location : Uniform; V1, V2, V3, V4 : UInt) renames
     API.UInts.Uniform4;
   
   procedure Set_UInt (Location : Uniform; Value : UInts.Vector4) is
   begin
      API.UInts.Uniform4v (Location, 4, (1 => Value));
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInt_Array) is
   begin
      API.UInts.Uniform1v (Location, Value'Length, Value);
   end Set_UInt;
   
   procedure Set_UInt (Location : Uniform; Value : UInts.Vector2_Array) is
   begin
      API.UInts.Uniform2v (Location, Value'Length, Value);
   end Set_UInt;
   
   procedure Set_UInt (Location : Uniform; Value : UInts.Vector3_Array) is
   begin
      API.UInts.Uniform3v (Location, Value'Length, Value);
   end Set_UInt;
   
   procedure Set_UInt (Location : Uniform; Value : UInts.Vector4_Array) is
   begin
      API.UInts.Uniform4v (Location, Value'Length, Value);
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix2) is
   begin
      API.UInts.Uniform_Matrix2 (Location, 1, Low_Level.False, (1 => Value));
   end Set_UInt;
   
   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix3) is
   begin
      API.UInts.Uniform_Matrix3 (Location, 1, Low_Level.False, (1 => Value));
   end Set_UInt;
   
   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix4) is
   begin
      API.UInts.Uniform_Matrix4 (Location, 1, Low_Level.False, (1 => Value));
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix2_Array) is
   begin
      API.UInts.Uniform_Matrix2 (Location, Value'Length, Low_Level.False, Value);
   end Set_UInt;
   
   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix3_Array) is
   begin
      API.UInts.Uniform_Matrix3 (Location, Value'Length, Low_Level.False, Value);
   end Set_UInt;
   
   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix4_Array) is
   begin
      API.UInts.Uniform_Matrix4 (Location, Value'Length, Low_Level.False, Value);
   end Set_UInt;

end GL.Uniforms;
