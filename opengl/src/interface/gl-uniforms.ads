--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types; use GL.Types;

package GL.Uniforms is
   pragma Preelaborate;

   type Uniform is new Int;

   procedure Set_Single (Location : Uniform; Value          : Single);
   procedure Set_Single (Location : Uniform; V1, V2         : Single);
   procedure Set_Single (Location : Uniform; Value          : Singles.Vector2);
   procedure Set_Single (Location : Uniform; V1, V2, V3     : Single);
   procedure Set_Single (Location : Uniform; Value          : Singles.Vector3);
   procedure Set_Single (Location : Uniform; V1, V2, V3, V4 : Single);
   procedure Set_Single (Location : Uniform; Value          : Singles.Vector4);

   procedure Set_Single  (Location : Uniform; Value : Single_Array);
   procedure Set_Single (Location : Uniform; Value : Singles.Vector2_Array);
   procedure Set_Single (Location : Uniform; Value : Singles.Vector3_Array);
   procedure Set_Single (Location : Uniform; Value : Singles.Vector4_Array);

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix2);
   procedure Set_Single (Location : Uniform; Value : Singles.Matrix3);
   procedure Set_Single (Location : Uniform; Value : Singles.Matrix4);

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix2_Array);
   procedure Set_Single (Location : Uniform; Value : Singles.Matrix3_Array);
   procedure Set_Single (Location : Uniform; Value : Singles.Matrix4_Array);

   procedure Set_Int (Location : Uniform; Value          : Int);
   procedure Set_Int (Location : Uniform; V1, V2         : Int);
   procedure Set_Int (Location : Uniform; Value          : Ints.Vector2);
   procedure Set_Int (Location : Uniform; V1, V2, V3     : Int);
   procedure Set_Int (Location : Uniform; Value          : Ints.Vector3);
   procedure Set_Int (Location : Uniform; V1, V2, V3, V4 : Int);
   procedure Set_Int (Location : Uniform; Value          : Ints.Vector4);

   procedure Set_Int  (Location : Uniform; Value : Int_Array);
   procedure Set_Int (Location : Uniform; Value : Ints.Vector2_Array);
   procedure Set_Int (Location : Uniform; Value : Ints.Vector3_Array);
   procedure Set_Int (Location : Uniform; Value : Ints.Vector4_Array);

   procedure Set_Int (Location : Uniform; Value : Ints.Matrix2);
   procedure Set_Int (Location : Uniform; Value : Ints.Matrix3);
   procedure Set_Int (Location : Uniform; Value : Ints.Matrix4);

   procedure Set_Int (Location : Uniform; Value : Ints.Matrix2_Array);
   procedure Set_Int (Location : Uniform; Value : Ints.Matrix3_Array);
   procedure Set_Int (Location : Uniform; Value : Ints.Matrix4_Array);

   procedure Set_UInt (Location : Uniform; Value          : UInt);
   procedure Set_UInt (Location : Uniform; V1, V2         : UInt);
   procedure Set_UInt (Location : Uniform; Value          : UInts.Vector2);
   procedure Set_UInt (Location : Uniform; V1, V2, V3     : UInt);
   procedure Set_UInt (Location : Uniform; Value          : UInts.Vector3);
   procedure Set_UInt (Location : Uniform; V1, V2, V3, V4 : UInt);
   procedure Set_UInt (Location : Uniform; Value          : UInts.Vector4);

   procedure Set_UInt (Location : Uniform; Value : UInt_Array);
   procedure Set_UInt (Location : Uniform; Value : UInts.Vector2_Array);
   procedure Set_UInt (Location : Uniform; Value : UInts.Vector3_Array);
   procedure Set_UInt (Location : Uniform; Value : UInts.Vector4_Array);

   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix2);
   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix3);
   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix4);

   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix2_Array);
   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix3_Array);
   procedure Set_UInt (Location : Uniform; Value : UInts.Matrix4_Array);

end GL.Uniforms;
