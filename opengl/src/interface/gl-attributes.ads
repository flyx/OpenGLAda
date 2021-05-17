--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types;

package GL.Attributes is
   pragma Preelaborate;

   use GL.Types;

   type Attribute is new UInt;

   -- This function is deprecated for compatibility reasons. Use:
   -- 1. Set_Vertex_Attrib_Pointer (other)
   -- 2. Set_Vertex_Integer_Attrib_Pointer 
   -- 3. Set_Vertex_Double_Attrib_Pointer 
   -- Stride - count of components
   -- Offset - count of components
   procedure Set_Vertex_Attrib_Pointer (Index  : Attribute;
                                        Count  : Component_Count;
                                        Kind   : Numeric_Type;
                                        Stride, Offset : Size);
   pragma Obsolescent
     (Entity => Set_Vertex_Attrib_Pointer,
      Message => "This subroutine is deprecated. Use the other" &
        "Set_Vertex_Attrib_Pointer, Set_Vertex_Integer_Attrib_Pointer, or " &
        "Set_Vertex_Double_Attrib_Pointer");
   
   -- Stride - bytes count
   -- Offset - bytes count
   procedure Set_Vertex_Attrib_Pointer (Index          : Attribute;
                                        Count          : Component_Count;
                                        Kind           : Numeric_Type;
                                        Normalized     : Boolean;
                                        Stride, Offset : Size);
   procedure Set_Vertex_Integer_Attrib_Pointer (Index  : Attribute;
                                                Count  : Component_Count;
                                                Kind   : Numeric_Type;
                                                Stride, Offset : Size);
   procedure Set_Vertex_Double_Attrib_Pointer (Index  : Attribute;
                                               Count  : Component_Count;
                                               Kind   : Numeric_Type;
                                               Stride, Offset : Size);
   
   procedure Enable_Vertex_Attrib_Array  (Index : Attribute);
   procedure Disable_Vertex_Attrib_Array (Index : Attribute);
   procedure Vertex_Attrib_Divisor (Index   : Attribute;
                                    Divisor : UInt);

   procedure Set_Short (Index : Attribute; Value          : Short);
   procedure Set_Short (Index : Attribute; V1, V2         : Short);
   procedure Set_Short (Index : Attribute; Value          : Shorts.Vector2);
   procedure Set_Short (Index : Attribute; V1, V2, V3     : Short);
   procedure Set_Short (Index : Attribute; Value          : Shorts.Vector3);
   procedure Set_Short (Index : Attribute; V1, V2, V3, V4 : Short);
   procedure Set_Short (Index : Attribute; Value          : Shorts.Vector4);

   procedure Set_Single (Index : Attribute; Value          : Single);
   procedure Set_Single (Index : Attribute; V1, V2         : Single);
   procedure Set_Single (Index : Attribute; Value          : Singles.Vector2);
   procedure Set_Single (Index : Attribute; V1, V2, V3     : Single);
   procedure Set_Single (Index : Attribute; Value          : Singles.Vector3);
   procedure Set_Single (Index : Attribute; V1, V2, V3, V4 : Single);
   procedure Set_Single (Index : Attribute; Value          : Singles.Vector4);

   procedure Set_Int (Index : Attribute; Value          : Int);
   procedure Set_Int (Index : Attribute; V1, V2         : Int);
   procedure Set_Int (Index : Attribute; Value          : Ints.Vector2);
   procedure Set_Int (Index : Attribute; V1, V2, V3     : Int);
   procedure Set_Int (Index : Attribute; Value          : Ints.Vector3);
   procedure Set_Int (Index : Attribute; V1, V2, V3, V4 : Int);
   procedure Set_Int (Index : Attribute; Value          : Ints.Vector4);

   procedure Set_UInt (Index : Attribute; Value          : UInt);
   procedure Set_UInt (Index : Attribute; V1, V2         : UInt);
   procedure Set_UInt (Index : Attribute; Value          : UInts.Vector2);
   procedure Set_UInt (Index : Attribute; V1, V2, V3     : UInt);
   procedure Set_UInt (Index : Attribute; Value          : UInts.Vector3);
   procedure Set_UInt (Index : Attribute; V1, V2, V3, V4 : UInt);
   procedure Set_UInt (Index : Attribute; Value          : UInts.Vector4);

   procedure Set_Double (Index : Attribute; Value          : Double);
   procedure Set_Double (Index : Attribute; V1, V2         : Double);
   procedure Set_Double (Index : Attribute; Value          : Doubles.Vector2);
   procedure Set_Double (Index : Attribute; V1, V2, V3     : Double);
   procedure Set_Double (Index : Attribute; Value          : Doubles.Vector3);
   procedure Set_Double (Index : Attribute; V1, V2, V3, V4 : Double);
   procedure Set_Double (Index : Attribute; Value          : Doubles.Vector4);

end GL.Attributes;
