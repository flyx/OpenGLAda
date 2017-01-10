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

with System;

with GL.API.Doubles;
with GL.API.Ints;
with GL.API.Shorts;
with GL.API.Singles;
with GL.API.UInts;
with GL.Low_Level;

package body GL.Attributes is
   procedure Set_Vertex_Attrib_Pointer (Index  : Attribute;
                                        Count  : Component_Count;
                                        Kind   : Numeric_Type;
                                        Stride, Offset : Size) is
   begin
      case Kind is
         when Single_Type =>
            API.Vertex_Attrib_Pointer (Index, Count, Kind, Low_Level.False,
                                       Stride * Single'Size / System.Storage_Unit,
                                       Offset * Single'Size / System.Storage_Unit);
         when Double_Type =>
            API.Vertex_AttribL_Pointer (Index, Count, Kind,
                                       Stride * Double'Size / System.Storage_Unit,
                                       Offset * Double'Size / System.Storage_Unit);
         when UInt_Type =>
            API.Vertex_AttribI_Pointer (Index, Count, Kind,
                                       Stride * UInt'Size / System.Storage_Unit,
                                       Offset * UInt'Size / System.Storage_Unit);
         when UByte_Type =>
            API.Vertex_AttribI_Pointer (Index, Count, Kind,
                                       Stride * UByte'Size / System.Storage_Unit,
                                       Offset * UByte'Size / System.Storage_Unit);
         when UShort_Type =>
            API.Vertex_AttribI_Pointer (Index, Count, Kind,
                                       Stride * UShort'Size / System.Storage_Unit,
                                       Offset * UShort'Size / System.Storage_Unit);
         when Int_Type =>
            API.Vertex_AttribI_Pointer (Index, Count, Kind,
                                       Stride * Int'Size / System.Storage_Unit,
                                       Offset * Int'Size / System.Storage_Unit);
         when Byte_Type =>
            API.Vertex_AttribI_Pointer (Index, Count, Kind,
                                       Stride * Byte'Size / System.Storage_Unit,
                                       Offset * Byte'Size / System.Storage_Unit);
         when Short_Type =>
            API.Vertex_AttribI_Pointer (Index, Count, Kind,
                                       Stride * Short'Size / System.Storage_Unit,
                                       Offset * Short'Size / System.Storage_Unit);
      end case;
      Raise_Exception_On_OpenGL_Error;
   end Set_Vertex_Attrib_Pointer;

   procedure Enable_Vertex_Attrib_Array  (Index : Attribute) is
   begin
      API.Enable_Vertex_Attrib_Array (Index);
   end Enable_Vertex_Attrib_Array;

   procedure Disable_Vertex_Attrib_Array (Index : Attribute) is
   begin
      API.Disable_Vertex_Attrib_Array (Index);
   end Disable_Vertex_Attrib_Array;

   procedure Set_Short (Index : Attribute; Value : Short) is
   begin
      API.Shorts.Vertex_Attrib1 (Index, Value);
   end Set_Short;
   procedure Set_Short (Index : Attribute; V1, V2 : Short) is
   begin
      API.Shorts.Vertex_Attrib2 (Index, V1, V2);
   end Set_Short;
   procedure Set_Short (Index : Attribute; Value : Shorts.Vector2) is
   begin
      API.Shorts.Vertex_Attrib2v (Index, Value);
   end Set_Short;
   procedure Set_Short (Index : Attribute; V1, V2, V3 : Short) is
   begin
      API.Shorts.Vertex_Attrib3 (Index, V1, V2, V3);
   end Set_Short;
   procedure Set_Short (Index : Attribute; Value : Shorts.Vector3) is
   begin
      API.Shorts.Vertex_Attrib3v (Index, Value);
   end Set_Short;
   procedure Set_Short (Index : Attribute; V1, V2, V3, V4 : Short) is
   begin
      API.Shorts.Vertex_Attrib4 (Index, V1, V2, V3, V4);
   end Set_Short;
   procedure Set_Short (Index : Attribute; Value : Shorts.Vector4) is
   begin
      API.Shorts.Vertex_Attrib4v (Index, Value);
   end Set_Short;

   procedure Set_Single (Index : Attribute; Value : Single) is
   begin
      API.Singles.Vertex_Attrib1 (Index, Value);
   end Set_Single;
   procedure Set_Single (Index : Attribute; V1, V2 : Single) is
   begin
      API.Singles.Vertex_Attrib2 (Index, V1, V2);
   end Set_Single;
   procedure Set_Single (Index : Attribute; Value : Singles.Vector2) is
   begin
      API.Singles.Vertex_Attrib2v (Index, Value);
   end Set_Single;
   procedure Set_Single (Index : Attribute; V1, V2, V3 : Single) is
   begin
      API.Singles.Vertex_Attrib3 (Index, V1, V2, V3);
   end Set_Single;
   procedure Set_Single (Index : Attribute; Value : Singles.Vector3) is
   begin
      API.Singles.Vertex_Attrib3v (Index, Value);
   end Set_Single;
   procedure Set_Single (Index : Attribute; V1, V2, V3, V4 : Single) is
   begin
      API.Singles.Vertex_Attrib4 (Index, V1, V2, V3, V4);
   end Set_Single;
   procedure Set_Single (Index : Attribute; Value : Singles.Vector4) is
   begin
      API.Singles.Vertex_Attrib4v (Index, Value);
   end Set_Single;

   procedure Set_Int (Index : Attribute; Value : Int) is
   begin
      API.Ints.Vertex_Attrib1 (Index, Value);
   end Set_Int;
   procedure Set_Int (Index : Attribute; V1, V2 : Int) is
   begin
      API.Ints.Vertex_Attrib2 (Index, V1, V2);
   end Set_Int;
   procedure Set_Int (Index : Attribute; Value : Ints.Vector2) is
   begin
      API.Ints.Vertex_Attrib2v (Index, Value);
   end Set_Int;
   procedure Set_Int (Index : Attribute; V1, V2, V3 : Int) is
   begin
      API.Ints.Vertex_Attrib3 (Index, V1, V2, V3);
   end Set_Int;
   procedure Set_Int (Index : Attribute; Value : Ints.Vector3) is
   begin
      API.Ints.Vertex_Attrib3v (Index, Value);
   end Set_Int;
   procedure Set_Int (Index : Attribute; V1, V2, V3, V4 : Int) is
   begin
      API.Ints.Vertex_Attrib4 (Index, V1, V2, V3, V4);
   end Set_Int;
   procedure Set_Int (Index : Attribute; Value : Ints.Vector4) is
   begin
      API.Ints.Vertex_Attrib4v (Index, Value);
   end Set_Int;

   procedure Set_UInt (Index : Attribute; Value : UInt) is
   begin
      API.UInts.Vertex_Attrib1 (Index, Value);
   end Set_UInt;
   procedure Set_UInt (Index : Attribute; V1, V2 : UInt) is
   begin
      API.UInts.Vertex_Attrib2 (Index, V1, V2);
   end Set_UInt;
   procedure Set_UInt (Index : Attribute; Value : UInts.Vector2) is
   begin
      API.UInts.Vertex_Attrib2v (Index, Value);
   end Set_Uint;
   procedure Set_UInt (Index : Attribute; V1, V2, V3 : UInt) is
   begin
      API.UInts.Vertex_Attrib3 (Index, V1, V2, V3);
   end Set_UInt;
   procedure Set_UInt (Index : Attribute; Value : UInts.Vector3) is
   begin
      API.UInts.Vertex_Attrib3v (Index, Value);
   end Set_UInt;
   procedure Set_UInt (Index : Attribute; V1, V2, V3, V4 : UInt) is
   begin
      API.UInts.Vertex_Attrib4 (Index, V1, V2, V3, V4);
   end Set_UInt;
   procedure Set_UInt (Index : Attribute; Value : UInts.Vector4) is
   begin
      API.UInts.Vertex_Attrib4v (Index, Value);
   end Set_UInt;

   procedure Set_Double (Index : Attribute; Value : Double) is
   begin
      API.Doubles.Vertex_Attrib1 (Index, Value);
   end Set_Double;
   procedure Set_Double (Index : Attribute; V1, V2 : Double) is
   begin
      API.Doubles.Vertex_Attrib2 (Index, V1, V2);
   end Set_Double;
   procedure Set_Double (Index : Attribute; Value : Doubles.Vector2) is
   begin
      API.Doubles.Vertex_Attrib2v (Index, Value);
   end Set_Double;
   procedure Set_Double (Index : Attribute; V1, V2, V3 : Double) is
   begin
      API.Doubles.Vertex_Attrib3 (Index, V1, V2, V3);
   end Set_Double;
   procedure Set_Double (Index : Attribute; Value : Doubles.Vector3) is
   begin
      API.Doubles.Vertex_Attrib3v (Index, Value);
   end Set_Double;
   procedure Set_Double (Index : Attribute; V1, V2, V3, V4 : Double) is
   begin
      API.Doubles.Vertex_Attrib4 (Index, V1, V2, V3, V4);
   end Set_Double;
   procedure Set_Double (Index : Attribute; Value : Doubles.Vector4) is
   begin
      API.Doubles.Vertex_Attrib4v (Index, Value);
   end Set_Double;
end GL.Attributes;
