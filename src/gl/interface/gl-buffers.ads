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

with GL.Low_Level;
with GL.Types.Colors;
with GL.Culling;

package GL.Buffers is
   pragma Preelaborate;
   
   use GL.Types;
   
   type Buffer_Bits is record
      Depth   : Boolean := False;
      Accum   : Boolean := False;
      Stencil : Boolean := False;
      Color   : Boolean := False;
   end record;
   
   subtype Depth is Double range 0.0 .. 1.0;
   
   subtype Stencil_Index is Int;

   type Stencil_Action is (Zero, Invert, Keep, Replace,
                           Increment, Decrement,
                           Increment_Wrap, Decrement_Wrap);

   -- Aux buffer support was dropped with OpenGL 3.
   -- this type allows selection of multiple buffers at once
   -- (Front => Front_Left and Front_Right and so on)
   type Color_Buffer_Selector is (None, Front_Left,
                                  Front_Right, Back_Left, Back_Right,
                                  Front, Back, Left, Right, Front_And_Back);
   
   -- defined here because of following subtype declaration
   for Color_Buffer_Selector use (None           => 0,
                                  Front_Left     => 16#0400#,
                                  Front_Right    => 16#0401#,
                                  Back_Left      => 16#0402#,
                                  Back_Right     => 16#0403#,
                                  Front          => 16#0404#,
                                  Back           => 16#0405#,
                                  Left           => 16#0406#,
                                  Right          => 16#0407#,
                                  Front_And_Back => 16#0408#);
   for Color_Buffer_Selector'Size use Low_Level.Enum'Size;

   subtype Base_Color_Buffer_Selector is Color_Buffer_Selector
     range Front .. Front_And_Back;

   -- misses Front, Right etc. from above because they may map to multiple
   -- buffers.
   type Explicit_Color_Buffer_Selector is (None, Front_Left, Front_Right,
                                           Back_Left, Back_Right,
                                           Color_Attachment0,
                                           Color_Attachment1,
                                           Color_Attachment2,
                                           Color_Attachment3,
                                           Color_Attachment4,
                                           Color_Attachment5,
                                           Color_Attachment6,
                                           Color_Attachment7,
                                           Color_Attachment8,
                                           Color_Attachment9,
                                           Color_Attachment10,
                                           Color_Attachment11,
                                           Color_Attachment12,
                                           Color_Attachment13,
                                           Color_Attachment14,
                                           Color_Attachment15);
   subtype Draw_Buffer_Index is UInt range 0 .. 15;
   type Explicit_Color_Buffer_List is array (Draw_Buffer_Index range <>)
     of Explicit_Color_Buffer_Selector;

   subtype Single_Face_Selector is Culling.Face_Selector
     range Culling.Front .. Culling.Back;

   procedure Clear (Bits : Buffer_Bits);
   
   procedure Set_Active_Buffer (Selector : Explicit_Color_Buffer_Selector);
   procedure Set_Active_Buffers (List : Explicit_Color_Buffer_List);
   
   procedure Set_Color_Clear_Value (Value : Colors.Color);
   function Color_Clear_Value return Colors.Color;
   
   procedure Set_Depth_Clear_Value (Value : Depth);
   function Depth_Clear_Value return Depth;
   
   procedure Set_Stencil_Clear_Value (Value : Stencil_Index);
   function Stencil_Clear_Value return Stencil_Index;
   
   -- dropped in OpenGL 3
   procedure Set_Accum_Clear_Value (Value : Colors.Color);
   function Accum_Clear_Value return Colors.Color;

   procedure Set_Depth_Function (Func : Compare_Function);
   function Depth_Function return Compare_Function;

   procedure Depth_Mask (Enabled : Boolean);
   function Depth_Mask return Boolean;

   procedure Set_Stencil_Function (Func : Compare_Function;
                                   Ref  : Int;
                                   Mask : UInt);

   procedure Set_Stencil_Function (Face : Culling.Face_Selector;
                                   Func : Compare_Function;
                                   Ref  : Int;
                                   Mask : UInt);

   function Stencil_Function (Face : Single_Face_Selector) return Compare_Function;
   function Stencil_Reference_Value (Face : Single_Face_Selector) return Int;
   function Stencil_Value_Mask (Face : Single_Face_Selector) return UInt;

   procedure Set_Stencil_Operation (Stencil_Fail : Buffers.Stencil_Action;
                                    Depth_Fail   : Buffers.Stencil_Action;
                                    Depth_Pass   : Buffers.Stencil_Action);

   procedure Set_Stencil_Operation (Face : Culling.Face_Selector;
                                    Stencil_Fail : Buffers.Stencil_Action;
                                    Depth_Fail   : Buffers.Stencil_Action;
                                    Depth_Pass   : Buffers.Stencil_Action);

   function Stencil_Operation_Stencil_Fail (Face : Single_Face_Selector) return Buffers.Stencil_Action;
   function Stencil_Operation_Depth_Fail (Face : Single_Face_Selector) return Buffers.Stencil_Action;
   function Stencil_Operation_Depth_Pass (Face : Single_Face_Selector) return Buffers.Stencil_Action;

   procedure Set_Stencil_Mask (Value : UInt);

   procedure Set_Stencil_Mask (Face  : Culling.Face_Selector;
                               Value : UInt);

   function Stencil_Mask (Face : Single_Face_Selector) return UInt;

   -- The following procedures are available since OpenGL 3.0
   
   -- for one or multiple color buffers
   procedure Clear_Color_Buffers (Selector : Base_Color_Buffer_Selector;
                                  Value    : Colors.Color);
   
   -- for one specific draw buffer
   procedure Clear_Draw_Buffer (Index : Draw_Buffer_Index;
                                Value : Colors.Color);
   
   procedure Clear_Depth_Buffer (Value : Depth);
   
   procedure Clear_Stencil_Buffer (Value : Stencil_Index);
   
   procedure Clear_Depth_And_Stencil_Buffer (Depth_Value   : Depth;
                                             Stencil_Value : Stencil_Index);
   
private
   for Buffer_Bits use record
      Depth   at 0 range 8 .. 8;
      Accum   at 0 range 9 .. 9;
      Stencil at 0 range 10 .. 10;
      Color   at 0 range 14 .. 14;
   end record;
   for Buffer_Bits'Size use Low_Level.Bitfield'Size;

   for Stencil_Action use (Zero           => 0,
                           Invert         => 16#150A#,
                           Keep           => 16#1E00#,
                           Replace        => 16#1E01#,
                           Increment      => 16#1E02#,
                           Decrement      => 16#1E03#,
                           Increment_Wrap => 16#8507#,
                           Decrement_Wrap => 16#8508#);
   for Stencil_Action'Size use Low_Level.Enum'Size;

   for Explicit_Color_Buffer_Selector use (None               => 0,
                                           Front_Left         => 16#0400#,
                                           Front_Right        => 16#0401#,
                                           Back_Left          => 16#0402#,
                                           Back_Right         => 16#0403#,
                                           Color_Attachment0  => 16#8CE0#,
                                           Color_Attachment1  => 16#8CE1#,
                                           Color_Attachment2  => 16#8CE2#,
                                           Color_Attachment3  => 16#8CE3#,
                                           Color_Attachment4  => 16#8CE4#,
                                           Color_Attachment5  => 16#8CE5#,
                                           Color_Attachment6  => 16#8CE6#,
                                           Color_Attachment7  => 16#8CE7#,
                                           Color_Attachment8  => 16#8CE8#,
                                           Color_Attachment9  => 16#8CE9#,
                                           Color_Attachment10 => 16#8CEA#,
                                           Color_Attachment11 => 16#8CEB#,
                                           Color_Attachment12 => 16#8CEC#,
                                           Color_Attachment13 => 16#8CED#,
                                           Color_Attachment14 => 16#8CEE#,
                                           Color_Attachment15 => 16#8CEF#);
   for Explicit_Color_Buffer_Selector'Size use Low_Level.Enum'Size;
   
   pragma Convention (C, Explicit_Color_Buffer_List);
   
end GL.Buffers;
