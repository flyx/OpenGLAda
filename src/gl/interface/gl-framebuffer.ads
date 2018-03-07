--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Buffers;
with GL.Pixels;
with GL.Types;

private with GL.Low_Level;

package GL.Framebuffer is
   pragma Preelaborate;

   use GL.Types;

   type Logic_Op is (Clear, And_Op, And_Reverse, Copy, And_Inverted, Noop,
                     Xor_Op, Or_Op, Nor, Equiv, Invert, Or_Reverse,
                     Copy_Inverted, Or_Inverted, Nand, Set);

   subtype Read_Buffer_Selector is Buffers.Color_Buffer_Selector range
     Buffers.Front_Left .. Buffers.Right;

   -- this package provides functionality the works implicitly on the current
   -- framebuffer. for working with framebuffer objects,
   -- see GL.Objects.Framebuffers.

   procedure Set_Clamp_Read_Color (Enabled : Boolean);

   procedure Set_Read_Buffer (Value : Read_Buffer_Selector);
   function Read_Buffer return Read_Buffer_Selector;

   generic
      type Element_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of aliased Element_Type;
   procedure Read_Pixels (X, Y : Int; Width, Height : Size;
                          Format : Pixels.Framebuffer_Format;
                          Data_Type : Pixels.Data_Type; Data : out Array_Type);

   procedure Set_Logic_Op_Mode (Value : Logic_Op);
   function Logic_Op_Mode return Logic_Op;
private
   for Logic_Op use (Clear         => 16#1500#,
                     And_Op        => 16#1501#,
                     And_Reverse   => 16#1502#,
                     Copy          => 16#1503#,
                     And_Inverted  => 16#1504#,
                     Noop          => 16#1505#,
                     Xor_Op        => 16#1506#,
                     Or_Op         => 16#1507#,
                     Nor           => 16#1508#,
                     Equiv         => 16#1509#,
                     Invert        => 16#150A#,
                     Or_Reverse    => 16#150B#,
                     Copy_Inverted => 16#150C#,
                     Or_Inverted   => 16#150D#,
                     Nand          => 16#150E#,
                     Set           => 16#150F#);
   for Logic_Op'Size use Low_Level.Enum'Size;
end GL.Framebuffer;
