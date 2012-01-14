--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

private package GL.Enums is

   type Attribute_Mask is record
      Depth_Buffer   : Boolean;
      Stencil_Buffer : Boolean;
      Color_Buffer   : Boolean;
   end record;



   type Alpha_Function is (Never, Less, Equal, LEqual, Greater, Not_Equal,
                           GEqual, Always);

   type Blending_Factor_Destination is (Zero, One, Src_Color, One_Minus_Src_Color,
                                        Src_Alpha, One_Minus_Src_Alpha, Dst_Alpha,
                                        One_Minus_Dst_Alpha);

   type Blending_Factor_Source is (Dst_Color, One_Minus_Dst_Color,
                                   Src_Alpha_Saturate);

   type Draw_Buffer_Mode is (None, Front_Left, Front_Right, Back_Left, Back_Right,
                             Front, Back, Left, Right, Front_And_Back);

   type Error_Code is (No_Error, Invalid_Enum, Invalid_Value, Invalid_Operation,
                       Out_Of_Memory);

   type Front_Face_Direction is (CW, CCW);

   type Matrix_Mode is (Modelview, Projection, Texture, Color);

private

   for Attribute_Mask use record
      Depth_Buffer   at 1 range 0 .. 0;
      Stencil_Buffer at 1 range 2 .. 2;
      Color_Buffer   at 1 range 6 .. 6;
   end record;
   for Attribute_Mask'Size use Low_Level.Bitfield'Size;


   for Alpha_Function use (Never     => 16#0200#,
                           Less      => 16#0201#,
                           Equal     => 16#0202#,
                           LEqual    => 16#0203#,
                           Greater   => 16#0204#,
                           Not_Equal => 16#0205#,
                           GEqual    => 16#0206#,
                           Always    => 16#0207#);

   for Blending_Factor_Destination use (Zero                => 0,
                                        One                 => 1,
                                        Src_Color           => 16#0300#,
                                        One_Minus_Src_Color => 16#0301#,
                                        Src_Alpha           => 16#0302#,
                                        One_Minus_Src_Alpha => 16#0303#,
                                        Dst_Alpha           => 16#0304#,
                                        One_Minus_Dst_Alpha => 16#0305#);
   for Blending_Factor_Destination'Size use Low_Level.Enum'Size;

   for Blending_Factor_Source use (Dst_Color           => 16#0306#,
                                   One_Minus_Dst_Color => 16#0307#,
                                   Src_Alpha_Saturate  => 16#0308#);
   for Blending_Factor_Source'Size use Low_Level.Enum'Size;

   for Error_Code use (No_Error          => 0,
                       Invalid_Enum      => 16#0500#,
                       Invalid_Value     => 16#0501#,
                       Invalid_Operation => 16#0502#,
                       Out_Of_Memory     => 16#0505#);
   for Error_Code'Size use Low_Level.Enum'Size;

   for Front_Face_Direction use (CW  => 16#0900#,
                                 CCW => 16#0901#);
   for Front_Face_Direction'Size use Low_Level.Enum'Size;

   for Matrix_Mode use (Modelview  => 16#1700#,
                        Projection => 16#1701#,
                        Texture    => 16#1702#,
                        Color      => 16#1800#);
   for Matrix_Mode'Size use Low_Level.Enum'Size;



end GL.Enums;
