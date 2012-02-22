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

private with GL.Low_Level;

package GL.Enums.Textures is
   -- Texture_Kind is declared in GL.Low_Level.Enums to be accessible for
   -- OpenCLAda

   type Parameter is (Width, Height, Internal_Format, Border_Color, Border,
                      Mag_Filter, Min_Filter, Wrap_S, Wrap_T, Red_Size,
                      Green_Size, Blue_Size, Alpha_Size, Luminance_Size,
                      Intensity_Size, Priority, Resident, Wrap_R,
                      Min_LoD, Max_LoD, Base_Level, Max_Level, Generate_Mipmap,
                      Depth, Compare_Mode, Compare_Func);

   -- needs to be declared here because of subtypes
   for Parameter use (Width           => 16#1000#,
                      Height          => 16#1001#,
                      Internal_Format => 16#1002#,
                      Border_Color    => 16#1003#,
                      Border          => 16#1004#,
                      Mag_Filter      => 16#2800#,
                      Min_Filter      => 16#2801#,
                      Wrap_S          => 16#2802#,
                      Wrap_T          => 16#2803#,
                      Red_Size        => 16#805C#,
                      Green_Size      => 16#805D#,
                      Blue_Size       => 16#805E#,
                      Alpha_Size      => 16#805F#,
                      Luminance_Size  => 16#8060#,
                      Intensity_Size  => 16#8061#,
                      Priority        => 16#8066#,
                      Resident        => 16#8067#,
                      Wrap_R          => 16#8072#,
                      Min_LoD         => 16#813A#,
                      Max_LoD         => 16#813B#,
                      Base_Level      => 16#813C#,
                      Max_Level       => 16#813D#,
                      Generate_Mipmap => 16#8191#,
                      Depth           => 16#884B#,
                      Compare_Mode    => 16#884C#,
                      Compare_Func    => 16#884D#);
   for Parameter'Size use Low_Level.Enum'Size;

   subtype LoD is Parameter range Min_LoD .. Max_Level;

   type Compare_Kind is (None, Compare_R_To_Texture);

   type Env_Target is (Texture_Env, Filter_Control, Point_Sprite);

   type Env_Parameter is (Alpha_Scale, Env_Mode, Env_Color, LoD_Bias, Combine_RGB,
                          Combine_Alpha, RGB_Scale, Src0_RGB, Src1_RGB, Src2_RGB,
                          Src0_Alpha, Src1_Alpha, Src2_Alpha, Operand0_RGB,
                          Operand1_RGB, Operand2_RGB, Operand0_Alpha,
                          Operand1_Alpha, Operand2_Alpha, Coord_Replace);

private

   for Compare_Kind use (None => 0, Compare_R_To_Texture => 16#884E#);
   for Compare_Kind'Size use Low_Level.Enum'Size;

   for Env_Target use (Texture_Env    => 16#2300#,
                               Filter_Control => 16#8500#,
                               Point_Sprite   => 16#8861#);
   for Env_Target'Size use Low_Level.Enum'Size;

   for Env_Parameter use (Alpha_Scale    => 16#0D1C#,
                          Env_Mode       => 16#2200#,
                          Env_Color      => 16#2201#,
                          Lod_Bias       => 16#8501#,
                          Combine_RGB    => 16#8571#,
                          Combine_Alpha  => 16#8572#,
                          RGB_Scale      => 16#8573#,
                          Src0_RGB       => 16#8580#,
                          Src1_RGB       => 16#8581#,
                          Src2_RGB       => 16#8582#,
                          Src0_Alpha     => 16#8588#,
                          Src1_Alpha     => 16#8589#,
                          Src2_Alpha     => 16#858A#,
                          Operand0_RGB   => 16#8590#,
                          Operand1_RGB   => 16#8591#,
                          Operand2_RGB   => 16#8592#,
                          Operand0_Alpha => 16#8598#,
                          Operand1_Alpha => 16#8599#,
                          Operand2_Alpha => 16#859A#,
                          Coord_Replace  => 16#8862#);
   for Env_Parameter'Size use Low_Level.Enum'Size;

end GL.Enums.Textures;
