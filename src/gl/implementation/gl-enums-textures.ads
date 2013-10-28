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

private with GL.Low_Level;

package GL.Enums.Textures is
   pragma Preelaborate;
   
   -- Texture_Kind is declared in GL.Low_Level.Enums to be accessible for
   -- OpenCLAda

   type Parameter is (Border_Color, Border, Mag_Filter, Min_Filter, Wrap_S,
                      Wrap_T, Priority, Resident, Wrap_R,
                      Min_LoD, Max_LoD, Base_Level, Max_Level, Generate_Mipmap,
                      Depth, Compare_Mode, Compare_Func);

   -- needs to be declared here because of subtypes
   for Parameter use (Border_Color    => 16#1003#,
                      Border          => 16#1004#,
                      Mag_Filter      => 16#2800#,
                      Min_Filter      => 16#2801#,
                      Wrap_S          => 16#2802#,
                      Wrap_T          => 16#2803#,
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
   
   type Level_Parameter is (Width, Height, Internal_Format, Red_Size,
                            Green_Size, Blue_Size, Alpha_Size, Luminance_Size,
                            Intensity_Size,  Compressed_Image_Size, Compressed,
                            Depth_Size, Red_Type, Green_Type, Blue_Type,
                            Alpha_Type, Depth_Type, Buffer_Offset, Buffer_Size);
   
   Texture_Unit_Start_Rep : constant := 16#84C0#;
   
   -- oh god why
   --type Texture_Unit is (Texture0,  Texture1,  Texture2,  Texture3,  Texture4,
   --                      Texture5,  Texture6,  Texture7,  Texture8,  Texture9,
   --                      Texture10, Texture11, Texture12, Texture13, Texture14,
   --                      Texture15, Texture16, Texture17, Texture18, Texture19,
   --                      Texture20, Texture21, Texture22, Texture23, Texture24,
   --                      Texture25, Texture26, Texture27, Texture28, Texture29,
   --                      Texture30, Texture31);

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
   
   for Level_Parameter use (Width           => 16#1000#,
                            Height          => 16#1001#,
                            Internal_Format => 16#1002#,
                            Red_Size        => 16#805C#,
                            Green_Size      => 16#805D#,
                            Blue_Size       => 16#805E#,
                            Alpha_Size      => 16#805F#,
                            Luminance_Size  => 16#8060#,
                            Intensity_Size  => 16#8061#,
                            Compressed_Image_Size => 16#86A0#,
                            Compressed      => 16#86A1#,
                            Depth_Size      => 16#884A#,
                            Red_Type        => 16#8C10#,
                            Green_Type      => 16#8C11#,
                            Blue_Type       => 16#8C12#,
                            Alpha_Type      => 16#8C13#,
                            Depth_Type      => 16#8C14#,
                            Buffer_Offset   => 16#919D#,
                            Buffer_Size     => 16#919E#);
   for Level_Parameter'Size use Low_Level.Enum'Size;
   
   --for Texture_Unit use (Texture0  => 16#84C0#,
   --                      Texture1  => 16#84C1#,
   --                      Texture2  => 16#84C2#,
   --                      Texture3  => 16#84C3#,
   --                      Texture4  => 16#84C4#,
   --                      Texture5  => 16#84C5#,
   --                      Texture6  => 16#84C6#,
   --                      Texture7  => 16#84C7#,
   --                      Texture8  => 16#84C8#,
   --                      Texture9  => 16#84C9#,
   --                      Texture10 => 16#84CA#,
   --                      Texture11 => 16#84CB#,
   --                      Texture12 => 16#84CC#,
   --                      Texture13 => 16#84CD#,
   --                      Texture14 => 16#84CE#,
   --                      Texture15 => 16#84CF#,
   --                      Texture16 => 16#84D0#,
   --                      Texture17 => 16#84D1#,
   --                      Texture18 => 16#84D2#,
   --                      Texture19 => 16#84D3#,
   --                      Texture20 => 16#84D4#,
   --                      Texture21 => 16#84D5#,
   --                      Texture22 => 16#84D6#,
   --                      Texture23 => 16#84D7#,
   --                      Texture24 => 16#84D8#,
   --                      Texture25 => 16#84D9#,
   --                      Texture26 => 16#84DA#,
   --                      Texture27 => 16#84DB#,
   --                      Texture28 => 16#84DC#,
   --                      Texture29 => 16#84DD#,
   --                      Texture30 => 16#84DE#,
   --                      Texture31 => 16#84DF#);
   --for Texture_Unit'Size use Low_Level.Enum'Size;

end GL.Enums.Textures;
