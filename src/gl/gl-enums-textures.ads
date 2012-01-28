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

   type Texture_Kind is (TK_1D, TK_2D, TK_3D, TK_Cube_Map);

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

private

   for Texture_Kind use (TK_1D       => 16#0DE0#,
                         TK_2D       => 16#0DE1#,
                         TK_3D       => 16#806F#,
                         TK_Cube_Map => 16#8513#);
   for Texture_Kind'Size use Low_Level.Enum'Size;

   for Compare_Kind use (None => 0, Compare_R_To_Texture => 16#884E#);
   for Compare_Kind'Size use Low_LEvel.Enum'Size;

end GL.Enums.Textures;
