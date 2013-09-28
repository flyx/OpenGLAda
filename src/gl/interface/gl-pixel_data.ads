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
private with GL.Types;

package GL.Pixel_Data is
   pragma Preelaborate;

   type Internal_Format is (Depth_Component, Alpha, RGB, RGBA, Luminance,
                            Luminance_Alpha, R3_G3_B2, Alpha4, Alpha8, Alpha12,
                            Alpha16, Luminance4, Luminance8, Luminance12,
                            Luminance16, Luminance4_Alpha4, Luminance6_Alpha2,
                            Luminance8_Alpha8, Luminance12_Alpha4,
                            Luminance12_Alpha12, Luminance16_Alpha16,
                            Intensity, Intensity4, Intensity8, Intensity12,
                            Intensity16, RGB4, RGB5, RGB8, RGB10, RGB12, RGB16,
                            RGBA2, RGBA4, RGB5_A1, RGBA8, RGB10_A2, RGBA12,
                            RGBA16, Depth_Component16, Depth_Component24,
                            Depth_Component32, Compressed_Alpha,
                            Compressed_Luminance, Compressed_Luminance_Alpha,
                            Compressed_Intensity, Compressed_RGB,
                            Compressed_RGBA, SRGB, SRGB8, SRGB_Alpha,
                            SRGB8_Alpha8, SLuminance_Alpha, SLuminance8_Alpha8,
                            SLuminance, SLuminance8);

   type Format is (Color_Index, Red, Green, Blue, Alpha, RGB, RGBA,
                   Luminance, Luminance_Alpha, BGR, BGRA);

   type Data_Type is (Byte, Unsigned_Byte, Short, Unsigned_Short, Int,
                      Unsigned_Int, Float, Bitmap, Unsigned_Byte_3_3_2,
                      Unsigned_Short_4_4_4_4,
                      Unsigned_Short_5_5_5_1,
                      Unsigned_Int_8_8_8_8,
                      Unsigned_Int_10_10_10_2,
                      Unsigned_Byte_2_3_3_Rev,
                      Unsigned_Short_5_6_5,
                      Unsinged_Short_5_6_5_Rev,
                      Unsigned_Short_4_4_4_4_Rev,
                      Unsigned_Short_1_5_5_5_Rev,
                      Unsigned_Int_8_8_8_8_Rev,
                      Unsigned_Int_2_10_10_10_Rev);

private
   for Internal_Format use (Depth_Component => 16#1902#,
                            Alpha           => 16#1906#,
                            RGB             => 16#1907#,
                            RGBA            => 16#1908#,
                            Luminance       => 16#1909#,
                            Luminance_Alpha => 16#190A#,

                            R3_G3_B2 => 16#2A10#,

                            Alpha4      => 16#803B#,
                            Alpha8      => 16#803C#,
                            Alpha12     => 16#803D#,
                            Alpha16     => 16#803E#,
                            Luminance4  => 16#803F#,
                            Luminance8  => 16#8040#,
                            Luminance12 => 16#8041#,
                            Luminance16 => 16#8042#,

                            Luminance4_Alpha4   => 16#8043#,
                            Luminance6_Alpha2   => 16#8044#,
                            Luminance8_Alpha8   => 16#8045#,
                            Luminance12_Alpha4  => 16#8046#,
                            Luminance12_Alpha12 => 16#8047#,
                            Luminance16_Alpha16 => 16#8048#,

                            Intensity   => 16#8049#,
                            Intensity4  => 16#804A#,
                            Intensity8  => 16#804B#,
                            Intensity12 => 16#804C#,
                            Intensity16 => 16#804D#,

                            RGB4     => 16#804F#,
                            RGB5     => 16#8050#,
                            RGB8     => 16#8051#,
                            RGB10    => 16#8052#,
                            RGB12    => 16#8053#,
                            RGB16    => 16#8054#,
                            RGBA2    => 16#8055#,
                            RGBA4    => 16#8056#,
                            RGB5_A1  => 16#8057#,
                            RGBA8    => 16#8058#,
                            RGB10_A2 => 16#8059#,
                            RGBA12   => 16#805A#,
                            RGBA16   => 16#805B#,

                            Depth_Component16 => 16#81A5#,
                            Depth_Component24 => 16#81A6#,
                            Depth_Component32 => 16#81A7#,

                            Compressed_Alpha           => 16#84E9#,
                            Compressed_Luminance       => 16#84EA#,
                            Compressed_Luminance_Alpha => 16#84EB#,
                            Compressed_Intensity       => 16#84EC#,
                            Compressed_RGB             => 16#84ED#,
                            Compressed_RGBA            => 16#84EE#,

                            SRGB               => 16#8C40#,
                            SRGB8              => 16#8C41#,
                            SRGB_Alpha         => 16#8C42#,
                            SRGB8_Alpha8       => 16#8C43#,
                            SLuminance_Alpha   => 16#8C44#,
                            SLuminance8_Alpha8 => 16#8C45#,
                            SLuminance         => 16#8C46#,
                            SLuminance8        => 16#8C47#);
   for Internal_Format'Size use GL.Types.Int'Size;

   for Format use (Color_Index     => 16#1900#,
                   Red             => 16#1903#,
                   Green           => 16#1904#,
                   Blue            => 16#1905#,
                   Alpha           => 16#1906#,
                   RGB             => 16#1907#,
                   RGBA            => 16#1908#,
                   Luminance       => 16#1909#,
                   Luminance_Alpha => 16#190A#,
                   BGR             => 16#80E0#,
                   BGRA            => 16#80E1#);
   for Format'Size use Low_Level.Enum'Size;

   for Data_Type use (Byte           => 16#1400#,
                      Unsigned_Byte  => 16#1401#,
                      Short          => 16#1402#,
                      Unsigned_Short => 16#1403#,
                      Int            => 16#1404#,
                      Unsigned_Int   => 16#1405#,
                      Float          => 16#1406#,
                      Bitmap         => 16#1A00#,

                      Unsigned_Byte_3_3_2         => 16#8032#,
                      Unsigned_Short_4_4_4_4      => 16#8033#,
                      Unsigned_Short_5_5_5_1      => 16#8034#,
                      Unsigned_Int_8_8_8_8        => 16#8035#,
                      Unsigned_Int_10_10_10_2     => 16#8036#,
                      Unsigned_Byte_2_3_3_Rev     => 16#8362#,
                      Unsigned_Short_5_6_5        => 16#8363#,
                      Unsinged_Short_5_6_5_Rev    => 16#8364#,
                      Unsigned_Short_4_4_4_4_Rev  => 16#8365#,
                      Unsigned_Short_1_5_5_5_Rev  => 16#8366#,
                      Unsigned_Int_8_8_8_8_Rev    => 16#8367#,
                      Unsigned_Int_2_10_10_10_Rev => 16#8368#);
   for Data_Type'Size use Low_Level.Enum'Size;

end GL.Pixel_Data;
