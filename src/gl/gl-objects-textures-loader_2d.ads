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

with GL.Pixel_Data;

private with GL.Low_Level;

package GL.Objects.Textures.Loader_2D is
   pragma Preelaborate;

   type Target_Kind is (TX_2D, Proxy_2D, Cube_Map_Positive_X, Cube_Map_Negative_X,
                        Cube_Map_Positive_Y, Cube_Map_Negative_Y,
                        Cube_Map_Positive_Z, Cube_Map_Negative_Z, Proxy_Cube_Map);

   procedure Load_Empty_Texture (Target: Target_Kind; Level : Integer;
                                 Internal_Format : Pixel_Data.Internal_Format;
                                 Width, Height : Natural;
                                 Border : Boolean;
                                 Format : Pixel_Data.Format;
                                 Data_Type : Pixel_Data.Data_Type);

   -- TODO: Create textures from data.

private
   for Target_Kind use (TX_2D    => 16#0DE1#,
                        Proxy_2D => 16#8064#,
                        Cube_Map_Positive_X => 16#8515#,
                        Cube_Map_Negative_X => 16#8516#,
                        Cube_Map_Positive_Y => 16#8517#,
                        Cube_Map_Negative_Y => 16#8518#,
                        Cube_Map_Positive_Z => 16#8519#,
                        Cube_Map_Negative_Z => 16#851A#,
                        Proxy_Cube_Map      => 16#851B#);
   for Target_Kind'Size use Low_Level.Enum'Size;

end GL.Objects.Textures.Loader_2D;
