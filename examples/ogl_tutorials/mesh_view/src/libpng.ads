-- Copyright (c) 2017, Leo Brewin <Leo.Brewin@monash.edu>
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

with GL;
with GL.Types;
with GL.Types.Colors;

package libPNG is

   type colour_table_array is array (Integer range <>, Integer range <>) of GL.Types.Singles.Vector3;

   procedure read_png_dimensions
     (num_rows : out Integer;
      num_cols : out Integer;
      png_file :     String);

   procedure make_colour_table
     (colour_table : out colour_table_array;
      png_file     :     String);

   function read_colour_table
     (uv           : GL.Types.Singles.Vector2;
      colour_table : colour_table_array)
      return         GL.Types.Singles.Vector3;

   function get_rgb
     (uv       : GL.Types.Singles.Vector2;
      png_file : String)
      return     GL.Types.Singles.Vector3;

   function get_rgba
     (uv       : GL.Types.Singles.Vector2;
      png_file : String)
      return     GL.Types.Colors.Color;

end libPNG;
