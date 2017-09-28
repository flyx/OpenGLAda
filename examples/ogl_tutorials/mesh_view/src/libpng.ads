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
