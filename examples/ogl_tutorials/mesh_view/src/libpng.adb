with Ada.Text_IO; use Ada.Text_IO;

with PNG_IO;

package body libPNG is

   function max (a, b : Integer) return Integer is
   begin
      if a > b
         then return a;
         else return b;
      end if;
   end max;

   function min (a, b : Integer) return Integer is
   begin
      if a < b
         then return a;
         else return b;
      end if;
   end min;

   -- trunc(+6.66) --> +6 and trunc(-6.66) --> -6
   function trunc (item : GL.Types.Single) return Integer is
   begin
      return Integer (GL.Types.Single'Truncation (item));
   end trunc;

   -- note: the png library uses 0 based indices for rows and columns
   -- subtype Row is Coordinate range 0 .. num_rows - 1;
   -- subtype Col is Coordinate range 0 .. num_cols - 1;

   procedure read_png_dimensions
     (num_rows : out Integer;
      num_cols : out Integer;
      png_file :     String)
   is
      img : PNG_IO.PNG_File;
   begin

      PNG_IO.Open (img, png_file);

      num_cols := PNG_IO.Width (img);
      num_rows := PNG_IO.Height (img);

      PNG_IO.Close (img);

   exception
      when others =>
         Put_Line ("An exception occurred in read_png_dimensions.");
         raise;

   end read_png_dimensions;

   procedure make_colour_table
     (colour_table : out colour_table_array;
      png_file     :     String)
   is
      use GL.Types;
      img      : PNG_IO.PNG_File;
      r, g, b  : Single;
      num_rows : Integer;
      num_cols : Integer;
   begin

      PNG_IO.Open (img, png_file);

      num_cols := PNG_IO.Width (img);
      num_rows := PNG_IO.Height (img);

      for the_row in 0 .. num_rows - 1 loop
         for the_col in 0 .. num_cols - 1 loop

            r := Single (PNG_IO.Red_Value   (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            g := Single (PNG_IO.Green_Value (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            b := Single (PNG_IO.Blue_Value  (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;

            colour_table (the_row, the_col) := (r, g, b);

         end loop;
      end loop;

      PNG_IO.Close (img);

   exception
      when others =>
         Put_Line ("An exception occurred in make_colour_table.");
         raise;

   end make_colour_table;

   function read_colour_table
     (uv           : GL.Types.Singles.Vector2;
      colour_table : colour_table_array)
      return         GL.Types.Singles.Vector3
   is
      use GL.Types;
      num_rows : Integer := colour_table'Last (1) + 1;
      num_cols : Integer := colour_table'Last (2) + 1;
      the_row  : Integer;
      the_col  : Integer;
      u        : Single  := uv (GL.X);  -- u runs across the columns
      v        : Single  := uv (GL.Y);  -- v runs across the rows
   begin

      the_row := max (0, min (num_rows - 1, trunc (v * Single (num_rows))));
      the_col := max (0, min (num_cols - 1, trunc (u * Single (num_cols))));

      return colour_table (the_row, the_col);

   exception
      when others =>
         Put_Line ("An exception occurred in read_colour_table.");
         raise;

   end read_colour_table;

   -- use this pair only for the occassional request for the rgb values
   -- why? because they read the *whole* file for every single call

   function get_rgb
     (uv       : GL.Types.Singles.Vector2;
      png_file : String)
      return     GL.Types.Singles.Vector3
   is
      use GL.Types;
      img          : PNG_IO.PNG_File;
      r, g, b      : Single;
      num_rows     : Integer;
      num_cols     : Integer;
      the_row      : Integer;
      the_col      : Integer;
      u            : Single := uv (GL.X);  -- u runs across the columns
      v            : Single := uv (GL.Y);  -- v runs across the rows
      Colour_Model : PNG_IO.Colour_Type_Code;
   begin

      PNG_IO.Open (img, png_file);

      num_cols := PNG_IO.Width (img);
      num_rows := PNG_IO.Height (img);

      Colour_Model := PNG_IO.Colour_Type (img);

      the_row := max (0, min (num_rows - 1, trunc (v * Single (num_rows))));
      the_col := max (0, min (num_cols - 1, trunc (u * Single (num_cols))));

      case Colour_Model is

         when PNG_IO.Two =>  -- RGB without Alpha channel

            r := Single (PNG_IO.Red_Value   (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            g := Single (PNG_IO.Green_Value (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            b := Single (PNG_IO.Blue_Value  (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;

         when PNG_IO.Six =>  -- RGB with Alpha channel

            r := Single (PNG_IO.Red_Value   (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            g := Single (PNG_IO.Green_Value (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            b := Single (PNG_IO.Blue_Value  (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;

         when others =>

            r := 0.5;
            g := 0.5;
            b := 0.5;

      end case;

      PNG_IO.Close (img);

      return (r, g, b);

   exception
      when others =>
         Put_Line ("An exception occurred in get_rgb.");
         raise;

   end get_rgb;

   function get_rgba
     (uv       : GL.Types.Singles.Vector2;
      png_file : String)
      return     GL.Types.Colors.Color
   is
      use GL.Types;
      img          : PNG_IO.PNG_File;
      r, g, b, a   : Single;
      num_rows     : Integer;
      num_cols     : Integer;
      the_row      : Integer;
      the_col      : Integer;
      u            : Single := uv (GL.X);  -- u runs across the columns
      v            : Single := uv (GL.Y);  -- v runs across the rows
      Colour_Model : PNG_IO.Colour_Type_Code;
   begin

      PNG_IO.Open (img, png_file);

      num_cols := PNG_IO.Width (img);
      num_rows := PNG_IO.Height (img);

      Colour_Model := PNG_IO.Colour_Type (img);

      the_row := max (0, min (num_rows - 1, trunc (v * Single (num_rows))));
      the_col := max (0, min (num_cols - 1, trunc (u * Single (num_cols))));

      case Colour_Model is

         when PNG_IO.Two =>  -- RGB without Alpha channel

            r := Single (PNG_IO.Red_Value   (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            g := Single (PNG_IO.Green_Value (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            b := Single (PNG_IO.Blue_Value  (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            a := 1.0;

         when PNG_IO.Six =>  -- RGB with Alpha channel

            r := Single (PNG_IO.Red_Value   (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            g := Single (PNG_IO.Green_Value (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            b := Single (PNG_IO.Blue_Value  (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;
            a := Single (PNG_IO.Alpha_Value (img, PNG_IO.Coordinate (the_row), PNG_IO.Coordinate (the_col))) / 255.0;

         when others =>

            r := 0.5;
            g := 0.5;
            b := 0.5;
            a := 1.0;

      end case;

      PNG_IO.Close (img);

      return (r, g, b, a);

   exception
      when others =>
         Put_Line ("An exception occurred in get_rgba.");
         raise;

   end get_rgba;

end libPNG;
