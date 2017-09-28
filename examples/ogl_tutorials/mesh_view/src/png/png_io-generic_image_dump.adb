-- $Id: png_io-generic_image_dump.adb,v 1.1 2009/05/04 19:21:41 sangwine Exp $
---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO  - Ada95 Portable Network Graphics Input/Output Package  --
--                                                                 --
--                http://png-io.sourceforge.net/                   --
--                                                                 --
--        Copyright (c) 1999-2009 Dr Stephen J. Sangwine           --
--                                S.Sangwine@IEEE.org              --
--                                                                 --
-- This software was created by Stephen J. Sangwine. He hereby     --
-- asserts his Moral Right to be identified as author of this      --
-- software.                                                       --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO is free software; you can redistribute it and/or modify  --
-- it under the terms of the GNU General Public License as         --
-- published by the Free Software Foundation; either version 3 of  --
-- the License, or (at your option) any later version.             --
--                                                                 --
-- PNG_IO is distributed in the hope that it will be useful, but   --
-- WITHOUT ANY WARRANTY; without even the implied warranty of      --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    --
-- GNU General Public License for more details.                    --
--                                                                 --
-- You should have received a copy of the GNU General Public       --
-- License along with this software (in the file gpl.txt).         --
-- If not access  http://www.gnu.org/licenses/                     --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Adapted 4 May 2009 for release as part of PNG_IO based on code  --
-- first written in 1996.                                          --
---------------------------------------------------------------------
---------------------------------------------------------------------

with Ada.Strings.Bounded;

procedure PNG_IO.Generic_Image_Dump is

   W : constant Natural := Natural'Max (Row_Coordinate'Width, Col_Coordinate'Width);

   -- This procedure has two major components.
   --
   -- 1. The package below, which handles the delivery of pixels from the
   --    image, in raster order (implicitly).
   -- 2. The 'main' code which gets pixels from the package and formats
   --    lines of output. Each block of output corresponding to a raster
   --    line in the image is prefixed with its Y coordinate. Each line
   --    of output is prefixed with the X coordinate of the first pixel
   --    displayed.

   package Pixel_Server is

      function Current_X return String;
      function Current_Y return String;
      function Current_Pixel return String;
      function End_of_Scan_Line return Boolean;
      function End_of_Image return Boolean;

      procedure Next_Pixel;

   end Pixel_Server;
   use Pixel_Server;

   package body Pixel_Server is

      Y : Row_Coordinate := Row_Coordinate'First;
      X : Col_Coordinate := Col_Coordinate'First;

      -- The following declarations are complex solely to deal with the cases
      -- where the image has one row and/or one column, otherwise they could
      -- be initialised to False.

      EOL : Boolean := X = Col_Coordinate'Last;
      EOI : Boolean := EOL and then Y = Row_Coordinate'Last;

      function Pad (X : String) return String is
      -- Pads the string with leading blanks to length W.
      begin
         pragma Assert (not (X'Length > W));
         if X'Length = W then
            return X;
         else
            return Pad (' ' & X);
         end if;
      end Pad;

      function Current_X return String is
      begin
         return Pad (Col_Coordinate'Image (X));
      end Current_X;

      function Current_Y return String is
      begin
         return Pad (Row_Coordinate'Image (Y));
      end Current_Y;

      function Current_Pixel return String is
      begin
         return Pixel_Image (Y, X);
      end Current_Pixel;

      function End_of_Scan_Line return Boolean is
      begin
         return EOL;
      end End_of_Scan_Line;
      function End_of_Image return Boolean is
      begin
         return EOI;
      end End_of_Image;

      procedure Next_Pixel is
         use type Row_Coordinate;
         use type Col_Coordinate;
      begin
         if EOI then
            raise Program_Error;
         end if;
         if not EOL then
            X   := Col_Coordinate'Succ (X);
            EOL := X = Col_Coordinate'Last;
            EOI := EOL and then Y = Row_Coordinate'Last;
         else
            X   := Col_Coordinate'First;
            EOL := False;
            Y   := Row_Coordinate'Succ (Y);
         end if;
      end Next_Pixel;

   end Pixel_Server;

   package Dump_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Width);
   use Dump_Strings;

   L : Bounded_String; -- Used to construct each output line.

   -- We need to know how many columns of output a pixel image requires.
   -- This is required to be constant, regardless of the pixel value. To
   -- find it out we read the current pixel (which will be the first pixel
   -- of the image) and construct its image, adding in the width of the
   -- separator to allow for the spacing between pixel values in the output.

   Pixel_Width : constant Positive := Separator'Length + Current_Pixel'Length;

   -- Verify that the page is wide enough to hold at least one pixel,
   -- otherwise a string length error will occur. W is the width of the
   -- coordinate image, plus 2 for the spaces inserted below.

   pragma Assert (2 * W + 2 + Pixel_Width <= Width);

begin
   loop -- Until whole image has been output.
      loop -- Until end of scan line.
         -- Prefix the line with the Y and X coordinates of the first pixel.
         L := To_Bounded_String (Current_Y & ' ' & Current_X & ' ');
         loop -- Until output line full.
            L := L & Separator & Current_Pixel; -- Add the first pixel value.
            exit when Length (L) + Pixel_Width > Width -- No more room on line
            or End_of_Scan_Line;            -- or end of raster line.
            Next_Pixel;
         end loop;
         Dump_Line (To_String (L));
         exit when End_of_Scan_Line;
         Next_Pixel;
      end loop;
      exit when End_of_Image;
      Next_Pixel;
   end loop;
   return;
end PNG_IO.Generic_Image_Dump;
