-- $Id: png_io-adam7.ads,v 1.4 2009/05/04 19:31:14 sangwine Exp $
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

private package PNG_IO.Adam7 is

   -- A package of functions for handling interlaced images using the Adam7
   -- interlacing scheme which requires 7 passes over the image.

   type Pass_Number is range 1 .. 7;

   -- Pass returns the pass number for a given pixel in the whole image,
   -- assuming that the image is Adam7 interlaced.

   function Pass (R, C : Coordinate) return Pass_Number;

   -- The width or height of a sub-image may be zero for images with fewer
   -- than 5 rows or columns. See the ISO standard, Section 8.2. This is
   -- why the next two functions return Natural, not Dimension.

   function Sub_Image_Width
     (W : Dimension;
      P : Pass_Number)
      return Natural;
   function Sub_Image_Height
     (H : Dimension;
      P : Pass_Number)
      return Natural;

   -- On input, pixels from an interlaced image must be fetched from the decompressed
   -- data buffer by computing the coordinates within the sub-image for the appropriate
   -- pass. The function Pass (above) determines which pass the pixel occurs in, and
   -- the two following functions determine the coordinates within the sub-image.

   function Sub_Image_Row (R, C : Coordinate) return Coordinate;
   function Sub_Image_Col (R, C : Coordinate) return Coordinate;

   -- On output, the Write procedures need to fetch pixels within a pass in
   -- raster sequence within the sub-image. This will not be raster sequence in the
   -- whole image, and the following two functions map from coordinates within
   -- a sub-image of a given pass to the coordinate position in the whole image
   -- which is needed to fetch the pixel value from the user's code.

   function Image_Row
     (R : Coordinate;
      P : Pass_Number)
      return Coordinate;
   function Image_Col
     (C : Coordinate;
      P : Pass_Number)
      return Coordinate;

end PNG_IO.Adam7;
