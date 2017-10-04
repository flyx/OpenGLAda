-- $Id: png_io-generic_image_dump.ads,v 1.1 2009/05/04 19:21:41 sangwine Exp $
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
-- Generic image dump utility. Produces a dump of an image using a --
-- user-supplied function to supply images of the pixel values (in --
-- the sense of the Ada attribute 'Image).                         --
---------------------------------------------------------------------
---------------------------------------------------------------------

generic

   -- Images are 'passed' into this package without any knowledge of
   -- how the images are stored or represented.

   type Row_Coordinate is (<>); -- The coordinate index types may be the
   type Col_Coordinate is (<>); -- same, but need not be. The image is
   -- assumed to have Row'First .. Row'Last
   -- rows and similarly for columns.

   -- The function below must produce a printable character representation
   -- of a pixel, and must always return the same length string, otherwise
   -- the formatting of the dump will go haywire. The string returned
   -- should not have any leading or trailing blanks.

   with function Pixel_Image
     (R : Row_Coordinate;
      C : Col_Coordinate)
      return String is <>;

   -- The procedure below will be called once for each line in the dump
   -- (a callback procedure.) After the last line has been passed back,
   -- the (instantiated) procedure itself will return. The strings passed
   -- back do not have any end-of-line characters.

   with procedure Dump_Line (S : in String);

   Width : Positive := 80; -- The maximum length of a line of dump.

   Separator : String := " "; -- This allows the invoking code to control
   -- the spacing of the dump, for example, by
   -- supplying two spaces here instead of the
   -- default of one.

procedure PNG_IO.Generic_Image_Dump;
