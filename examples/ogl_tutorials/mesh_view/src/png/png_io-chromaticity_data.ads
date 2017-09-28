-- $Id: png_io-chromaticity_data.ads,v 1.3 2009/05/04 19:31:14 sangwine Exp $
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
-- This package defines some standard chromaticity data for use in --
-- constructing chromaticity chunks. The values given here are     --
-- (x, y) chromaticity coordinates scaled by 100_000, so that they --
-- are in the form required for the parameters of the function     --
-- PNG_IO.Chromaticity_Chunk.                                      --
---------------------------------------------------------------------
---------------------------------------------------------------------

package PNG_IO.Chromaticity_Data is

   -- CIE standardized whitepoints, taken from:
   --
   -- Sproson, W. N., Colour Science in Television and Display Systems,
   --                 Adam Hilger, Bristol, 1983, p29,32.

   C   : constant Pair := (31_010, 31_620);
   D65 : constant Pair := (31_270, 32_900);

   -- RGB television system primaries.

   EBU_R : constant Pair := (64_000, 33_000); -- Given by Sproson (ibid)
   EBU_G : constant Pair := (29_000, 60_000); -- p32.
   EBU_B : constant Pair := (15_000, 6_000);

   FCC_R : constant Pair := (67_000, 33_000); -- Given by Sproson (ibid)
   FCC_G : constant Pair := (21_000, 71_000); -- p31.
   FCC_B : constant Pair := (14_000, 8_000);

   -- The CIE chromaticity primaries and whitepoint are taken from:
   --
   -- Palus, H., Colour Spaces, in Sangwine and Horne (eds), The Colour
   --            Image Processing Handbook, Chapman and Hall, 1998.

   CIE_W : constant Pair := (33_300, 33_300);

   CIE_R : constant Pair := (73_500, 26_500);
   CIE_G : constant Pair := (27_400, 71_700);
   CIE_B : constant Pair := (16_700, 900);

   -- The following primary chromaticities are for ITU-R BT.709 and sRGB.
   -- The values are taken from the PNG Specification V1.2 and agree with
   -- those in the draft sRGB standard.

   BT709_R : constant Pair := (64_000, 33_000);
   BT709_G : constant Pair := (30_000, 60_000);
   BT709_B : constant Pair := (15_000, 6_000);

end PNG_IO.Chromaticity_Data;
