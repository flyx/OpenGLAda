-- $Id: png_io-base.ads,v 1.4 2009/05/10 17:28:39 sangwine Exp $
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
-- Created: 4 May 2009                                             --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- This package contains basic items and functions that are used   --
-- within the PNG_IO package or the utility programs distributed   --
-- with it. The intent in placing them here is to allow their easy --
-- use to extend PNG_IO.                                           --
---------------------------------------------------------------------
---------------------------------------------------------------------

with Interfaces; use Interfaces;

package PNG_IO.Base is

   -- PNG files start with an 8-byte signature (ISO/W3C standard sec. 5.2).

   PNG_Signature : constant Stream_Element_Array := (16#89#, 16#50#, 16#4E#, 16#47#, 16#0D#, 16#0A#, 16#1A#, 16#0A#);

   -------------------------------------------------------------------------

   -- A PNG file can contain various chunks, each with a 4-byte chunk type
   -- code. The chunk type codes are defined here numerically, so that they
   -- are static, but they are verified at the end of the package body so
   -- that errors in the coding of the numerical values are trapped. If any
   -- new codes are added the verification code must also be added. PNG type
   -- codes are defined in terms of ISO 8859-1 Latin-1 character codes as
   -- are the codes for the Ada95 package Standard listed in LRM Section A.1
   -- from which the following table is derived:
   --
   --      x = 0123456789ABCDEF
   -- 16#4x#    ABCDEFGHIJKLMNO
   -- 16#5x#   PQRSTUVWXYZ
   -- 16#6x#    abcdefghijklmno
   -- 16#7x#   pqrstuvwxyz

   IHDR : constant := 16#49484452#;
   PLTE : constant := 16#504C5445#;
   IDAT : constant := 16#49444154#;
   IEND : constant := 16#49454E44#;

   tRNS : constant := 16#74524E53#;
   gAMA : constant := 16#67414D41#;
   cHRM : constant := 16#6348524D#;
   sRGB : constant := 16#73524742#;
   iCCP : constant := 16#69434350#;
   tEXt : constant := 16#74455874#;
   zTXt : constant := 16#7A545874#;
   iTXt : constant := 16#69545874#;

   bKGD : constant := 16#624B4744#;
   pHYs : constant := 16#70485973#;
   sBIT : constant := 16#73424954#;
   sPLT : constant := 16#73504C54#;
   hIST : constant := 16#68495354#;
   tIME : constant := 16#74494D45#;

   --------------------------------

   -- The IEND chunk in any PNG file contains the same 12 bytes, since the
   -- data length is zero, and therefore the CRC always has the same value.
   -- The 12 bytes here are the entire content of the IEND chunk. They are
   -- represented as a Stream_Element_Array so that the 12 bytes can be
   -- directly written to a PNG file.

   IEND_Chunk : constant Stream_Element_Array :=
     (16#00#, 16#00#, 16#00#, 16#00#,  -- Size = 0.
     16#49#, 16#45#, 16#4E#, 16#44#,  -- IEND.
     -- No data bytes.
   16#AE#, 16#42#, 16#60#, 16#82#); -- CRC.

   -------------------------------------------------------------------------

   -- Look up tables for converting bit depths and colour type codes to
   -- numeric values.

   Bit_Depth_Table : constant array (Depth) of Stream_Element := (1, 2, 4, 8, 16);

   Colour_Type_Table : constant array (Colour_Type_Code) of Stream_Element := (0, 2, 3, 4, 6);

   -------------------------------------------------------------------------

   -- The number of hexadecimal characters needed to represent a pixel value
   -- of a given bit depth.

   Hex_Width_Table : constant array (Depth) of Positive := (1, 1, 1, 2, 4);

   -------------------------------------------------------------------------

   -- Convert a numerical representation of a chunk name to a 4-character
   -- string. Example: 16#49454E44# gives "IEND" (see IEND_Chunk above).

   function To_Chunk_Name (C : Unsigned_32) return Chunk_Name;

   -------------------------------------------------------------------------

   -- Convert a natural value to a hexadecimal string.

   function To_Hex_String
     (Value : Natural;
      Width : Natural)
      return String;

   -------------------------------------------------------------------------

   -- Return a brief description of the colour type for use in dump output.

   function Colour_Type_String (T : Colour_Type_Code) return String;

   -- Add here functions to convert to and from Buffer2/4 and Unsigned?
   -- plus the declarations of Buffer_2 etc? See png_io.open.adb 125 and
   -- png_io.adb 274.

private

   -- There are certain assumptions made in the design of PNG_IO. These are
   -- verified here, because this base package is used by PNG_IO itself and
   -- also by utility programs bundled with the package that do not 'with'
   -- the main PNG_IO package.

   -- PNG_IO assumes that Natural has a large enough range to be able to
   -- pass 32-bit values such as image dimensions from PNG files. PNG
   -- four-byte unsigned integers are limited to the range 0 to 2**31 - 1,
   -- so we verify that Natural can support this range.
   -- (The LRM requires the minimum range for Integer to be 16-bits, so it
   -- is not guaranteed that Natural has a large enough range.)

   pragma Assert (Natural'Last >= 2**31 - 1);

   -- The type Stream_Element is defined in Ada.Streams as
   -- "mod implementation defined". The most likely possibility is a byte,
   -- i.e. mod 2**8, on all modern systems, but it is not guaranteed that
   -- this will be so. If Stream_Element is not a byte, PNG_IO will not
   -- compile and work without modification (neither will Zlib_Ada, so this
   -- would be a fairly serious problem).

   pragma Assert (Stream_Element'Size = 8);
   pragma Assert (Stream_Element'Modulus = 2**8);

end PNG_IO.Base;
