-- $Id: png_io-base.adb,v 1.4 2009/05/10 17:29:52 sangwine Exp $
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
-- Created: 4 May 2009 with code copied from other PNG_IO files.   --
---------------------------------------------------------------------
---------------------------------------------------------------------

package body PNG_IO.Base is

   function To_Chunk_Name (C : Unsigned_32) return Chunk_Name is
   begin
      return Character'Val ((Shift_Right (C, 24) and 16#FF#)) &
        Character'Val ((Shift_Right (C, 16) and 16#FF#)) &
        Character'Val ((Shift_Right (C, 8) and 16#FF#)) &
        Character'Val (C and 16#FF#);
   end To_Chunk_Name;

   function To_Hex_String
     (Value : Natural;
      Width : Natural)
      return String
   is
      Hex_Char : constant array (0 .. 15) of Character := "0123456789ABCDEF";
   begin
      if Width = 0 then
         pragma Assert (Value = 0);
         return "";
      else
         return To_Hex_String (Value / 16, Width - 1) & Hex_Char (Value rem 16);
      end if;
   end To_Hex_String;

   function Colour_Type_String (T : Colour_Type_Code) return String is
   -- The strings returned are taken from the W3C/ISO standard, Table 6.1.
   begin
      case T is
         when Zero =>
            return "Greyscale";
         when Two =>
            return "Truecolour";
         when Three =>
            return "Indexed-colour";
         when Four =>
            return "Greyscale with alpha";
         when Six =>
            return "Truecolour with alpha";
      end case;
   end Colour_Type_String;

   -------------------------------------------------------------------------
   -- Verify the integrity of the chunk type codes declared in the package
   -- specification. Pragma Assert is not valid Ada95, but will be ignored
   -- by a compiler that does not recognise it. An Ada2005 compiler will
   -- recognise the pragma.

   pragma Assert (To_Chunk_Name (IHDR) = "IHDR");
   pragma Assert (To_Chunk_Name (PLTE) = "PLTE");
   pragma Assert (To_Chunk_Name (IDAT) = "IDAT");
   pragma Assert (To_Chunk_Name (IEND) = "IEND");
   pragma Assert (To_Chunk_Name (cHRM) = "cHRM");
   pragma Assert (To_Chunk_Name (gAMA) = "gAMA");
   pragma Assert (To_Chunk_Name (sRGB) = "sRGB");
   pragma Assert (To_Chunk_Name (pHYs) = "pHYs");
   pragma Assert (To_Chunk_Name (tEXt) = "tEXt");
   pragma Assert (To_Chunk_Name (zTXt) = "zTXt");
   pragma Assert (To_Chunk_Name (tRNS) = "tRNS");
   pragma Assert (To_Chunk_Name (iCCP) = "iCCP");
   pragma Assert (To_Chunk_Name (iTXt) = "iTXt");
   pragma Assert (To_Chunk_Name (bKGD) = "bKGD");
   pragma Assert (To_Chunk_Name (sBIT) = "sBIT");
   pragma Assert (To_Chunk_Name (sPLT) = "sPLT");
   pragma Assert (To_Chunk_Name (hIST) = "hIST");
   pragma Assert (To_Chunk_Name (tIME) = "tIME");

end PNG_IO.Base;
