-- $Id: png_io.adb,v 1.14 2011/11/23 21:41:07 sangwine Exp $
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

with Ada.Exceptions; use Ada.Exceptions;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Ada.Unchecked_Deallocation, Ada.Characters.Latin_1, Ada.Characters.Handling;

with Interfaces; use Interfaces;

with PNG_IO.Adam7; use PNG_IO.Adam7;

with PNG_IO.Base; use PNG_IO.Base;

with PNG_IO.Chromaticity_Data;

package body PNG_IO is

   -- 12 August 2006 : the way the version number is declared was changed in order
   -- to make PNG_IO comply with the Ravenscar profile (at least under Gnat/gcc -
   -- since there are some implementation dependencies in this profile). Thanks
   -- to Samuel Tardieu for pointing out that PNG_IO was almost Ravenscar compliant
   -- and that only a small change would be needed to make it so.

   Version_String : constant String := "4.5.1";

   function Version return String is
   begin
      return Version_String;
   end Version;

   ---------------------------------------------------------

   function Valid_Zlib_Header (CMG, FLG : in Stream_Element) return Boolean is
   -- This function checks the data in the first two bytes of a Zlib
   -- data stream, supplied as the two parameters. (ISO standard, section 10.1.)
   -- We check the least significant 4 bits of CMG (the first byte)
   -- and that the overall 16-bit value is a multiple of 31 as required
   -- by RFC1950 (the Zlib Compressed Data Format Specification).
   begin
      return (CMG and 2#1111#) = 8 and (Unsigned_16 (CMG) * 256 + Unsigned_16 (FLG)) mod 31 = 0;
   end Valid_Zlib_Header;

   ----------------------------------------------------------------------
   -- PNG files written by this package contain the following text in a
   -- tEXt chunk to indicate the software that wrote the chunk.

   Package_Identifier : constant String :=
     "Software" & Ada.Characters.Latin_1.NUL & "PNG_IO v" & Version_String & " by Steve Sangwine (S.Sangwine@IEEE.org).";

   -------------------------------------------------------------------------
   IDAT_Size : constant := 8 * 2**10; -- This determines the maximum size of
   -- IDAT chunk that will be output.
   -------------------------------------------------------------------------

   function To_Unsigned_32 (N : Chunk_Name) return Unsigned_32 is
      B1 : constant Unsigned_32 := Shift_Left (Character'Pos (N (1)), 24);
      B2 : constant Unsigned_32 := Shift_Left (Character'Pos (N (2)), 16);
      B3 : constant Unsigned_32 := Shift_Left (Character'Pos (N (3)), 8);
      B4 : constant Unsigned_32 := Character'Pos (N (4));
   begin
      return B1 or B2 or B3 or B4;
   end To_Unsigned_32;

   -------------------------

   package Chunk_Ordering is

      -- An encoding/LUT of the rules laid out in the ISO standard
      -- (Section 5.6 and Table 5.3). This should not be confused with the
      -- three positions available to the user of PNG_IO when writing a
      -- file (before the PLTE, between PLTE and IDAT, and after IDAT).

      function Known_Chunk (C : Unsigned_32) return Boolean;

      function Before_PLTE (C : Unsigned_32) return Boolean;
      --function  After_PLTE(C : Unsigned_32) return Boolean; -- Not currently used.
      function Before_IDAT (C : Unsigned_32) return Boolean;

   end Chunk_Ordering;
   use Chunk_Ordering;

   package body Chunk_Ordering is separate;

   ----------------------------------------

   function Known_Chunk (Name : Chunk_Name) return Boolean is
   begin
      return Known_Chunk (To_Unsigned_32 (Name));
   end Known_Chunk;

   function Position (C : Unsigned_32) return Chunk_Position is
   -- Gives the position in terms of the three choices available
   -- to the user. The choice is a priority encoding of the positions
   -- defined in the PNG specification.
   begin
      if Before_PLTE (C) then
         return Before_PLTE;
      elsif Before_IDAT (C) then
         return Before_IDAT; -- This means after PLTE if it exists.
      else
         return Anywhere;
      end if;
   end Position;

   function Position (Name : Chunk_Name) return Chunk_Position is
      C : constant Unsigned_32 := To_Unsigned_32 (Name);
   begin
      if Known_Chunk (C) then
         return Position (C);
      else
         raise Argument_Error;
      end if;
   end Position;

   -------------------------------------------------------------

   -- We also need to be able to check other chunk types to make
   -- sure they are ancillary (bit 5 of the first byte is set).

   function Ancillary (C : Unsigned_32) return Boolean is
   begin
      return (C and 2#00100000_00000000_00000000_00000000#) /= 0;
   end Ancillary;

   function Safe_to_Copy (C : Unsigned_32) return Boolean is
   -- An internal function which tests bit 5 of the least significant
   -- byte of a chunk type (the safe to copy bit).
   begin
      return (C and 2#00000000_00000000_00000000_00100000#) /= 0;
   end Safe_to_Copy;
   pragma Inline (Safe_To_Copy);

   function Safe_to_Copy (C : Chunk) return Boolean is
   -- The function visible outside the package.
   begin
      return Safe_to_Copy (To_Unsigned_32 (C.Name));
   end Safe_to_Copy;

   ------------------------------------

   -- Define the PNG filter type codes.

   None    : constant := 0;
   Sub     : constant := 1;
   Up      : constant := 2;
   Average : constant := 3;
   Paeth   : constant := 4;

   -------------------------------------------------------------------------------------------

   type Palette_Data is array (Unsigned_8 range <>) of Unsigned_8;

   type Colour_Palette (Size : Unsigned_8) is record
      R, G, B : Palette_Data (0 .. Size);
   end record;

   type Palette_Pointer is access Colour_Palette;

   subtype Buffer is Stream_Element_Array;

   type Buffer_Pointer is access Buffer;

   subtype Buffer_2 is Buffer (1 .. 2); -- I.e. 2 bytes, 16 bits.
   subtype Buffer_4 is Buffer (1 .. 4); -- I.e. 4 bytes, 32 bits.

   function To_Buffer_2 (U : Unsigned_16) return Buffer_2 is
   begin
      return Stream_Element (Shift_Right (U and 16#FF00#, 8)) & Stream_Element (U and 16#00FF#);
   end To_Buffer_2;

   function To_Buffer_4 (U : Unsigned_32) return Buffer_4 is
      B : Buffer_4;
      L : Unsigned_32 := U;
   begin
      for I in reverse B'Range loop
         B (I) := Stream_Element (L and 16#FF#);
         exit when I = B'First;
         L := Shift_Right (L, 8);
      end loop;
      return B;
   end To_Buffer_4;

   -- The Text_Item type is used to store a keyword/text string pair and a
   -- link pointer. Text strings are stored in a linked list with the
   -- head pointer in the PNG_File_Descriptor. This is awkward, but
   -- since the number of these strings is not defined, and they are
   -- of variable length, it is difficult to see what else to do.

   type String_Pointer is access String;

   type Text_Item;
   type Text_Item_Pointer is access Text_Item;

   type Text_Item is record
      Keyword     : String_Pointer;
      Text_String : String_Pointer;
      Link        : Text_Item_Pointer;
   end record;

   type Pass_Offsets is array (Pass_Number) of Stream_Element_Offset;

   type PNG_File_Descriptor is record
      Handle                                                 : File_Type;
      Stream                                                 : Stream_Access;
      Width, Height                                          : Dimension;
      Bit_Depth, Colour_Type, Compression, Filter, Interlace : Unsigned_8;

      Gamma                                                            : Boolean := False;
      Gamma_Value                                                      : Unsigned_32;
      Chroma                                                           : Boolean := False;
      White_X, White_Y, Red_X, Red_Y, Green_X, Green_Y, Blue_X, Blue_Y : Unsigned_32;
      SRGB                                                             : Boolean := False;
      Rendering                                                        : Rendering_Intent;
      Physical                                                         : Boolean := False;
      Phys_X, Phys_Y                                                   : Unsigned_32;
      Phys_Unit                                                        : Unsigned_8;
      Number_of_Texts                                                  : Natural := 0;
      Text_Strings                                                     : Text_Item_Pointer;
      Number_of_Chunks                                                 : Natural := 0; -- Unrecognised ancillary chunks
      Ancillary_Chunks                                                 : Chunk_List;   -- are tacked on here as a list.

      Palette           : Palette_Pointer;
      Uncompressed_Data : Buffer_Pointer;
      Interlace_Offsets : Pass_Offsets;
   end record;

   procedure Deallocate is new Ada.Unchecked_Deallocation (Buffer, Buffer_Pointer);

   function To_Stream_Element_Array (S : String) return Stream_Element_Array is
      B : Stream_Element_Array (1 .. S'Length);
      I : Stream_Element_Offset := B'First;
      J : Positive              := S'First;
   begin
      loop
         B (I) := Stream_Element (Character'Pos (S (J)));
         exit when J = S'Last;
         I := I + 1;
         J := J + 1;
      end loop;
      return B;
   end To_Stream_Element_Array;

   To_Stream_Element : constant array (Boolean) of Stream_Element := (0, 1);

   procedure Check (F : in PNG_File) is
   -- Checks that the file descriptor F exists.
   begin
      if F = null then
         Raise_Exception (Call_Error'Identity, "Attempt to access non-existent file descriptor.");
      end if;
   end Check;
   pragma Inline (Check);

   function Bits_per_Pixel
     (Colour_Type : Colour_Type_Code;
      Bit_Depth   : Depth)
      return Positive
   is
      -- The number of bits per pixel in the IDAT chunks. This is not
      -- always the same as the number of bits per pixel in the image.
      -- The only exception is that there are 24 bits per pixel for
      -- images of colour type 3 (palette colour).
      BD : constant Positive := Positive (Bit_Depth_Table (Bit_Depth));
   begin
      case Colour_Type is
         when Zero =>
            return BD;     -- Greyscale.
         when Two =>
            return BD * 3; -- RGB
         when Three =>
            return BD;     -- Palette colour.
         when Four =>
            return BD * 2; -- Greyscale + alpha.
         when Six =>
            return BD * 4; -- RGB + alpha.
      end case;
   end Bits_per_Pixel;
   pragma Inline (Bits_per_Pixel);

   function Bytes_per_Pixel
     (Colour_Type : Colour_Type_Code;
      Bit_Depth   : Depth)
      return Positive
   is
   -- The number of bytes in each pixel rounded up to 1 for bit depths
   -- less than 8. Used in filtering scanlines where it defines the
   -- offset between one byte and the corresponding byte from the next
   -- pixel, or containing the next pixel.
   begin
      return Positive'Max (1, (Bits_per_Pixel (Colour_Type, Bit_Depth)) / 8);
   end Bytes_per_Pixel;
   pragma Inline (Bytes_per_Pixel);

   function Bytes_per_Scanline
     (Colour_Type    : Colour_Type_Code;
      Bit_Depth      : Depth;
      Scanline_Width : Dimension)
      return Stream_Element_Count
   is
   -- The number of bytes per scanline (excluding the filter type
   -- byte) must allow for pixels of less than 8 bits with image
   -- widths which do not result in an integral number of bytes.
   -- We round up the number of bytes by adding 7 to the number
   -- of bits.
   begin
      return Stream_Element_Count (Bits_per_Pixel (Colour_Type, Bit_Depth) * Scanline_Width + 7) / 8;
   end Bytes_per_Scanline;
   pragma Inline (Bytes_per_Scanline);

   function Image_Size
     (Colour_Type : Colour_Type_Code;
      Bit_Depth   : Depth;
      X, Y        : Dimension;
      Interlaced  : Boolean)
      return Stream_Element_Count
   is

      -- Computes the size of image data in bytes, taking account of
      -- wasted bits, filter type bytes and interlacing, for use in
      -- allocating buffers.

      function Sub_Image_Size (W, H : Natural) return Stream_Element_Count is
      begin
         if W = 0 or H = 0 then
            return 0; -- Empty pass: see PNG Specification Section 2.6.
         else
            -- The + 1 in the next line is to allow for the filter type byte.
            return Stream_Element_Count (H) * (Bytes_per_Scanline (Colour_Type, Bit_Depth, W) + 1);
         end if;
      end Sub_Image_Size;
      pragma Inline (Sub_Image_Size);

   begin
      if not Interlaced then
         return Sub_Image_Size (X, Y);
      else
         declare
            R : Stream_Element_Count := 0;
         begin
            for P in Pass_Number loop
               R := R + Sub_Image_Size (Sub_Image_Width (X, P), Sub_Image_Height (Y, P));
            end loop;
            return R;
         end;
      end if;
   end Image_Size;

   function Interlaced (F : PNG_File) return Boolean is
   begin
      return F.Interlace = 1; -- Simplified 26 May 2002. The byte can only be 0 or 1.
   end Interlaced;

   function Mean (X, Y : Stream_Element) return Stream_Element is
      -- Function to compute the mean value used in the Average
      -- filter described in the ISO standard, Section 9.2 and Table 9.1.
      type Nine_Bit is mod 2**9;
   begin
      return Stream_Element ((Nine_Bit (X) + Nine_Bit (Y)) / 2);
   end Mean;
   pragma Inline (Mean);

   function PaethPredictor (A, B, C : Stream_Element) return Stream_Element is
      -- This code is based on the pseudocode given in the ISO standard, Section 9.4.
      P  : constant Integer := Integer (A) + Integer (B) - Integer (C);
      PA : constant Integer := abs (P - Integer (A));
      PB : constant Integer := abs (P - Integer (B));
      PC : constant Integer := abs (P - Integer (C));
   begin
      if PA <= PB and PA <= PC then
         return A;
      elsif PB <= PC then
         return B;
      else
         return C;
      end if;
   end PaethPredictor;
   pragma Inline (PaethPredictor);

   function Width (F : PNG_File) return Dimension is
   begin
      Check (F);
      return F.Width;
   end Width;

   function Height (F : PNG_File) return Dimension is
   begin
      Check (F);
      return F.Height;
   end Height;

   function Bit_Depth (F : PNG_File) return Depth is
   begin
      Check (F);
      case F.Bit_Depth is
         when 1 =>
            return One;
         when 2 =>
            return Two;
         when 4 =>
            return Four;
         when 8 =>
            return Eight;
         when 16 =>
            return Sixteen;
         when others => -- Since F.Bit_Depth was validated when the IHDR chunk
            -- was read by Open, this simply should not happen. So
            raise Program_Error;
      end case;
   end Bit_Depth;

   function Sample_Depth (F : PNG_File) return Positive is
   begin
      if Colour_Type (F) /= Three then
         return Positive (Bit_Depth_Table (Bit_Depth (F)));
      else        -- The image is a palette image and the number of
         return 8; -- bits per sample is independent of bit depth.
      end if;
   end Sample_Depth;

   function Colour_Type (F : PNG_File) return Colour_Type_Code is
   begin
      Check (F);
      declare
         T : Unsigned_8 renames F.Colour_Type;
      begin
         if T = 0 then
            return Zero;
         elsif T = 2 then
            return Two;
         elsif T = 3 then
            return Three;
         elsif T = 4 then
            return Four;
         elsif T = 6 then
            return Six;
         else
            raise Program_Error;
         end if;
      end;
   end Colour_Type;

   function Palette (F : PNG_File) return Boolean is
   -- The test here was modified 29 June 2004. Previously
   -- it checked for a colour type of 3 rather than for the
   -- presence of a palette. (Colour types 2 and 6 can have
   -- an optional palette.)
   begin
      Check (F);
      return F.Palette /= null;
   end Palette;

   function Palette_Size (F : PNG_File) return Positive is
   begin
      return Positive (Natural (F.Palette.Size) + 1);
   end Palette_Size;

   -- Palette indices run from 0 .. Size - 1;

   function Palette_R_Value
     (F     : PNG_File;
      Index : Natural)
      return Natural
   is
   begin
      if Unsigned_8 (Index) <= F.Palette.Size then
         return Natural (F.Palette.R (Unsigned_8 (Index)));
      else
         Raise_Exception (Call_Error'Identity, "Palette index out of range.");
      end if;
   end Palette_R_Value;

   function Palette_G_Value
     (F     : PNG_File;
      Index : Natural)
      return Natural
   is
   begin
      if Unsigned_8 (Index) <= F.Palette.Size then
         return Natural (F.Palette.G (Unsigned_8 (Index)));
      else
         Raise_Exception (Call_Error'Identity, "Palette index out of range.");
      end if;
   end Palette_G_Value;

   function Palette_B_Value
     (F     : PNG_File;
      Index : Natural)
      return Natural
   is
   begin
      if Unsigned_8 (Index) <= F.Palette.Size then
         return Natural (F.Palette.B (Unsigned_8 (Index)));
      else
         Raise_Exception (Call_Error'Identity, "Palette index out of range.");
      end if;
   end Palette_B_Value;

   procedure Open (F : in out PNG_File; Filename : in String) is separate;

   procedure Close (F : in out PNG_File) is
      -- Called by the user but also in the event of an exception during Open.
      procedure Deallocate is new Ada.Unchecked_Deallocation (Colour_Palette, Palette_Pointer);
      procedure Deallocate is new Ada.Unchecked_Deallocation (Text_Item, Text_Item_Pointer);
      procedure Deallocate is new Ada.Unchecked_Deallocation (PNG_File_Descriptor, PNG_File);
      procedure Deallocate is new Ada.Unchecked_Deallocation (Chunk_List_Element, Chunk_List);
   begin
      if F = null then
         -- The file is not open or has not been opened (it might already have been closed.)
         -- The standard packages such as Ada.Direct_IO raise Status_Error in this situation
         -- so we do the same here. We cannot allow this to go undetected, otherwise we will
         -- get exceptions in the following code when we attempt to access elements of the
         -- record (not) accessed by F.
         Raise_Exception (Status_Error'Identity, "Attempt to close non-open file.");
      end if;
      -- Deallocate has no effect if the access value is null. LRM 13.11.2(8)
      Deallocate (F.Palette);
      Deallocate (F.Uncompressed_Data);
      while F.Ancillary_Chunks /= null loop
         declare
            Temp : constant Chunk_List := F.Ancillary_Chunks.Link;
         begin
            Deallocate (F.Ancillary_Chunks);
            F.Ancillary_Chunks := Temp;
         end;
      end loop;
      while F.Text_Strings /= null loop
         declare
            Temp : constant Text_Item_Pointer := F.Text_Strings.Link;
         begin
            Deallocate (F.Text_Strings);
            F.Text_Strings := Temp;
         end;
      end loop;
      if Is_Open (F.Handle) then
         Close (F.Handle);
      end if;
      Deallocate (F);
   end Close;

   function Pixel_Index
     (F    : PNG_File;
      R, C : Coordinate)
      return Stream_Element_Offset
   is
   -- A function to compute the index in the uncompressed data buffer
   -- of the first byte containing the pixel at position R, C. Note
   -- that the pixel itself may occupy more or less than a whole byte.
   begin
      Check (F); -- Make sure F exists.
      if R + 1 > F.Height or C + 1 > F.Width then
         Raise_Exception (Constraint_Error'Identity, "Coordinate(s) out of range.");
      end if;
      declare
         function Index
           (R, C : Coordinate;
            W    : Dimension)
            return Stream_Element_Offset
         is
            -- Computes the index of the desired byte within the image or sub-image.
            CT  : constant Colour_Type_Code := Colour_Type (F);
            BD  : constant Depth            := Bit_Depth (F);
            Bpp : constant Positive         := Bits_per_Pixel (CT, BD);
         begin
            return 1 +
              Stream_Element_Offset (R) * Stream_Element_Offset (Bytes_per_Scanline (CT, BD, W) + 1) +
              1 +
              (Stream_Element_Offset (C) * Stream_Element_Offset (Bpp)) / 8;
         end Index;
         pragma Inline (Index);

         W : constant Dimension := Width (F);
      begin
         if not Interlaced (F) then
            return Index (R, C, W);
         else
            declare
               P : constant Pass_Number := Pass (R, C);
            begin
               return F.Interlace_Offsets (P) - 1 + Index (Sub_Image_Row (R, C), Sub_Image_Col (R, C), Sub_Image_Width (W, P));
            end;
         end if;
      end;
   end Pixel_Index;
   pragma Inline (Pixel_Index);

   -- Low-level functions to fetch the byte/word at a given coordinate in the image.
   -- The offset allows for colour and alpha bytes/words to be fetched.

   function U8
     (F      : PNG_File;
      R, C   : Coordinate;
      Offset : Stream_Element_Offset := 0)
      return Unsigned_8
   is
   begin
      return Unsigned_8 (F.Uncompressed_Data (Pixel_Index (F, R, C) + Offset));
   end U8;
   pragma Inline (U8);

   function U16
     (F      : PNG_File;
      R, C   : Coordinate;
      Offset : Stream_Element_Offset := 0)
      return Unsigned_16
   is
      P : constant Stream_Element_Offset := Pixel_Index (F, R, C) + Offset;
      D : Stream_Element_Array renames F.Uncompressed_Data.all;
   begin
      return Shift_Left (Unsigned_16 (D (P)), 8) or Unsigned_16 (D (P + 1));
   end U16;
   pragma Inline (U16);

   -- Low-level function to fetch bits, half nibbles or nibbles at a given coordinate
   -- in the image. Since the leftmost pixels are in the high order bits of the byte
   -- ISO standard, Section 7.2), we have to reverse the position by subtraction from 7/3/1.
   -- Modified 18 July 2000 to change BD from Depth_1_2_4 to Unsigned_8 for efficiency.

   function U1_2_4
     (F    : PNG_File;
      R, C : Coordinate;
      BD   : Unsigned_8)
      return Unsigned_8
   is
      B : constant Unsigned_8                    := U8 (F, R, C); -- The byte containing the pixel.
      T : constant array (Boolean) of Coordinate := (False => C, True => Sub_Image_Col (R, C));
      K : constant Coordinate                    := T (Interlaced (F));
   begin
      case BD is
         when 1 =>
            return Shift_Right (B, 7 - (K rem 8)) and 2#0000_0001#;
         when 2 =>
            return Shift_Right (B, 2 * (3 - (K rem 4))) and 2#0000_0011#;
         when 4 =>
            return Shift_Right (B, 4 * (1 - (K rem 2))) and 2#0000_1111#;
         when others =>
            raise Program_Error;
      end case;
   end U1_2_4;
   pragma Inline (U1_2_4);

   -- Because the following functions will be called a very large number of
   -- times there is limited error checking of the values in order not to
   -- slow down image reading. These functions were modified 18 July 2000 to
   -- remove calls to Bit_Depth and replace these with F.Bit_Depth because
   -- this is more efficient.

   function Palette_Index
     (F    : PNG_File;
      R, C : Coordinate)
      return Unsigned_8
   is
      function Validate (I : Unsigned_8) return Unsigned_8 is
      begin
         if I > F.Palette.Size then
            Raise_Exception (Format_Error'Identity, "Invalid pixel data in palette image.");
         else
            return I;
         end if;
      end Validate;
      pragma Inline (Validate);
   begin
      case F.Bit_Depth is
         when 1 | 2 | 4 =>
            return Validate (U1_2_4 (F, R, C, F.Bit_Depth));
         when 8 =>
            return Validate (U8 (F, R, C));
         when others =>
            raise Program_Error; -- Since the PLTE chunk was checked on
      end case;                                  -- reading this shouldn't happen.
   end Palette_Index;
   pragma Inline (Palette_Index);

   function Pixel_Value
     (F    : PNG_File;
      R, C : Coordinate)
      return Natural
   is
   begin
      if F.Colour_Type = 3 then
         return Natural (Palette_Index (F, R, C));
      else
         case F.Bit_Depth is
            when 1 | 2 | 4 =>
               return Natural (U1_2_4 (F, R, C, F.Bit_Depth));
            when 8 =>
               return Natural (U8 (F, R, C));
            when 16 =>
               return Natural (U16 (F, R, C));
            when others =>
               raise Program_Error; -- This shouldn't happen.
         end case;
      end if;
   end Pixel_Value;

   function Red_Value
     (F    : PNG_File;
      R, C : Coordinate)
      return Natural
   is
   begin
      if F.Colour_Type = 3 then
         return Natural (F.Palette.R (Palette_Index (F, R, C)));
      else
         case F.Bit_Depth is
            when 8 =>
               return Natural (U8 (F, R, C));
            when 16 =>
               return Natural (U16 (F, R, C));
            when others =>
               raise Call_Error;
         end case;
      end if;
   end Red_Value;

   function Green_Value
     (F    : PNG_File;
      R, C : Coordinate)
      return Natural
   is
   begin
      if F.Colour_Type = 3 then
         return Natural (F.Palette.G (Palette_Index (F, R, C)));
      else
         case F.Bit_Depth is
            when 8 =>
               return Natural (U8 (F, R, C, 1));
            when 16 =>
               return Natural (U16 (F, R, C, 2));
            when others =>
               raise Call_Error;
         end case;
      end if;
   end Green_Value;

   function Blue_Value
     (F    : PNG_File;
      R, C : Coordinate)
      return Natural
   is
   begin
      if F.Colour_Type = 3 then
         return Natural (F.Palette.B (Palette_Index (F, R, C)));
      else
         case F.Bit_Depth is
            when 8 =>
               return Natural (U8 (F, R, C, 2));
            when 16 =>
               return Natural (U16 (F, R, C, 4));
            when others =>
               raise Call_Error;
         end case;
      end if;
   end Blue_Value;

   function Alpha_Value
     (F    : PNG_File;
      R, C : Coordinate)
      return Natural
   is
   -- This function may be called for a Type 4 or Type 6 PNG.  Prior to
   -- version 1.3, this function gave incorrect values for Type 4 PNGs
   -- because it did not take account of the PNG type and the offsets
   -- supplied to U8/16 were for Type 6 RGBA images.
   begin
      case F.Colour_Type is
         when 4 =>
            case F.Bit_Depth is
               when 8 =>
                  return Natural (U8 (F, R, C, 1));
               when 16 =>
                  return Natural (U16 (F, R, C, 2));
               when others =>
                  raise Call_Error;
            end case;
         when 6 =>
            case F.Bit_Depth is
               when 8 =>
                  return Natural (U8 (F, R, C, 3));
               when 16 =>
                  return Natural (U16 (F, R, C, 6));
               when others =>
                  raise Call_Error;
            end case;
         when others =>
            raise Call_Error;
      end case;
   end Alpha_Value;

   function Gamma (F : PNG_File) return Boolean is
   begin
      return F.Gamma;
   end Gamma;

   function Gamma_Value (F : PNG_File) return Natural is
   begin
      if F.Gamma then
         return Natural (F.Gamma_Value);
      else
         raise Call_Error;
      end if;
   end Gamma_Value;

   function Chromaticity (F : PNG_File) return Boolean is
   begin
      return F.Chroma;
   end Chromaticity;

   function White_Point (F : PNG_File) return Pair is
   begin
      if F.Chroma then
         return (Positive (F.White_X), Positive (F.White_Y));
      else
         raise Call_Error;
      end if;
   end White_Point;

   function Red_Primary (F : PNG_File) return Pair is
   begin
      if F.Chroma then
         return (Positive (F.Red_X), Positive (F.Red_Y));
      else
         raise Call_Error;
      end if;
   end Red_Primary;

   function Green_Primary (F : PNG_File) return Pair is
   begin
      if F.Chroma then
         return (Positive (F.Green_X), Positive (F.Green_Y));
      else
         raise Call_Error;
      end if;
   end Green_Primary;

   function Blue_Primary (F : PNG_File) return Pair is
   begin
      if F.Chroma then
         return (Positive (F.Blue_X), Positive (F.Blue_Y));
      else
         raise Call_Error;
      end if;
   end Blue_Primary;

   function Standard_RGB (F : PNG_File) return Boolean is
   begin
      return F.SRGB;
   end Standard_RGB;

   function SRGB_Rendering (F : PNG_File) return Rendering_Intent is
   begin
      if F.SRGB then
         return F.Rendering;
      else
         raise Call_Error;
      end if;
   end SRGB_Rendering;

   function Standard_RGB_Chunk (R : Rendering_Intent) return Chunk is
   begin
      return Chunk'(1, To_Chunk_Name (sRGB), Stream_Element_Array'(1 => Rendering_Intent'Pos (R)), Before_PLTE);
   end Standard_RGB_Chunk;

   function Standard_RGB_Chroma return Chunk is
      use Chromaticity_Data;
   begin
      return Chromaticity_Chunk (D65, BT709_R, BT709_G, BT709_B);
   end Standard_RGB_Chroma;

   function Standard_RGB_Gamma return Chunk is
   -- The gamma value used here is laid down in the PNG Specification V1.2, p22.
   begin
      return Gamma_Chunk (Gamma_2_2);
   end Standard_RGB_Gamma;

   function Physical (F : PNG_File) return Boolean is
   begin
      return F.Physical;
   end Physical;

   function Unit_Unknown (F : PNG_File) return Boolean is
   begin
      if F.Physical then
         return F.Phys_Unit = 0;
      else
         raise Call_Error;
      end if;
   end Unit_Unknown;

   function Unit_Meter (F : PNG_File) return Boolean is
   begin
      if F.Physical then
         return F.Phys_Unit = 1;
      else
         raise Call_Error;
      end if;
   end Unit_Meter;

   function Physical_Value (F : PNG_File) return Pair is
   begin
      if F.Physical then
         return (Positive (F.Phys_X), Positive (F.Phys_Y));
      else
         raise Call_Error;
      end if;
   end Physical_Value;

   function NTexT (F : PNG_File) return Natural is
   begin
      return F.Number_of_Texts;
   end NTexT;

   function TextN
     (F : PNG_File;
      N : Positive)
      return Text_Item_Pointer
   is
      P : Text_Item_Pointer := F.Text_Strings; -- May be null.
   begin
      if N <= F.Number_of_Texts then
         -- We work through the list here in reverse order. If N = F.Text_Strings
         -- we return the first list item, if N = 1 we return the last. (The list
         -- was added to at the head so the first item is the last text string
         -- found in the file.
         for I in 1 .. F.Number_of_Texts - N loop
            P := P.Link;
         end loop;
         return P;
      else
         Raise_Exception (Call_Error'Identity, "String index too large.");
      end if;
   end TextN;
   pragma Inline (TextN);

   function Text_Keyword
     (F : PNG_File;
      N : Positive)
      return String
   is
   begin
      return TextN (F, N).Keyword.all;
   end Text_Keyword;

   function Text_String
     (F : PNG_File;
      N : Positive)
      return String
   is
   begin
      return TextN (F, N).Text_String.all;
   end Text_String;

   function Ancillary_Chunk_Count (F : PNG_File) return Natural is
   begin
      return F.Number_of_Chunks;
   end Ancillary_Chunk_Count;

   function Ancillary_Chunk
     (F : PNG_File;
      N : Positive)
      return Chunk
   is
      P : Chunk_List := F.Ancillary_Chunks; -- May be null.
   begin
      if N <= F.Number_of_Chunks then
         -- We work through the list here in reverse order. If N = F.Number_of_Chunks
         -- we return the first list item, if N = 1 we return the last. (The list
         -- was added to at the head so the first item is the last chunk found in
         -- the file.
         for I in 1 .. F.Number_of_Chunks - N loop
            P := P.Link;
         end loop;
         return P.Chnk;
      else
         Raise_Exception (Argument_Error'Identity, "Chunk index too large.");
      end if;
   end Ancillary_Chunk;

   function Name (C : Chunk) return Chunk_Name is
   begin
      return C.Name;
   end Name;
   function Data (C : Chunk) return Stream_Element_Array is
   begin
      return C.Data;
   end Data;

   procedure Destroy (L : in out Chunk_List) is
      T : Chunk_List;
      procedure Deallocate is new Ada.Unchecked_Deallocation (Chunk_List_Element, Chunk_List);
   begin
      while L /= Null_Chunk_List loop
         T := L;
         L := L.Link;
         Deallocate (T);
      end loop;
   end Destroy;

   function To_Chunk_List (C : Chunk) return Chunk_List is
   begin
      return new Chunk_List_Element'(C.Size, C, Null_Chunk_List);
   end To_Chunk_List;

   function "&" (Left, Right : Chunk) return Chunk_List is
   begin
      return To_Chunk_List (Left) & Right;
   end "&";

   function "&"
     (Left  : Chunk;
      Right : Chunk_List)
      return Chunk_List
   is
   begin
      return new Chunk_List_Element'(Left.Size, Left, Right);
   end "&";

   function "&"
     (Left  : Chunk_List;
      Right : Chunk)
      return Chunk_List
   is
   begin
      return Left & To_Chunk_List (Right);
   end "&";

   function "&" (Left, Right : Chunk_List) return Chunk_List is
   begin
      if Left = Null_Chunk_List then

         -- The user supplied a null list on the left. We handle this without
         -- complaint and return the right list (which might be null, but not
         -- a problem if it is).

         return Right;
      end if;

      -- Otherwise, we need to feel our way down to the end of the Left list and
      -- tack Right onto the end. If Right is null, this will have no effect.

      declare
         T : Chunk_List := Left;   -- This cannot be null, because
         pragma Assert (T /= null); -- we checked just above.
      begin
         while T.Link /= Null_Chunk_List loop
            T := T.Link;
         end loop;
         T.Link := Right;
         return Left;
      end;
   end "&";

   function Ancillary_Chunk
     (Name  : Chunk_Name;
      Data  : Stream_Element_Array;
      Where : Chunk_Position)
      return Chunk
   is
      N : constant Unsigned_32 := To_Unsigned_32 (Name);
   begin
      if not Ancillary (N) then
         Raise_Exception (Argument_Error'Identity, "Chunk name does not denote an ancillary chunk.");
      end if;

      -- If the chunk name is known to PNG_IO, verify the given chunk position.

      if Known_Chunk (N) and then Position (N) /= Where then
         Raise_Exception
           (Argument_Error'Identity,
            "Position for chunk " & Name & " should be " & Chunk_Position'Image (Position (N)));
      end if;

      return (Data'Length, Name, Data, Where);
   end Ancillary_Chunk;

   function Gamma_Chunk (Gamma : Natural := Unity_Gamma) return Chunk is
   -- Perhaps it would be wise to do some sort of sanity check on Gamma?
   -- On the other hand, the PNG specification allows a 4-byte value and
   -- doesn't specify any constraints on the value.
   begin
      return Chunk'(4, To_Chunk_Name (gAMA), To_Buffer_4 (Unsigned_32 (Gamma)), Before_PLTE);
   end Gamma_Chunk;

   function Text_Chunk (Keyword, Text : String) return Chunk is

      -- This function should create a compressed text chunk if
      -- the text is more than 1024 characters long, but for now
      -- it doesn't. Actually, maybe it should compute the compressed
      -- text and write the compressed text if it is smaller than the
      -- uncompressed text. There is a simple call in Zlib that can do
      -- this?

      L : constant Stream_Element_Count := Keyword'Length + 1 + Text'Length;

      use Ada.Characters.Latin_1;
   begin

      -- Check the keyword for validity. (ISO standard, Section 11.3.4.2).

      if Keyword'Length = 1                  -- There must be at least one character.
      or else Keyword (Keyword'First) = Space -- Leading spaces are not permitted.
      or else Keyword (Keyword'Last) = Space -- Trailing spaces ditto.
      then
         Raise_Exception (Data_Error'Identity, "Illegal use of spaces in text chunk keyword.");
      end if;
      for I in Keyword'Range loop
         if Keyword (I) = No_Break_Space then
            Raise_Exception (Data_Error'Identity, "Illegal non-break space character in text chunk keyword.");
         elsif not Ada.Characters.Handling.Is_Graphic (Keyword (I)) then
            Raise_Exception (Data_Error'Identity, "Illegal non-graphic character in text chunk keyword.");
         end if;
      end loop;

      -- All seems OK with the keyword, create the chunk.

      return Chunk'(L, To_Chunk_Name (PNG_IO.Base.tEXt), -- The chunk code,
      -- not the parameter!
      To_Stream_Element_Array (Keyword & NUL & Text), Anywhere);
   end Text_Chunk;

   function Chromaticity_Chunk (White_Point, Red_Primary, Green_Primary, Blue_Primary : Pair) return Chunk is
      D : constant Stream_Element_Array :=
        To_Buffer_4 (Unsigned_32 (White_Point.X)) &
        To_Buffer_4 (Unsigned_32 (White_Point.Y)) &
        To_Buffer_4 (Unsigned_32 (Red_Primary.X)) &
        To_Buffer_4 (Unsigned_32 (Red_Primary.Y)) &
        To_Buffer_4 (Unsigned_32 (Green_Primary.X)) &
        To_Buffer_4 (Unsigned_32 (Green_Primary.Y)) &
        To_Buffer_4 (Unsigned_32 (Blue_Primary.X)) &
        To_Buffer_4 (Unsigned_32 (Blue_Primary.Y));
   begin
      return Chunk'(D'Length, To_Chunk_Name (cHRM), D, Before_PLTE);
   end Chromaticity_Chunk;

   function Physical_Chunk
     (Value : Pair;
      Metre : Boolean)
      return Chunk
   is
      D : constant Stream_Element_Array :=
        To_Buffer_4 (Unsigned_32 (Value.X)) & To_Buffer_4 (Unsigned_32 (Value.Y)) & To_Stream_Element (Metre);
   begin
      return Chunk'(D'Length, To_Chunk_Name (pHYs), D, Before_IDAT);
   end Physical_Chunk;

   -- Procedures for writing PNG files. There are variants of the user visible generics
   -- for each type of PNG file and for 8-bit and 16-bit sample sizes.
   -- However, most of the actual code for file output is common.

   -- The procedure Write_Chunk is responsible for computing the CRC of the chunk, as
   -- well as the length.

   Null_Stream_Element_Array : constant Stream_Element_Array (1 .. 0) := (others => 0);

   procedure Write_Chunk
     (F          : in File_Type;
      Chunk_Code : in Unsigned_32;
      Chunk_Data : in Stream_Element_Array := Null_Stream_Element_Array)
   is
   begin
      Write (F, To_Buffer_4 (Chunk_Data'Length));
      declare
         CRC : ZLib.Unsigned_32 := 0;
      begin

         -- Convert the chunk code to an array of bytes, and calculate its CRC.

         declare
            CC : constant Buffer_4 := To_Buffer_4 (Chunk_Code);
         begin
            ZLib.CRC32 (CRC, CC);
            Write (F, CC);
         end;

         -- The chunk data field can be empty (example, the IEND chunk)
         -- in which case a null buffer will be supplied, and we write
         -- nothing to the file. The check on the chunk data may be redundant
         -- because the IEND chunk is now written from a constant to avoid
         -- recomputing the CRC every time a file is written. If there are no
         -- other cases where a chunk has zero length chunk data, we could
         -- eliminate this check. 25 April 2009.

         if Chunk_Data'Length > 0 then
            ZLib.CRC32 (CRC, Chunk_Data);
            Write (F, Chunk_Data);
         end if;

         Write (F, To_Buffer_4 (Unsigned_32 (CRC)));
      end;
   end Write_Chunk;
   pragma Inline (Write_Chunk);

   procedure Start_File
     (Filename   : in     String;
      X, Y       : in     Dimension;
      CT         : in     Colour_Type_Code;
      BD         : in     Depth;
      Interlace  : in     Boolean;
      F          :    out File_Type;
      Compressor : in out ZLib.Filter_Type;
      Level      : in     ZLib.Compression_Level)
   is

   -- This procedure creates the PNG file and writes the PNG signature and
   -- IHDR chunk to the file.  The chunk data is small, and is constructed
   -- as a constant array on the stack. The procedure also initialises the
   -- Zlib compressor used to compress the IDAT data.

   begin
      ZLib.Deflate_Init (Compressor, Level);

      Create (F, Out_File, Filename);
      Write (F, PNG_Signature);
      declare
         Chunk_Data : constant Stream_Element_Array :=
           To_Buffer_4 (Unsigned_32 (X)) &
           To_Buffer_4 (Unsigned_32 (Y)) &
           Bit_Depth_Table (BD) &
           Colour_Type_Table (CT) &
           0 & -- Compression method.
           0 & -- Filter method.
           To_Stream_Element (Interlace);
      begin
         Write_Chunk (F, IHDR, Chunk_Data);
      end;
   end Start_File;
   pragma Inline (Start_File);

   -- Zlib calls the callback procedure below to output compressed data. The callback
   -- is passed to Zlib in each of the procedures Write_PNG_Type_X. Normally the action
   -- of the procedure is to output an IDAT chunk containing whatever data it is given.
   -- However, Zlib passes only two bytes of data on the first call (the Zlib header)
   -- and it would be a shame to output an IDAT chunk with only two bytes of data, so
   -- we cache these two bytes until the next call and output them then. Obviously, the
   -- behaviour of Zlib is subject to change, so we build in some checks on what is
   -- happening, as well as check on the content of the two header bytes, in case any
   -- future changes to Zlib or Zlib_Ada defaults cause a change from the assumed
   -- behaviour. The procedure is generic in order to permit the state information and
   -- the file handle to be declared in the code that sets up the Zlib compressor.

   -- The three states in the following types correspond to an initial state before the
   -- first call, the first call (which should pass the two header bytes) and all later
   -- calls, when the default action is taken.

   type Zlib_Header_Output_Status is (Not_Yet_Seen, Cached, Output);

   subtype Zlib_Header is Stream_Element_Array (1 .. 2);

   generic
      F      : in out File_Type;
      Cache  : in out Zlib_Header;
      Status : in out Zlib_Header_Output_Status;
   procedure Write_Compressed_IDAT_Data (Data : in Stream_Element_Array);

   procedure Write_Compressed_IDAT_Data (Data : in Stream_Element_Array) is
   begin
      -- It would not be harmful to output a zero length IDAT chunk, but there is
      -- no point in doing so. Versions of Zlib_Ada prior to 1.3 passed zero length
      -- data, and the assertion that follows is just a check that this does not
      -- occur, as it simplifies the rest of the procedure.
      pragma Assert (Data'Length /= 0);

      case Status is
         when Not_Yet_Seen => -- This must be the first call. It is possible that only
            -- one byte has been passed in which case we have a big
            -- problem, so we check for this with an assertion.

            pragma Assert (Data'Length > 1);

            -- Otherwise, we can handle two or more bytes. If there are
            -- only two, we cache the two bytes until the next call,
            -- if there are more than two, we output an IDAT chunk.
            -- Obviously, if Zlib passes three bytes, this results in
            -- stupid behaviour, but it is likely that 'more than two'
            -- will, in practice, be a large block of data.

            Cache := Data (Data'First .. Stream_Element_Offset'Succ (Data'First));

            -- Check that the two bytes are valid as a Zlib header in a
            -- PNG file. If not, something in the defaults or parameters
            -- of Zlib or Zlib_Ada must have changed and PNG_IO needs to
            -- be modified to suit.

            pragma Assert (Valid_Zlib_Header (Cache (1), Cache (2)));

            if Data'Length = 2 then -- This is the expected behaviour.
               Status := Cached; -- Output the two bytes on the next call.
            else
               -- This is plausible behaviour, but not what Zlib does at
               -- present.
               Write_Chunk (F, IDAT, Data); -- Output a chunk now, and note
               Status := Output;           -- that the header has been output.
            end if;

         when Cached => -- Prepend the two cached bytes to the data passed on
            -- this, the second call, and output an IDAT chunk.
            Write_Chunk (F, IDAT, Cache & Data);
            Status := Output;

         when Output =>
            Write_Chunk (F, IDAT, Data); -- Normal action: output a chunk.
      end case;
   end Write_Compressed_IDAT_Data;

   procedure Write_Ancillary_Chunks
     (F : in File_Type;
      L : in Chunk_List;
      W :    Chunk_Position)
   is
      -- Writes any chunks in L whose positioning matches W.
      P : Chunk_List := L;
   begin
      while P /= null loop
         if P.Chnk.Where = W then
            Write_Chunk (F, To_Unsigned_32 (P.Chnk.Name), P.Chnk.Data);
         end if;
         P := P.Link;
      end loop;
   end Write_Ancillary_Chunks;
   pragma Inline (Write_Ancillary_Chunks);

   procedure Finish_File (F : in out Ada.Streams.Stream_IO.File_Type) is
   -- This procedure writes a tEXt chunk and the IEND chunk to the file
   -- and closes the file. The tEXt chunk identifies this software as the
   -- creator of the PNG file. The IEND chunk always contains the same
   -- byte sequence, including the CRC, so it is defined as a constant (in
   -- PNG_IO.Base). We could precompute the tEXt chunk, but since it
   -- changes with each release of PNG_IO, this would be slightly more
   -- difficult to do.
   begin
      Write_Chunk (F, tEXt, To_Stream_Element_Array (Package_Identifier));
      Write (F, IEND_Chunk);
      Close (F);
   end Finish_File;

   generic
      Bpp : in Stream_Element_Offset := 1; -- The offset between corresponding bytes
      -- of adjacent pixels within a scanline, as
      -- described in Section 6 of the PNG Specification.
   function Adaptive_Filter (Raw, Prior : Stream_Element_Array) return Stream_Element_Array;

   function Adaptive_Filter (Raw, Prior : Stream_Element_Array) return Stream_Element_Array is separate;

   procedure Write_PNG_Type_0
     (Filename  : in String;
      I         : in Image_Handle;
      X, Y      : in Dimension;
      Bit_Depth : in Depth             := Eight;
      Interlace : in Boolean           := False;
      Ancillary : in Chunk_List        := Null_Chunk_List;
      Level     : in Compression_Level := Default_Compression) is separate;

   procedure Write_PNG_Type_2
     (Filename  : in String;
      I         : in Image_Handle;
      X, Y      : in Dimension;
      Bit_Depth : in Depth_8_16        := Eight;
      Interlace : in Boolean           := False;
      Ancillary : in Chunk_List        := Null_Chunk_List;
      Level     : in Compression_Level := Default_Compression) is separate;

   procedure Write_PNG_Type_3
     (Filename  : in String;
      P         : in Palette_Handle;
      I         : in Image_Handle;
      X, Y      : in Dimension;
      Interlace : in Boolean           := False;
      Ancillary : in Chunk_List        := Null_Chunk_List;
      Level     : in Compression_Level := Default_Compression) is separate;

   procedure Write_PNG_Type_4
     (Filename  : in String;
      I         : in Image_Handle;
      X, Y      : in Dimension;
      Bit_Depth : in Depth_8_16        := Eight;
      Interlace : in Boolean           := False;
      Ancillary : in Chunk_List        := Null_Chunk_List;
      Level     : in Compression_Level := Default_Compression) is separate;

   procedure Write_PNG_Type_6
     (Filename  : in String;
      I         : in Image_Handle;
      X, Y      : in Dimension;
      Bit_Depth : in Depth_8_16        := Eight;
      Interlace : in Boolean           := False;
      Ancillary : in Chunk_List        := Null_Chunk_List;
      Level     : in Compression_Level := Default_Compression) is separate;

end PNG_IO;
