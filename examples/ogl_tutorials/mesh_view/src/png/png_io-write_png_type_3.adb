-- $Id: png_io-write_png_type_3.adb,v 1.7 2009/05/04 19:31:14 sangwine Exp $
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

separate (PNG_IO)
procedure Write_PNG_Type_3
  (Filename  : in String;
   P         : in Palette_Handle;
   I         : in Image_Handle;
   X, Y      : in Dimension;
   Interlace : in Boolean           := False;
   Ancillary : in Chunk_List        := Null_Chunk_List;
   Level     : in Compression_Level := Default_Compression)
is
   F  : File_Type;
   L  : constant Positive range 1 .. 256                      := Palette_Length (P);
   BD : constant array (1 .. 256) of Depth range One .. Eight :=
     (1 .. 2 => One, 3 .. 4 => Two, 5 .. 16 => Four, 17 .. 256 => Eight);
   Bit_Depth : constant Depth range One .. Eight := BD (L);

   Compressor : ZLib.Filter_Type;

begin

   Start_File (Filename, X, Y, Three, Bit_Depth, Interlace, F, Compressor, Level);

   -- Write any ancillary chunks supplied that have to be positioned before the
   -- PLTE chunk.

   Write_Ancillary_Chunks (F, Ancillary, Before_PLTE);

   -- Write the PLTE chunk.

   declare
      Buffer : Stream_Element_Array (1 .. 3 * Stream_Element_Count (L));
   begin
      for Z in 0 .. Stream_Element_Offset (L - 1) loop
         Buffer (1 + 3 * Z .. 1 + 3 * Z + 2) :=
           Stream_Element (Palette_R_Value (P, Palette_Index (Z))) &
           Stream_Element (Palette_G_Value (P, Palette_Index (Z))) &
           Stream_Element (Palette_B_Value (P, Palette_Index (Z)));
      end loop;
      Write_Chunk (F, PLTE, Buffer);
   end;

   -- Write any ancillary chunks supplied that have to be positioned before the
   -- IDAT chunk (and after the PLTE chunk).

   Write_Ancillary_Chunks (F, Ancillary, Before_IDAT);

   -- Write the IDAT chunk.

   declare

      Header        : Zlib_Header; -- Used to cache the header if necessary.
      Header_Status : Zlib_Header_Output_Status := Not_Yet_Seen;

      procedure Write_Compressed_Data is new Write_Compressed_IDAT_Data (F, Header, Header_Status);

      procedure Compress is new ZLib.Write (Write_Compressed_Data, Buffer_Size => IDAT_Size);

      -- The following procedure writes the image or a sub-image to the uncompressed
      -- data buffer. The Pass parameter default value is used when writing a whole
      -- (non-interlaced) image. In this case, it is not used in the procedure. The
      -- procedure cannot be called for a zero size sub-image, since its X and Y
      -- parameters are of type Dimension, for which the lower bound is 1.

      procedure Write_Image (X, Y : in Dimension; Pass : Pass_Number := 1) is
         Bps      : constant Stream_Element_Count := Bytes_per_Scanline (Three, Bit_Depth, X);
         Scanline : Stream_Element_Array (1 .. Bps);

         function Checked_Value (V : Sample) return Unsigned_8 is
         begin
            if Positive (Sample'Pos (V) + 1) > L then
               Raise_Exception (Call_Error'Identity, "Pixel value greater than palette length.");
            else
               return Unsigned_8 (Sample'Pos (V));
            end if;
         end Checked_Value;
         pragma Inline (Checked_Value);

         function Map (C : Coordinate) return Coordinate is
         begin
            if Interlace then
               return Image_Col (C, Pass);
            else
               return C;
            end if;
         end Map;
         pragma Inline (Map);

      begin
         pragma Assert (Interlace or Pass = 1);
         for Row in 0 .. Coordinate (Y - 1) loop
            declare
               R : Coordinate := Row;
            begin
               if Interlace then            -- If the image is interlaced we must map from
                  R := Image_Row (Row, Pass); -- sub-image to whole image coordinates.
               end if;
               case Bit_Depth is
                  when One | Two | Four =>
                     declare
                        Offset : constant array (Depth_1_2_4) of Coordinate := (7, 3, 1);
                        Shift  : constant array (Depth_1_2_4) of Natural    := (1, 2, 4);
                        Mask   : constant array (Depth_1_2_4) of Unsigned_8 := (2#1#, 2#11#, 2#1111#);
                        Ppb    : constant array (Depth_1_2_4) of Coordinate := (8, 4, 2);
                     begin
                        for P in Scanline'Range loop
                           declare
                              B : Unsigned_8 := 0;
                           begin
                              for O in 0 .. Offset (Bit_Depth) loop
                                 declare

                                    -- We have to be careful about the last few bits in the last byte
                                    -- of the scanline where the number of pixels does not fill all
                                    -- the bits in the last byte. We handle this with the Min attribute
                                    -- and thus replicate the last pixel to fill the spare bits. The
                                    -- ISO standard, Section 7.2 says the content of the spare bits
                                    -- is unspecified, so we are OK to replicate.

                                    Col : constant Coordinate :=
                                      Coordinate'Min (Coordinate (X - 1), Coordinate (P - 1) * Ppb (Bit_Depth) + O);
                                 begin
                                    B :=
                                      Shift_Left (B, Shift (Bit_Depth)) or
                                      (Checked_Value (Pixel_Value (I, R, Map (Col))) and Mask (Bit_Depth));
                                 end;
                              end loop;
                              Scanline (P) := Stream_Element (B);
                           end;
                        end loop;
                     end;
                  when Eight =>
                     for P in Scanline'Range loop
                        Scanline (P) := Stream_Element (Checked_Value (Pixel_Value (I, R, Map (Coordinate (P - 1)))));
                     end loop;
               end case;

               -- Following the recommendations of Section 12.8 in the ISO standard,
               -- the scanlines are not filtered for type 3 images. We simply have to
               -- prepend a byte (None) to indicate no filtering.

               Compress (Compressor, None & Scanline, ZLib.No_Flush);
            end;
         end loop;
      end Write_Image;

   begin

      if Interlace then
         for P in Pass_Number loop
            declare
               W : constant Natural := Sub_Image_Width (X, P);
               H : constant Natural := Sub_Image_Height (Y, P);
            begin
               if W > 0 and H > 0 then
                  Write_Image (W, H, P);
               end if;
            end;
         end loop;
      else
         Write_Image (X, Y);
      end if;

      -- Flush all the IDAT data from the Zlib compressor to IDAT chunks in the file.

      while not ZLib.Stream_End (Compressor) loop
         Compress (Compressor, Null_Stream_Element_Array, ZLib.Finish); -- This may not be the best
         -- way to flush the data ..
      end loop;
      ZLib.Close (Compressor);

   end;

   -- Write any ancillary chunks supplied that may be positioned anywhere.

   Write_Ancillary_Chunks (F, Ancillary, Anywhere);

   Finish_File (F);
end Write_PNG_Type_3;
