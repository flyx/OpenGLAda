-- $Id: png_io-write_png_type_4.adb,v 1.6 2009/05/04 19:31:14 sangwine Exp $
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
procedure Write_PNG_Type_4
  (Filename  : in String;
   I         : in Image_Handle;
   X, Y      : in Dimension;
   Bit_Depth : in Depth_8_16        := Eight;
   Interlace : in Boolean           := False;
   Ancillary : in Chunk_List        := Null_Chunk_List;
   Level     : in Compression_Level := Default_Compression)
is
   F : File_Type;

   Table : constant array (Depth_8_16) of Stream_Element_Offset := (2, 4);
   Bpp : Stream_Element_Offset renames Table (Bit_Depth);
   function Filter is new Adaptive_Filter (Bpp);

   Compressor : ZLib.Filter_Type;

begin

   Start_File (Filename, X, Y, Four, Bit_Depth, Interlace, F, Compressor, Level);

   -- Write any ancillary chunks supplied that have to be positioned before the
   -- IDAT chunk(s).

   Write_Ancillary_Chunks (F, Ancillary, Before_PLTE); -- Probably a do nothing.
   Write_Ancillary_Chunks (F, Ancillary, Before_IDAT);

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
         Bps               : constant Stream_Element_Count   := Bytes_per_Scanline (Four, Bit_Depth, X);
         Scanline          : Stream_Element_Array (1 .. Bps);
         Previous_Scanline : Stream_Element_Array (1 .. Bps) := (others => 0);
      begin
         pragma Assert (Interlace or Pass = 1);
         for Row in 0 .. Coordinate (Y - 1) loop
            for Col in 0 .. Coordinate (X - 1) loop
               declare
                  P : constant Stream_Element_Offset := 1 + Stream_Element_Offset (Col) * Bpp;
                  R : Coordinate                     := Row;
                  C : Coordinate                     := Col;
               begin
                  if Interlace then
                     R := Image_Row (Row, Pass); -- If the image is interlaced we must map from
                     C := Image_Col (Col, Pass); -- sub-image to whole image coordinates.
                  end if;
                  case Bit_Depth is
                     when Eight =>
                        Scanline (P .. P + 1) :=
                          Stream_Element (Sample'Pos (Grey_Sample (I, R, C))) &
                          Stream_Element (Sample'Pos (Alpha_Sample (I, R, C)));
                     when Sixteen =>
                        Scanline (P .. P + 3) :=
                          To_Buffer_2 (Unsigned_16 (Sample'Pos (Grey_Sample (I, R, C)))) &
                          To_Buffer_2 (Unsigned_16 (Sample'Pos (Alpha_Sample (I, R, C))));
                  end case;
               end;
            end loop;
            Compress (Compressor, Filter (Scanline, Previous_Scanline), ZLib.No_Flush);
            Previous_Scanline := Scanline;
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
end Write_PNG_Type_4;
