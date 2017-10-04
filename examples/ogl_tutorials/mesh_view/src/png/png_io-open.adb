-- $Id: png_io-open.adb,v 1.10 2011/11/21 10:16:43 sangwine Exp $
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
procedure Open (F : in out PNG_File; Filename : in String) is
-- This procedure has the following major stages:
-- 1. Read the file signature and IHDR chunk.
-- 2. Read the IDAT, PLTE and other chunks up to the IEND chunk,
--    placing the raw image data into a buffer and any information
--    extracted from other chunks into the descriptor. In the case
--    of IDAT and zTXt chunks, the data is decompressed on the fly
--    as it is read in. Known chunks are interpreted and their data
--    loaded into the descriptor. Unknown chunks are linked into the
--    ancillary chunks list.
-- 3. Defilter the uncompressed image data, leaving the result in
--    a buffer for later use by the pixel access functions. Note
--    that, for an interlaced image, no de-interlacing is done:
--    the pixel access functions have to compute the location of
--    the pixel data taking interlacing into account.
begin

   if F /= null then
      Raise_Exception (Call_Error'Identity, "PNG File " & Filename & " is already open.");
   end if;

   -- Open the file for reading.

   F := new PNG_File_Descriptor;
   Open (F.Handle, In_File, Filename);
   F.Stream := Stream (F.Handle);

   -- Verify that the file really is a PNG file. If the signature in the
   -- first 8 bytes is OK, assume that it is a PNG file. The code below
   -- handles the case where the file has less than 8 bytes, because Read
   -- will return even if there are less than 8 bytes read. Because we
   -- initialise Signature to be zeros, any bytes that are not read from
   -- the file will remain as zeros and will fail to match with the true
   -- signature.

   declare
      Signature : Stream_Element_Array (1 .. 8) := (others => 0);
      Last      : Stream_Element_Offset;
   begin
      Read (F.Handle, Signature, Last);
      if Signature /= PNG_Signature then
         Close (F);
         raise Signature_Error;
      end if;
   end;

   -- Read in the rest of the file, which is structured into chunks, starting
   -- with a 4-byte chunk length field (most significant byte first), then the
   -- chunk name (4 bytes), the chunk data (variable length, from zero bytes
   -- upward), and finally the 4-byte CRC.

   Read_Chunks : declare

      -- The need to read 4 bytes and interpret them as an unsigned 32-bit
      -- integer arises often. Therefore, we define functions to do it, in
      -- two steps: first, to read the 4 bytes from the file into a buffer;
      -- second, to convert a 4-byte buffer into an Unsigned_32. Doing it
      -- in two steps allows us to read 32-bit values direct from the file,
      -- or from a buffer that has already been read in.

      function To_Unsigned_32 (B : Buffer_4) return Unsigned_32 is
         L : Unsigned_32 := Unsigned_32 (B (1));
      begin
         for I in Stream_Element_Offset'(2) .. 4 loop
            L := Shift_Left (L, 8) or Unsigned_32 (B (I));
         end loop;
         return L;
      end To_Unsigned_32;

      function Read_Unsigned_32 return Unsigned_32 is
         B : Buffer_4;
         L : Stream_Element_Count;
      begin
         Read (F.Handle, B, L);
         if L /= B'Last then
            raise End_Error;
         end if;
         return To_Unsigned_32 (B);
      end Read_Unsigned_32;

      function Read_Chunk return Buffer_Pointer is

         -- A function to read the next chunk from the file into a
         -- dynamically allocated buffer, and verify its CRC.

         -- The chunk length field in the chunk represents the size of the
         -- data in the chunk. We are about to read the chunk type (4 bytes)
         -- as well as the data, so we must add 4 to the length read from the file.

         Chunk_Length : constant Unsigned_32 := Read_Unsigned_32;

         BP : Buffer_Pointer := new Buffer (1 .. Stream_Element_Count (Chunk_Length + 4));
         B : Buffer renames BP.all;
      begin

         -- Read the chunk into the buffer. Note that if the chunk length was zero,
         -- we will read in 4 bytes (the chunk type).

         declare
            L : Stream_Element_Count;
         begin
            Read (F.Handle, B, L);
            if L /= B'Last then
               raise End_Error;
            end if;
         end;

         -- Check the CRC. This covers the chunk type and the chunk data, i.e.
         -- the whole content of the buffer B. The CRC at the end of the chunk is
         -- the 1's complement of the CRC computed over the chunk type and data.

         declare
            NChunk_CRC : constant ZLib.Unsigned_32 := ZLib.Unsigned_32 (Read_Unsigned_32);
            Buffer_CRC : ZLib.Unsigned_32          := 0;
            use type ZLib.Unsigned_32;
         begin
            ZLib.CRC32 (Buffer_CRC, B);
            if Buffer_CRC /= NChunk_CRC then
               raise CRC_Error;
            end if;
         end;

         return BP;
      exception
         when others =>
            Deallocate (BP);
            raise;
      end Read_Chunk;

   begin

      -- Read the IHDR chunk (which must come next, immediately after the signature).

      declare
         BP : Buffer_Pointer := Read_Chunk;
         B : Buffer renames BP.all;
      begin

         if B'Length /= 17 or else To_Unsigned_32 (B (1 .. 4)) /= IHDR then
            Deallocate (BP);
            raise Format_Error;
         end if;

         -- The chunk seems OK so far, so copy the data into
         -- the file descriptor, and deallocate the chunk.

         begin
            F.Width  := Dimension (To_Unsigned_32 (B (5 .. 8)));
            F.Height := Dimension (To_Unsigned_32 (B (9 .. 12)));
         exception
            when Constraint_Error =>
               Raise_Exception (Format_Error'Identity, "Invalid image dimension in IHDR chunk.");
         end;

         F.Bit_Depth   := Unsigned_8 (B (13));
         F.Colour_Type := Unsigned_8 (B (14));
         F.Compression := Unsigned_8 (B (15));
         F.Filter      := Unsigned_8 (B (16));
         F.Interlace   := Unsigned_8 (B (17));

         Deallocate (BP);

      end;

      -- Now check that the values just read in are valid. This is a check
      -- on the validity of the encoder that wrote the PNG file rather than
      -- the integrity of the file, since we have just checked the CRC and
      -- found it to be correct.

      -- Check the colour type and the bit depth together since these are inter-related.

      declare

         procedure Verify (V : in Boolean) is
         begin
            if not V then
               Raise_Exception (Format_Error'Identity, "Invalid combination of colour type and bit depth in IHDR chunk.");
            end if;
         end Verify;
         pragma Inline (Verify);

         T : Unsigned_8 renames F.Colour_Type;
         D : Unsigned_8 renames F.Bit_Depth;

      begin
         if T = 0 then
            Verify (D = 1 or D = 2 or D = 4 or D = 8 or D = 16);
         elsif T = 2 then
            Verify (D = 8 or D = 16);
         elsif T = 3 then
            Verify (D = 1 or D = 2 or D = 4 or D = 8);
         elsif T = 4 then
            Verify (D = 8 or D = 16);
         elsif T = 6 then
            Verify (D = 8 or D = 16);
         else
            Raise_Exception (Format_Error'Identity, "Invalid colour type in IHDR chunk.");
         end if;
      end;

      if F.Compression /= 0 or F.Filter /= 0 or F.Interlace > 1 then
         Raise_Exception (Format_Error'Identity, "Illegal compression, filter or interlace value in IHDR chunk.");
      end if;

      -- We are now ready to read the chunks. If the image is of colour
      -- type 3 we are looking for the PLTE chunk before the first IDAT
      -- chunk, otherwise we are looking for an IDAT chunk first.
      -- There may be other chunks present after the IDAT chunks, which must
      -- be ancillary chunks (ignored here, although their CRCs are checked).

      -- From version 4.0 of PNG_IO, the compressed data in IDAT chunks and
      -- zTXt chunks is decompressed on-the-fly. That is, it is read from the
      -- buffer containing the chunk data, and directly decompressed from there
      -- to the uncompressed data buffer in the descriptor.

      declare
         PLTE_Flag, IDAT_Flag : Boolean     := False;
         Previous_Chunk_Type  : Unsigned_32 := 0;

         -- We verify the first two bytes of the IDAT stream (and of any zTXt streams too).
         -- Since IDAT chunks of length 1 (and even 0!) are legal, we can't guarantee that
         -- the first two bytes will be found in the first IDAT chunk (or even in consecutive
         -- IDAT chunks, since zero length IDAT chunks could occur). Therefore we may have to
         -- save the value of the first byte and check the two bytes only when we have read
         -- the second.

         IDAT_1 : Stream_Element := 0; -- To store the first byte of the IDAT stream.
         IDAT_V : Boolean        := False;    -- Set True when we have seen the second byte and tested.

         procedure Validate_Zlib_Stream (CMG, FLG : in Stream_Element; Message : in String) is
         begin
            if not Valid_Zlib_Header (CMG, FLG) then
               Raise_Exception (Format_Error'Identity, Message);
            end if;
         end Validate_Zlib_Stream;

         Z : ZLib.Filter_Type; -- This is used for the IDAT decompression.

      begin

         -- Allocate a buffer for the uncompressed image data in the IDAT chunks.
         -- The size of this buffer is exactly that needed for the uncompressed pixel
         -- data.

         F.Uncompressed_Data :=
           new Buffer (1 .. Image_Size (Colour_Type (F), Bit_Depth (F), Width (F), Height (F), Interlaced (F)));

         For_each_chunk :
         while Previous_Chunk_Type /= IEND loop -- Read all the chunks,
            declare                                               -- including IEND.
               BP : Buffer_Pointer := Read_Chunk;
               B : Buffer renames BP.all;

               Chunk_Type   : constant Unsigned_32          := To_Unsigned_32 (B (1 .. 4));
               Chunk_Length : constant Stream_Element_Count := B'Length - 4;

               procedure Confirm_Chunk_Length (L : in Stream_Element_Count) is
               begin
                  if Chunk_Length /= L then
                     Raise_Exception
                       (Format_Error'Identity,
                        "Incorrect chunk length in " & To_Chunk_Name (Chunk_Type) & " chunk.");
                  end if;
               end Confirm_Chunk_Length;

            begin
               case Chunk_Type is
                  when PLTE =>

                     -- We have to check here that:
                     -- 1. The length of the chunk is divisible by 3,
                     --    and that there are between 1 and 256 entries.
                     -- 2. The image is a colour image.
                     -- 3. There have been no previous PLTE chunks
                     --    (because only one is allowed).
                     -- 4. There have been no IDAT chunks (because PLTE
                     --    must precede IDAT if it occurs).

                     if Chunk_Length rem 3 /= 0 or Chunk_Length > 768 or Chunk_Length < 3 then
                        Raise_Exception (Format_Error'Identity, "Illegal length in PLTE chunk.");
                     end if;
                     if (F.Colour_Type and 16#02#) = 0 then
                        Raise_Exception (Format_Error'Identity, "Illegal PLTE chunk in greyscale PNG.");
                     end if;
                     if PLTE_Flag then
                        Raise_Exception (Format_Error'Identity, "Illegal multiple PLTE chunks.");
                     end if;
                     if IDAT_Flag then
                        Raise_Exception (Format_Error'Identity, "Illegal PLTE chunk after IDAT chunk(s).");
                     end if;
                     PLTE_Flag := True;

                     -- Allocate a palette and copy the colour palette data to it
                     -- from the chunk buffer.

                     F.Palette := new Colour_Palette (Unsigned_8 (Chunk_Length / 3 - 1));
                     for X in F.Palette.R'Range loop
                        declare
                           Y : constant Stream_Element_Offset := 3 * Stream_Element_Offset (X);
                        begin
                           F.Palette.R (X) := Unsigned_8 (B (Y + 5));
                           F.Palette.G (X) := Unsigned_8 (B (Y + 6));
                           F.Palette.B (X) := Unsigned_8 (B (Y + 7));
                        end;
                     end loop;

                  when IDAT =>

                     -- We have to check here that:
                     -- 1. If this is the first IDAT chunk, a PLTE has been
                     --    seen IF REQUIRED.
                     -- 2. If this is not the first IDAT chunk, the previous
                     --    chunk was also an IDAT (because other chunks are
                     --    not allowed in between successive IDAT chunks).

                     if (not PLTE_Flag) and Palette (F) then
                        Raise_Exception (Format_Error'Identity, "Missing PLTE chunk before IDAT chunk(s).");
                     end if;
                     if IDAT_Flag and Previous_Chunk_Type /= IDAT then
                        Raise_Exception (Format_Error'Identity, "IDAT chunks not consecutive.");
                     end if;

                     -- All seems OK. Proceed with decompressing the data.

                     Decompress : declare
                        U : Buffer renames F.Uncompressed_Data.all;
                        I, O : Stream_Element_Offset;

                        use ZLib;
                     begin
                        if Chunk_Length /= 0 then -- If this is a zero length IDAT, skip.
                           if not IDAT_Flag then

                              -- This is the first IDAT chunk (or the first of non-zero length!).

                              Inflate_Init (Z);   -- Initialise the Zlib decompressor.

                              IDAT_Flag := True; -- Note that we have seen the first IDAT, and
                              -- therefore that the Zlib decompressor has been
                              -- intialised (and must be closed if we exit due
                              -- to an exception).

                              if Chunk_Length > 1 then

                                 -- We can check the first two bytes now. There is no need to store the
                                 -- first one.

                                 Validate_Zlib_Stream (B (5), B (6), "Invalid Zlib header in first IDAT chunk.");
                                 IDAT_V := True;
                              else

                                 -- We have to store the first byte, and check the second one later when
                                 -- we get another chunk with non-zero length.

                                 IDAT_1 := B (5);
                              end if;

                              -- This is the first IDAT chunk so we can use the whole of the
                              -- uncompressed data buffer (we may need to, if this is the only
                              -- IDAT chunk). If there are other IDAT chunks, then this call
                              -- to Zlib will not fill the uncompressed data buffer. If this
                              -- chunk has length one, we will pass only the first header byte
                              -- into Zlib (allowed).

                              Translate (Z, B (5 .. B'Last), I, U, O, No_Flush);
                           else

                              -- This is not the first (non-zero length) IDAT chunk, so we need to
                              -- get Zlib to store the decompressed data after any data that is already
                              -- in the uncompressed data buffer. We also have to check the first
                              -- two bytes if we have not done so already.

                              if not IDAT_V then
                                 Validate_Zlib_Stream (IDAT_1, B (5), "Invalid Zlib header in IDAT chunks.");
                                 IDAT_V := True;
                              end if;

                              Translate (Z, B (5 .. B'Last), I, U (Total_Out (Z) + 1 .. U'Last), O, No_Flush);
                           end if;

                           pragma Assert (I = B'Last); -- There doesn't seem to be any similar
                           -- check we can do on the value of O.
                        end if;
                     end Decompress;

                  when IEND =>
                     Confirm_Chunk_Length (0);
                     -- We do not check here whether there is any more
                     -- data in the file after the IEND chunk, even
                     -- though the presence of such data would make the
                     -- PNG file which we are reading non-compliant.
                     -- The compliance conditions on PNG decoders are
                     -- given in section 15.2.3 of the W3C standard,
                     -- and these compliance conditions do not require
                     -- a decoder to raise an error.
                     Close (F.Handle);
                     if not IDAT_Flag then
                        Raise_Exception (Format_Error'Identity, "No IDAT chunks in file.");
                     end if;

                  when cHRM =>
                     Confirm_Chunk_Length (32);
                     if IDAT_Flag or PLTE_Flag then
                        Raise_Exception (Format_Error'Identity, "cHRM chunk after IDAT/PLTE.");
                     elsif F.Chroma then
                        Raise_Exception (Format_Error'Identity, "Multiple cHRM chunks.");
                     end if;
                     F.Chroma  := True;
                     F.White_X := To_Unsigned_32 (B (5 .. 8));
                     F.White_Y := To_Unsigned_32 (B (9 .. 12));
                     F.Red_X   := To_Unsigned_32 (B (13 .. 16));
                     F.Red_Y   := To_Unsigned_32 (B (17 .. 20));
                     F.Green_X := To_Unsigned_32 (B (21 .. 24));
                     F.Green_Y := To_Unsigned_32 (B (25 .. 28));
                     F.Blue_X  := To_Unsigned_32 (B (29 .. 32));
                     F.Blue_Y  := To_Unsigned_32 (B (33 .. 36));
                  when gAMA =>
                     Confirm_Chunk_Length (4);
                     if IDAT_Flag or PLTE_Flag then
                        Raise_Exception (Format_Error'Identity, "gAMA chunk after IDAT/PLTE.");
                     elsif F.Gamma then
                        Raise_Exception (Format_Error'Identity, "Multiple gAMA chunks.");
                     end if;
                     F.Gamma       := True;
                     F.Gamma_Value := To_Unsigned_32 (B (5 .. 8));
                  when sRGB =>
                     Confirm_Chunk_Length (1);
                     if IDAT_Flag or PLTE_Flag then
                        Raise_Exception (Format_Error'Identity, "sRGB chunk after IDAT/PLTE.");
                     elsif F.SRGB then
                        Raise_Exception (Format_Error'Identity, "Multiple sRGB chunks.");
                     end if;
                     F.SRGB := True;
                     declare
                        B5 : Stream_Element renames B (5);
                     begin
                        if B5 > Rendering_Intent'Pos (Rendering_Intent'Last) then
                           Raise_Exception (Format_Error'Identity, "sRGB chunk has bad value.");
                        end if;
                        F.Rendering := Rendering_Intent'Val (B5);
                     end;
                  when pHYs =>
                     Confirm_Chunk_Length (9);
                     if IDAT_Flag then
                        Raise_Exception (Format_Error'Identity, "pHYs chunk after IDAT.");
                     elsif F.Physical then
                        Raise_Exception (Format_Error'Identity, "Multiple pHYs chunks.");
                     end if;
                     F.Physical  := True;
                     F.Phys_X    := To_Unsigned_32 (B (5 .. 8));
                     F.Phys_Y    := To_Unsigned_32 (B (9 .. 12));
                     F.Phys_Unit := Unsigned_8 (B (13));
                     if (F.Phys_Unit and 16#FE#) /= 0 then
                        Raise_Exception (Format_Error'Identity, "Bad unit specifier in pHYs chunk.");
                     end if;

                  when tEXt | zTXt =>

                     -- Both types of chunk can be dealt with here, because
                     -- the only difference in content is that the text (not
                     -- the keyword) in a zTXt chunk is compressed and there
                     -- is an extra null byte indicating deflate/inflate
                     -- compression method following the null separator after
                     -- the keyword text. Therefore the only extra step in
                     -- reading a zTXt chunk is to check the extra null and
                     -- decompress the text.

                     F.Number_of_Texts := F.Number_of_Texts + 1;

                     -- Create a new item at the head of the list. This means
                     -- the list is in reverse order to what is found in the
                     -- file. This is corrected when the strings are supplied
                     -- to the user.

                     F.Text_Strings := new Text_Item'(null, null, F.Text_Strings);

                     -- Read the keyword string, verifying that the characters in
                     -- the string are allowed characters. (ISO standard, Section
                     -- 11.3.4.2).

                     declare
                        K : String (1 .. 80); -- The keyword string is always less than
                        -- 80 characters in length, but we need to
                        -- allow space for a NUL at the end.
                        use Ada.Characters.Latin_1;
                     begin
                        for I in K'Range loop -- Read the chunk data up to the null.
                           K (I) := Character'Val (Natural (B (Stream_Element_Offset (I) + 4)));
                           if K (I) = NUL then

                              -- We have reached the end of the keyword string. Check
                              -- that it has no leading or trailing spaces, or consecutive
                              -- spaces.

                              if I = 1       -- The keyword must be at least one character.
                              or else K (1) = Space -- Leading spaces are not permitted.
                              or else K (I - 1) = Space -- Trailing spaces ditto.
                              then
                                 Raise_Exception (Format_Error'Identity, "Illegal use of spaces in text chunk keyword.");
                              end if;
                              F.Text_Strings.Keyword := new String'(K (1 .. I - 1));
                              exit;
                           end if;

                           -- The character just read was not a NUL, check its legality.

                           if K (I) = No_Break_Space then
                              Raise_Exception
                                (Format_Error'Identity,
                                 "Illegal non-break space character in text chunk keyword.");
                           elsif not Ada.Characters.Handling.Is_Graphic (K (I)) then
                              Raise_Exception (Format_Error'Identity, "Illegal non-graphic character in text chunk keyword.");
                           end if;
                        end loop;

                        -- The rest of the chunk is either compressed text or plain text.
                        -- If it is plain text, all we need to do is copy it to the
                        -- descriptor. If it is compressed we need to decompress it and
                        -- then copy it to the descriptor.

                        declare
                           T : Buffer renames B (F.Text_Strings.Keyword'Length + 6 .. B'Last);
                        begin
                           if Chunk_Type = zTXt then

                              -- Verify the compression method byte.

                              if T (T'First) /= 0 then
                                 Raise_Exception (Format_Error'Identity, "Unknown compression method in zTXt chunk.");
                              end if;

                              -- Verify the first two bytes of the Zlib stream.

                              Validate_Zlib_Stream (T (T'First + 1), T (T'First + 2), "Invalid Zlib format in zTXt chunk.");

                              -- Decompress the text. We don't know the size of the
                              -- decompressed text, and assuming that zTXt chunks will not
                              -- contain huge amounts of text, we adopt the kludge of
                              -- allocating a buffer of a fixed multiple of the compressed
                              -- buffer size and hope for the best.
                              -- Pity the PNG design team didn't think to include the size
                              -- of the text in the chunk! It would have been easily done
                              -- with a few extra bytes after the compression method.

                              declare
                                 DP : Buffer_Pointer := new Buffer (1 .. T'Length * 10);
                                 D : Buffer renames DP.all;

                                 use ZLib;

                                 Z : Filter_Type;

                                 I, O : Stream_Element_Offset;
                              begin

                                 Inflate_Init (Z);
                                 Translate (Z, T (T'First + 1 .. T'Last), I, D, O, Finish);

                                 pragma Assert (I = T'Last); -- There doesn't seem to be any similar
                                 -- check we can do on the value of O.
                                 Close (Z);

                                 -- Copy the text out of the D buffer into the text item.

                                 F.Text_Strings.Text_String := new String (1 .. Natural (O));
                                 for I in F.Text_Strings.Text_String'Range loop
                                    F.Text_Strings.Text_String (I) := Character'Val (D (Stream_Element_Offset (I)));
                                 end loop;

                                 Deallocate (DP);
                              end;
                           elsif Chunk_Type = tEXt then

                              -- Copy the text out of the T buffer into the text item.

                              F.Text_Strings.Text_String := new String (1 .. T'Length);
                              for I in F.Text_Strings.Text_String'Range loop
                                 F.Text_Strings.Text_String (I) := Character'Val (T (T'First - 1 + Stream_Element_Offset (I)));
                              end loop;
                           else
                              raise Program_Error;
                           end if;

                        end;
                     end;

                  when others =>
                     if Ancillary (Chunk_Type) then -- add it to the linked list in the descriptor.
                        F.Number_of_Chunks := F.Number_of_Chunks + 1;
                        declare
                           Temp : constant Chunk_List := new Chunk_List_Element (Chunk_Length);
                           C : Chunk renames Temp.Chnk;
                        begin
                           C.Name := To_Chunk_Name (Chunk_Type);
                           for X in C.Data'Range loop
                              C.Data (X) := B (X + 4);
                           end loop;

                           if Known_Chunk (Chunk_Type) then

                              -- We know where this chunk should occur.  We can verify that the
                              -- position in the file is legal, AND set the Where field to what
                              -- it should be.

                              C.Where := Position (Chunk_Type);

                              if Before_PLTE (Chunk_Type) and PLTE_Flag then
                                 Raise_Exception
                                   (Format_Error'Identity,
                                    To_Chunk_Name (Chunk_Type) & " found after PLTE chunk.");
                              elsif Before_IDAT (Chunk_Type) and IDAT_Flag then
                                 Raise_Exception
                                   (Format_Error'Identity,
                                    To_Chunk_Name (Chunk_Type) & " found after IDAT chunk.");
                              end if;

                           else

                              -- We don't know where this chunk should occur, so we cannot
                              -- check the legality of its positioning in the file. We have
                              -- to deduce the Where field from the position actually found
                              -- in the file, although we may not yet have seen the PLTE if
                              -- present. This isn't totally satisfactory, but it is unlikely
                              -- to cause problems in practice, since it is unlikely that an
                              -- unknown ancillary chunk must precede PLTE.

                              if IDAT_Flag then
                                 C.Where := Anywhere;    -- The chunk was found after IDAT.
                              else
                                 C.Where := Before_IDAT; -- The chunk was found before IDAT.
                              end if;
                           end if;
                           Temp.Link          := F.Ancillary_Chunks;
                           F.Ancillary_Chunks := Temp;
                        end;
                     else
                        Raise_Exception (Format_Error'Identity, "Unknown critical chunk " & To_Chunk_Name (Chunk_Type) & '.');
                     end if;
               end case;
               Previous_Chunk_Type := Chunk_Type;
               Deallocate (BP);
            exception
               when others =>
                  Deallocate (BP);
                  raise;
            end;
         end loop For_each_chunk;

         if IDAT_Flag then

            -- We have seen one or more IDAT chunks and therefore the Zlib filter will have been
            -- initialised/opened, and we must close it. Check also that all the IDAT data has
            -- come out of ZLib.

            if ZLib.Stream_End (Z) and ZLib.Total_Out (Z) = F.Uncompressed_Data.all'Length then
               ZLib.Close (Z);
               IDAT_Flag := False; -- This indicates that the Zlib filter has been closed.
            else
               Raise_Exception (Format_Error'Identity, "IDAT data has not been correctly decompressed.");
            end if;
         end if;
      exception
         when others =>
            if IDAT_Flag then
               ZLib.Close (Z);
            end if;
            raise;
      end;
   end Read_Chunks;

   -- The uncompressed data must now be processed to extract the pixel
   -- values. This requires defiltering (the inverse of the scanline-based
   -- filtering operation carried out when the PNG file was created.
   -- The defiltering can be done in place in the buffer, because once a
   -- pixel has been filtered the unfiltered value is no longer needed.
   -- There is a slight trick required to cope with the first line and
   -- first column, where pixels to the left or before the top of the
   -- image are defined to be zero for filtering.

   Defilter : declare

      Data : Stream_Element_Array renames F.Uncompressed_Data.all;

      procedure Check_Filter_Type (B : in Stream_Element) is
      begin
         if B > 4 then
            Raise_Exception (Format_Error'Identity, "Illegal filter type byte.");
         end if;
      end Check_Filter_Type;
      pragma Inline (Check_Filter_Type);

      -- We need to know the number of bytes per pixel, which is defined
      -- to be 1 for bit depths of 1, 2, or 4. This value is used in
      -- accessing the previous scanline byte at the same column position.

      Bpp : constant Natural := Bytes_per_Pixel (Colour_Type (F), Bit_Depth (F));

      -- To allow for the case of filtering sub-images in interlaced
      -- PNG files we provide a procedure to deal with all cases. The
      -- parameters are the number of lines to be filtered, the number
      -- of pixels in each line (excluding the filter type byte) and
      -- the index in the buffer of the filter type byte of the first line.

      Pointer : Stream_Element_Offset := 1; -- The index in the buffer of the
      -- byte currently being filtered.

      procedure Pass (N_Lines, N_Pixels : in Natural) is
         -- We need to calculate the number of pixel bytes per line, including
         -- an allowance for unused bits at the end of the line if there are
         -- less than 8 bits per pixel.
         W : constant Natural := (N_Pixels * Bits_per_Pixel (Colour_Type (F), Bit_Depth (F)) + 7) / 8;
      begin
         for Y in 1 .. N_Lines loop -- We have to allow for the possibility that a pass
            -- has zero columns or rows as described in Section
            -- 8.2 of the ISO standard. If N_Lines is zero this
            -- loop will execute zero times and Pass will do nothing.
            exit when W = 0;         -- If W is zero we need to skip the code in the loop.
            declare
               -- P is the index of the first byte of the previous scanline, except when Y = 1.
               P : constant Stream_Element_Offset := Stream_Element_Offset'Max (1, Pointer - Stream_Element_Offset (W + 1));
               FT : Stream_Element renames Data (Pointer);
            begin
               Check_Filter_Type (FT);
               Pointer := Pointer + 1;   -- Skip the filter type byte.
               for X in 1 .. W loop      -- For each byte to be filtered.

                  -- The code in this loop should be recoded to use a function
                  -- to operate on each scanline as is done for the write PNG
                  -- part of the package which was written later in a much more
                  -- hygienic manner!

                  declare
                     Current : Stream_Element renames Data (Pointer);

                     function Previous return Stream_Element is
                     begin
                        if X <= Bpp then
                           return 0;
                        else
                           return Data (Pointer - Stream_Element_Offset (Bpp));
                        end if;
                     end Previous;

                     function Prior return Stream_Element is
                     begin
                        if Y = 1 then
                           return 0;
                        else
                           return Data (P + Stream_Element_Offset (X));
                        end if;
                     end Prior;

                     function PrevPrior return Stream_Element is
                     begin
                        if X <= Bpp or Y = 1 then
                           return 0;
                        else
                           return Data (P + Stream_Element_Offset (X - Bpp));
                        end if;
                     end PrevPrior;
                     pragma Inline (Previous, Prior, PrevPrior);

                  begin
                     case FT is
                        when None =>
                           null; -- There is no filtering to do.
                        when Sub =>
                           Current := Current + Previous;
                        when Up =>
                           Current := Current + Prior;
                        when Average =>
                           Current := Current + Mean (Previous, Prior);
                        when Paeth =>
                           Current := Current + PaethPredictor (Previous, Prior, PrevPrior);
                        when others =>
                           raise Program_Error; -- Can't happen.
                     end case;
                  end;
                  Pointer := Pointer + 1; -- Skip the byte just filtered.
               end loop;
            end;
         end loop;
      end Pass;

   begin
      case Interlaced (F) is
         when False =>
            Pass (Height (F), Width (F)); -- There is only one pass.
         when True =>

            -- We have a tricky calculation to do here. We have to calculate
            -- from the actual dimension of the image (width or height) the
            -- dimension of the reduced image on each pass. We do this from
            -- tables in the Adam7 package.

            for P in Pass_Number loop
               F.Interlace_Offsets (P) := Pointer; -- Remember for use in reading
               -- the pixels later.
               Pass (Sub_Image_Height (Height (F), P), Sub_Image_Width (Width (F), P));
            end loop;
      end case;

      if Pointer /= F.Uncompressed_Data.all'Length + 1 then
         Raise_Exception (Format_Error'Identity, "Incorrect length of uncompressed image data.");
      end if;

   end Defilter;

exception
   when others =>
      if F /= null then
         Close (F);
      end if;
      raise;
end Open;
