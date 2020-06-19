---------------------------------
-- GID - Generic Image Decoder --
---------------------------------
--
--  Private child of GID, with helpers for identifying
--  image formats and reading header informations.
--

with GID.Buffering,
     GID.Color_tables,
     GID.Decoding_JPG,
     GID.Decoding_PNG,
     GID.Decoding_PNM;

with Ada.Unchecked_Deallocation;

package body GID.Headers is

  -------------------------------------------------------
  -- The very first: read signature to identify format --
  -------------------------------------------------------

  procedure Load_signature (
    image   : in out Image_descriptor;
    try_tga :        Boolean:= False

  )
  is
    use Bounded_255;
    c: Character;
    FITS_challenge : String(1..5); -- without the initial
    GIF_challenge  : String(1..5); -- without the initial
    PNG_challenge  : String(1..7); -- without the initial
    PNG_signature  : constant String:= "PNG" & ASCII.CR & ASCII.LF & ASCII.SUB & ASCII.LF;
    PNM_challenge  : Character;
    TIFF_challenge : String(1..3); -- without the initial
    TIFF_signature : String(1..2);
    procedure Dispose is
      new Ada.Unchecked_Deallocation(Color_table, p_Color_table);
  begin
    --  Some cleanup
    Dispose(image.palette);
    image.next_frame:= 0.0;
    image.display_orientation:= Unchanged;
    --
    Character'Read(image.stream, c);
    image.first_byte:= Character'Pos(c);
    case c is
      when 'B' =>
        Character'Read(image.stream, c);
        if c='M' then
          image.detailed_format:= To_Bounded_String("BMP");
          image.format:= BMP;
          return;
        end if;
      when 'S' =>
        String'Read(image.stream, FITS_challenge);
        if FITS_challenge = "IMPLE"  then
          image.detailed_format:= To_Bounded_String("FITS");
          image.format:= FITS;
          return;
        end if;
      when 'G' =>
        String'Read(image.stream, GIF_challenge);
        if GIF_challenge = "IF87a" or GIF_challenge = "IF89a" then
          image.detailed_format:= To_Bounded_String('G' & GIF_challenge & ", ");
          image.format:= GIF;
          return;
        end if;
      when 'I' | 'M' =>
        String'Read(image.stream, TIFF_challenge);
        if c = TIFF_challenge(1) then
          --  TIFF begins either with II (Intel) or MM (Motorola) - TIFF 6.0 Specification p.13
          if c = 'I' then
            image.detailed_format:= To_Bounded_String("TIFF, little-endian");
            image.endianess:= little;
            TIFF_signature:= '*' & ASCII.NUL; -- 42 (The Answer) on 16 bits
          else
            image.detailed_format:= To_Bounded_String("TIFF, big-endian");
            image.endianess:= big;
            TIFF_signature:= ASCII.NUL & '*'; -- 42 (The Answer) on 16 bits
          end if;
          if TIFF_challenge(2..3) = TIFF_signature then
            image.format:= TIFF;
            return;
          end if;
        end if;
      when Character'Val(16#FF#) =>
        Character'Read(image.stream, c);
        if c=Character'Val(16#D8#) then
          --  SOI (Start of Image) segment marker (FFD8)
          image.detailed_format:= To_Bounded_String("JPEG");
          image.format:= JPEG;
          return;
        end if;
      when Character'Val(16#89#) =>
        String'Read(image.stream, PNG_challenge);
        if PNG_challenge = PNG_signature  then
          image.detailed_format:= To_Bounded_String("PNG");
          image.format:= PNG;
          return;
        end if;
      when 'P' =>
        Character'Read(image.stream, PNM_challenge);
        if PNM_challenge in '1'..'6' then
          image.detailed_format:= To_Bounded_String("PNM (PBM, PGM or PPM)");
          image.format:= PNM;
          image.subformat_id:= Integer'Value((1 => PNM_challenge));
          return;
        end if;

      when others =>
        if try_tga then
          image.detailed_format:= To_Bounded_String("TGA");
          image.format:= TGA;
          return;
        else
          raise unknown_image_format;
        end if;
    end case;
    raise unknown_image_format;
  end Load_signature;

  --  Define reading of unsigned numbers from a byte stream

  --  Little-endian
  generic
    type Number is mod <>;
  procedure Read_Intel_x86_number(
    from : in     Stream_Access;
    n    :    out Number
  );
    pragma Inline(Read_Intel_x86_number);

  generic
    type Number is mod <>;
  procedure Big_endian_number(
    from : in     Stream_Access;
    n    :    out Number
  );
    pragma Inline(Big_endian_number);

  generic
    type Number is mod <>;
  procedure Big_endian_number_buffered(
    from : in out Input_buffer;
    n    :    out Number
  );
    pragma Inline(Big_endian_number_buffered);

  generic
    type Number is mod <>;
  procedure Read_any_endian_number(
    from : in     Stream_Access;
    n    :    out Number;
    endi : in     Endianess_type
  );
    pragma Inline(Read_any_endian_number);

  --  Implementations

  procedure Read_Intel_x86_number(
    from : in     Stream_Access;
    n    :    out Number
  )
  is
    b: U8;
    m: Number:= 1;
  begin
    n:= 0;
    for i in 1..Number'Size/8 loop
      U8'Read(from, b);
      n:= n + m * Number(b);
      m:= m * 256;
    end loop;
  end Read_Intel_x86_number;

  procedure Big_endian_number(
    from : in     Stream_Access;
    n    :    out Number
  )
  is
    b: U8;
  begin
    n:= 0;
    for i in 1..Number'Size/8 loop
      U8'Read(from, b);
      n:= n * 256 + Number(b);
    end loop;
  end Big_endian_number;

  procedure Big_endian_number_buffered(
    from : in out Input_buffer;
    n    :    out Number
  )
  is
    b: U8;
  begin
    n:= 0;
    for i in 1..Number'Size/8 loop
      Buffering.Get_Byte(from, b);
      n:= n * 256 + Number(b);
    end loop;
  end Big_endian_number_buffered;

  procedure Read_any_endian_number(
    from : in     Stream_Access;
    n    :    out Number;
    endi : in     Endianess_type
  )
  is
    procedure Read_Intel is new Read_Intel_x86_number(Number);
    procedure Big_endian is new Big_endian_number(Number);
  begin
    case endi is
      when little => Read_Intel(from, n);
      when big =>    Big_endian(from, n);
    end case;
  end Read_any_endian_number;

  --  Instantiations

  procedure Read_Intel is new Read_Intel_x86_number( U16 );
  procedure Read_Intel is new Read_Intel_x86_number( U32 );
  procedure Big_endian_buffered is new Big_endian_number_buffered( U32 );
  procedure Read_any_endian is new Read_any_endian_number( U32 );

  ----------------------------------------------------------
  -- Loading of various format's headers (past signature) --
  ----------------------------------------------------------

  ----------------
  -- BMP header --
  ----------------

  procedure Load_BMP_header (image: in out Image_descriptor) is
    n, dummy: U32;
    pragma Warnings(off, dummy);
    w, dummy16: U16;
    pragma Warnings(off, dummy16);
  begin
    --   Pos= 3, read the file size
    Read_Intel(image.stream, dummy);
    --   Pos= 7, read four bytes, unknown
    Read_Intel(image.stream, dummy);
    --   Pos= 11, read four bytes offset, file top to bitmap data.
    --            For 256 colors, this is usually 36 04 00 00
    Read_Intel(image.stream, dummy);
    --   Pos= 15. The beginning of Bitmap information header.
    --   Data expected:  28H, denoting 40 byte header
    Read_Intel(image.stream, dummy);
    --   Pos= 19. Bitmap width, in pixels.  Four bytes
    Read_Intel(image.stream, n);
    image.width:=  Positive_32 (n);
    --   Pos= 23. Bitmap height, in pixels.  Four bytes
    Read_Intel(image.stream, n);
    image.height:= Positive_32 (n);
    --   Pos= 27, skip two bytes.  Data is number of Bitmap planes.
    Read_Intel(image.stream, dummy16); -- perform the skip
    --   Pos= 29, Number of bits per pixel
    --   Value 8, denoting 256 color, is expected
    Read_Intel(image.stream, w);
    case w is
      when 1 | 4 | 8 | 24 =>
        null;
      when others =>
        raise unsupported_image_subformat with "BMP bit depth =" & U16'Image(w);
    end case;
    image.bits_per_pixel:= Integer(w);
    --   Pos= 31, read four bytes
    Read_Intel(image.stream, n);          -- Type of compression used
    --  BI_RLE8 = 1
    --  BI_RLE4 = 2
    if n /= 0 then
      raise unsupported_image_subformat with "BMP: RLE compression";
    end if;
    --
    Read_Intel(image.stream, dummy); -- Pos= 35, image size
    Read_Intel(image.stream, dummy); -- Pos= 39, horizontal resolution
    Read_Intel(image.stream, dummy); -- Pos= 43, vertical resolution
    Read_Intel(image.stream, n); -- Pos= 47, number of palette colors
    if image.bits_per_pixel <= 8 then
      if n = 0 then
        image.palette:= new Color_table(0..2**image.bits_per_pixel-1);
      else
        image.palette:= new Color_table(0..Natural(n)-1);
      end if;
    end if;
    Read_Intel(image.stream, dummy); -- Pos= 51, number of important colors
    --   Pos= 55 (36H), - start of palette
    Color_tables.Load_palette(image);
  end Load_BMP_header;

  procedure Load_FITS_header (image: in out Image_descriptor) is
  begin
    raise known_but_unsupported_image_format;
  end Load_FITS_header;

  ----------------
  -- GIF header --
  ----------------

  procedure Load_GIF_header (image: in out Image_descriptor) is
    --  GIF - logical screen descriptor
    screen_width, screen_height           : U16;
    packed, background, aspect_ratio_code : U8;
    global_palette: Boolean;
  begin
    Read_Intel(image.stream, screen_width);
    Read_Intel(image.stream, screen_height);
    if screen_width = 0 then
      raise error_in_image_data with "GIF image: zero width";
    end if;
    if screen_height = 0 then
      raise error_in_image_data with "GIF image: zero height";
    end if;
    image.width:= Positive_32 (screen_width);
    image.height:= Positive_32 (screen_height);
    image.transparency:= True; -- cannot exclude transparency at this level.
    U8'Read(image.stream, packed);
    --  Global Color Table Flag       1 Bit
    --  Color Resolution              3 Bits
    --  Sort Flag                     1 Bit
    --  Size of Global Color Table    3 Bits
    global_palette:= (packed and 16#80#) /= 0;
    image.bits_per_pixel:= Natural((packed and 16#7F#)/16#10#) + 1;
    --  Indicative:
    --  iv) [...] This value should be set to indicate the
    --      richness of the original palette
    U8'Read(image.stream, background);
    U8'Read(image.stream, aspect_ratio_code);
    Buffering.Attach_Stream(image.buffer, image.stream);
    if global_palette then
      image.subformat_id:= 1+(Natural(packed and 16#07#));
      --  palette's bits per pixels, usually <= image's
      --
      --  if image.subformat_id > image.bits_per_pixel then
      --    raise
      --      error_in_image_data with
      --      "GIF: global palette has more colors than the image" &
      --       image.subformat_id'img & image.bits_per_pixel'img;
      --  end if;
      image.palette:= new Color_table(0..2**(image.subformat_id)-1);
      Color_tables.Load_palette(image);
    end if;
  end Load_GIF_header;

  -----------------
  -- JPEG header --
  -----------------

  procedure Load_JPEG_header (image: in out Image_descriptor) is
    --  http://en.wikipedia.org/wiki/JPEG
    use GID.Decoding_JPG, GID.Buffering;
    sh: Segment_head;
    b: U8;
  begin
    --  We have already passed the SOI (Start of Image) segment marker (FFD8).
    image.JPEG_stuff.restart_interval:= 0;
    Attach_Stream(image.buffer, image.stream);
    loop
      Read(image, sh);
      case sh.kind is
        when DHT => -- Huffman Table
          Read_DHT(image, Natural(sh.length));
        when DQT =>
          Read_DQT(image, Natural(sh.length));
        when DRI => -- Restart Interval
          Read_DRI(image);
        when SOF_0 .. SOF_15 =>
          Read_SOF(image, sh);
          exit; -- we've got header-style informations, then it's time to quit
        when APP_1 =>
          Read_EXIF(image, Natural(sh.length));
        when others =>
          --  Skip segment data
          for i in 1..sh.length loop
            Get_Byte(image.buffer, b);
          end loop;
      end case;
    end loop;
  end Load_JPEG_header;

  ----------------
  -- PNG header --
  ----------------

  procedure Load_PNG_header (image: in out Image_descriptor) is
    use Decoding_PNG, Buffering;
    ch: Chunk_head;
    n, dummy: U32;
    pragma Warnings(off, dummy);
    b, color_type: U8;
    palette: Boolean:= False;
  begin
    Buffering.Attach_Stream(image.buffer, image.stream);
    Read(image, ch);
    if ch.kind /= IHDR then
      raise error_in_image_data with "PNG: expected 'IHDR' chunk as first chunk in PNG stream";
    end if;
    Big_endian_buffered(image.buffer, n);
    if n = 0 then
      raise error_in_image_data with "PNG image with zero width";
    end if;
    if n > U32 (Positive_32'Last) then
      raise error_in_image_data with "PNG image: width value too large:" & U32'Image (n);
    end if;
    image.width:=  Positive_32 (n);
    Big_endian_buffered(image.buffer, n);
    if n = 0 then
      raise error_in_image_data with "PNG image with zero height";
    end if;
    if n > U32 (Positive_32'Last) then
      raise error_in_image_data with "PNG image: height value too large:" & U32'Image (n);
    end if;
    image.height:= Positive_32 (n);
    Get_Byte(image.buffer, b);
    if b = 0 then
      raise error_in_image_data with "PNG image: zero bit-per-pixel";
    end if;
    image.bits_per_pixel:= Integer(b);
    Get_Byte(image.buffer, color_type);
    image.subformat_id:= Integer(color_type);
    case color_type is
      when 0 => -- Greyscale
        image.greyscale:= True;
        case image.bits_per_pixel is
          when 1 | 2 | 4 | 8 | 16 =>
            null;
          when others =>
            raise error_in_image_data with
              "PNG, type 0 (greyscale): wrong bit-per-channel depth";
        end case;
      when 2 => -- RGB TrueColor
        case image.bits_per_pixel is
          when 8 | 16 =>
            image.bits_per_pixel:= 3 * image.bits_per_pixel;
          when others =>
            raise error_in_image_data with
              "PNG, type 2 (RGB): wrong bit-per-channel depth";
        end case;
      when 3 => -- RGB with palette
        palette:= True;
        case image.bits_per_pixel is
          when 1 | 2 | 4 | 8 =>
            null;
          when others =>
            raise error_in_image_data with
              "PNG, type 3: wrong bit-per-channel depth";
        end case;
      when 4 => -- Grey & Alpha
        image.greyscale:= True;
        image.transparency:= True;
        case image.bits_per_pixel is
          when 8 | 16 =>
            image.bits_per_pixel:= 2 * image.bits_per_pixel;
          when others =>
            raise error_in_image_data with
              "PNG, type 4 (Greyscale & Alpha): wrong bit-per-channel depth";
        end case;
      when 6 => -- RGBA
        image.transparency:= True;
        case image.bits_per_pixel is
          when 8 | 16 =>
            image.bits_per_pixel:= 4 * image.bits_per_pixel;
          when others =>
            raise error_in_image_data with
              "PNG, type 6 (RGBA): wrong bit-per-channel depth";
        end case;
      when others =>
        raise error_in_image_data with "PNG: unknown color type";
    end case;
    Get_Byte(image.buffer, b);
    if b /= 0 then
      raise error_in_image_data with
        "PNG: unknown compression format; ISO/IEC 15948:2003" &
        " knows only 'method 0' (Deflate)";
    end if;
    Get_Byte(image.buffer, b);
    if b /= 0 then
      raise error_in_image_data with
        "PNG: unknown filtering; ISO/IEC 15948:2003 knows only 'method 0'";
    end if;
    Get_Byte(image.buffer, b);
    image.interlaced:= b = 1; -- Adam7
    Big_endian_buffered(image.buffer, dummy); -- Chunk's CRC
    if palette then
      loop
        Read(image, ch);
        case ch.kind is
          when IEND =>
            raise error_in_image_data with
              "PNG: a palette (PLTE) is expected here, found IEND";
          when PLTE =>
            if ch.length rem 3 /= 0 then
              raise error_in_image_data with
                "PNG: palette chunk byte length must be a multiple of 3";
            end if;
            image.palette:= new Color_table(0..Integer(ch.length/3)-1);
            Color_tables.Load_palette(image);
            Big_endian_buffered(image.buffer, dummy); -- Chunk's CRC
            exit;
          when others =>
            --  Skip chunk data and CRC
            for i in 1..ch.length + 4 loop
              Get_Byte(image.buffer, b);
            end loop;
        end case;
      end loop;
    end if;
  end Load_PNG_header;

  --------------------------------
  -- PNM (PBM, PGM, PPM) header --
  --------------------------------

  procedure Load_PNM_header (image: in out Image_descriptor) is
    use Decoding_PNM;
    depth_val: Integer;
  begin
    image.width := Get_Positive_32 (image.stream);
    case image.subformat_id is
      when 1 | 4 =>
        image.height := Get_Positive_32 (image.stream, needs_EOL => True);
        image.greyscale:= True;
        image.bits_per_pixel:= 3;
      when 2..3 | 5..6 =>
        image.height := Get_Positive_32 (image.stream);
        depth_val := Get_Integer(image.stream, needs_EOL => True);
        if depth_val /= 255 then
          raise unsupported_image_subformat with
            "PNM: maximum depth value" & Integer'Image(depth_val) &
             "; only 255 is supported";
        end if;
        image.greyscale:= image.subformat_id = 2 or image.subformat_id = 5;
        image.bits_per_pixel:= 24;
      when others =>
        raise unsupported_image_subformat with
          "PNM: P" & Integer'Image(image.subformat_id) & " not supported";
    end case;
  exception
    when Constraint_Error =>
      raise error_in_image_data with "PNM: invalid numeric value in PNM header";
  end Load_PNM_header;

  ------------------------
  -- TGA (Targa) header --
  ------------------------

  procedure Load_TGA_header (image: in out Image_descriptor) is
    --  TGA FILE HEADER, p.6
    --
    image_ID_length: U8; -- Field 1
    color_map_type : U8; -- Field 2
    image_type     : U8; -- Field 3
    --  Color Map Specification - Field 4
    first_entry_index   : U16; -- Field 4.1
    color_map_length    : U16; -- Field 4.2
    color_map_entry_size: U8;  -- Field 4.3
    --  Image Specification - Field 5
    x_origin: U16;
    y_origin: U16;
    image_width: U16;
    image_height: U16;
    pixel_depth: U8;
    tga_image_descriptor: U8;
    --
    dummy: U8;
    base_image_type: Integer;
  begin
    --  Read the header
    image_ID_length:= image.first_byte;
    U8'Read(image.stream, color_map_type);
    U8'Read(image.stream, image_type);
    --   Color Map Specification - Field 4
    Read_Intel(image.stream, first_entry_index);
    Read_Intel(image.stream, color_map_length);
    U8'Read(image.stream, color_map_entry_size);
    --   Image Specification - Field 5
    Read_Intel(image.stream, x_origin);
    Read_Intel(image.stream, y_origin);
    Read_Intel(image.stream, image_width);
    Read_Intel(image.stream, image_height);
    U8'Read(image.stream, pixel_depth);
    U8'Read(image.stream, tga_image_descriptor);
    --  Done.
    --
    --  Image type:
    --      1 = 8-bit palette style
    --      2 = Direct [A]RGB image
    --      3 = grayscale
    --      9 = RLE version of Type 1
    --     10 = RLE version of Type 2
    --     11 = RLE version of Type 3
    --
    base_image_type:= U8'Pos(image_type and 7);
    image.RLE_encoded:= (image_type and 8) /= 0;
    --
    if color_map_type /= 0 then
      image.palette:= new Color_table(
        Integer(first_entry_index)..
        Integer(first_entry_index)+Integer(color_map_length)-1
      );
      image.subformat_id:= Integer(color_map_entry_size);
      case image.subformat_id is -- = palette's bit depth
        when 8 =>       -- Grey
          null;
        when 15 => -- RGB 3*5 bit
          null;
        when 16 => -- RGBA 3*5+1 bit
          image.transparency:= True;
        when 24 => -- RGB 3*8 bit
          null;
        when 32 => -- RGBA 4*8 bit
          image.transparency:= True;
        when others =>
          raise error_in_image_data with
            "TGA color map (palette): wrong bit depth:" &
            Integer'Image(image.subformat_id);
      end case;
    end if;
    --
    image.greyscale:= False; -- ev. overridden later
    case base_image_type is
      when 1 =>
        image.greyscale:= color_map_entry_size = 8;
      when 2 =>
        null;
      when 3 =>
        image.greyscale:= True;
      when others =>
        raise unsupported_image_subformat with
          "TGA type =" & Integer'Image(base_image_type);
    end case;

    image.width  := U16'Pos(image_width);
    image.height := U16'Pos(image_height);
    image.bits_per_pixel := U8'Pos(pixel_depth);

    --  Make sure we are loading a supported TGA_type
    case image.bits_per_pixel is
      when 24 | 15 | 8 =>
        null;
      when 32 | 16 =>
        image.transparency:= True;
      when others =>
        raise unsupported_image_subformat with
          "TGA bits per pixels =" & Integer'Image(image.bits_per_pixel) &
          "; supported bpp are: 8, 15, 16, 24, 32";
    end case;
    image.top_first:= (tga_image_descriptor and 32) /= 0;
    --  *** Image and color map data
    --  * Image ID
    for i in 1..image_ID_length loop
      U8'Read( image.stream, dummy );
    end loop;
    --  * Color map data (palette)
    Color_tables.Load_palette(image);
    --  * Image data: Read by Load_image_contents.
  end Load_TGA_header;

  procedure Load_TIFF_header (image: in out Image_descriptor) is
    first_IFD_offset: U32;
    --
    --  IFD: Image File Directory. Basically, the image header.
    --  Issue with TIFF: often the image header is stored after the image data.
    --  This would need streams with Set_Index instead of a general stream
    --  (e.g. a file, not an HTTP stream), or to store the full image data
    --  in a temp buffer. Perhaps we'll do that one day.
  begin
    Read_any_endian(image.stream, first_IFD_offset, image.endianess);
    raise known_but_unsupported_image_format with
      "TIFF is not appropriate for streaming. Use PNG, BMP (lossless) or JPEG instead." &
      "Info: IFD Offset=" & U32'Image(first_IFD_offset);
  end Load_TIFF_header;

end GID.Headers;
