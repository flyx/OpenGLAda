--  GIF Decoder by André van Splunter
--
--  A GIF stream is made of several "blocks".
--  The image itself is contained in an Image Descriptor block.
--
with GID.Buffering, GID.Color_tables;

with Ada.Text_IO;

package body GID.Decoding_GIF is

  generic
    type Number is mod <>;
  procedure Read_Intel_x86_number(
    from : in out Input_buffer;
    n    :    out Number
  );
    pragma Inline(Read_Intel_x86_number);

  procedure Read_Intel_x86_number(
    from : in out Input_buffer;
    n    :    out Number
  )
  is
    b: U8;
    m: Number:= 1;
  begin
    n:= 0;
    for i in 1..Number'Size/8 loop
      GID.Buffering.Get_Byte(from, b);
      n:= n + m * Number(b);
      m:= m * 256;
    end loop;
  end Read_Intel_x86_number;

  procedure Read_Intel is new Read_Intel_x86_number( U16 );

  ----------
  -- Load --
  ----------

  procedure Load (
    image     : in out Image_descriptor;
    next_frame:    out Ada.Calendar.Day_Duration
  )
  is
    local: Image_descriptor;
    --  With GIF, each frame is a local image with an eventual
    --  palette, different dimensions, etc. ...

    use GID.Buffering;

    type GIFDescriptor is record
      ImageLeft,
      ImageTop,
      ImageWidth,
      ImageHeight : U16;
      Depth       : U8;
    end record;

    --  For loading from the GIF file
    Descriptor : GIFDescriptor;

    --  Coordinates
    X, tlX, brX : Natural;
    Y, tlY, brY : Natural;

    --  Code information
    subtype Code_size_range is Natural range 2..12;
    CurrSize : Code_size_range;

    subtype Color_type is U8;
    Transp_color   : Color_type:= 0;

    --  GIF data is stored in blocks and sub-blocks.
    --  We initialize block_read and block_size to force
    --  reading and buffering the next sub-block
    block_size   : Natural:= 0;
    block_read   : Natural:= 0;

    function Read_Byte return U8 is
    pragma Inline(Read_Byte);
      b: U8;
    begin
      if block_read >= block_size then
        Get_Byte(image.buffer, b);
        block_size:= Natural(b);
        block_read:= 0;
      end if;
      Get_Byte(image.buffer, b);
      block_read:= block_read + 1;
      return b;
    end Read_Byte;

    --  Used while reading the codes
    bits_in : U8:= 8;
    bits_buf: U8;

    --  Local procedure to read the next code from the file
    function Read_Code return Natural is
      bit_mask: Natural:= 1;
      code: Natural:= 0;
    begin
      --  Read the code, bit by bit
      for Counter  in reverse  0..CurrSize - 1  loop
        --  Next bit
        bits_in:= bits_in + 1;
        --  Maybe, a new byte needs to be loaded with a further 8 bits
        if bits_in = 9 then
          bits_buf:= Read_Byte;
          bits_in := 1;
        end if;
        --  Add the current bit to the code
        if (bits_buf  and  1) > 0 then
          code:= code + bit_mask;
        end if;
        bit_mask := bit_mask * 2;
        bits_buf := bits_buf / 2;
      end loop;
      return code;
    end Read_Code;

    generic
      --  Parameter(s) that are constant through
      --  the whole image. Macro-expanded generics and
      --  some optimization will trim corresponding "if's"
      interlaced     : Boolean;
      transparency   : Boolean;
      pixel_mask     : U32;
      --
    procedure GIF_Decode;

    procedure GIF_Decode is

      procedure Pixel_with_palette(b: U8) is
      pragma Inline(Pixel_with_palette);
        function Times_257(x: Primary_color_range) return Primary_color_range is
        pragma Inline(Times_257);
        begin
          return 16 * (16 * x) + x;  --  this is 257 * x, = 16#0101# * x
          --  Numbers 8-bit -> no OA warning at instanciation. Returns x if type Primary_color_range is mod 2**8.
        end Times_257;
        full_opaque: constant Primary_color_range:= Primary_color_range'Last;
      begin
        if transparency and then b = Transp_color then
          Put_Pixel(0,0,0, 0);
          return;
        end if;
        case Primary_color_range'Modulus is
          when 256 =>
            Put_Pixel(
              Primary_color_range(local.palette(Integer(b)).red),
              Primary_color_range(local.palette(Integer(b)).green),
              Primary_color_range(local.palette(Integer(b)).blue),
              full_opaque
            );
          when 65_536 =>
            Put_Pixel(
              Times_257(Primary_color_range(local.palette(Integer(b)).red)),
              Times_257(Primary_color_range(local.palette(Integer(b)).green)),
              Times_257(Primary_color_range(local.palette(Integer(b)).blue)),
              --  Times_257 makes max intensity FF go to FFFF
              full_opaque
            );
          when others =>
            raise invalid_primary_color_range with "GIF: color range not supported";
        end case;
      end Pixel_with_palette;

      --  Interlacing
      Interlace_pass : Natural range 1..4:= 1;
      Span           : Natural:= 7;

      --  Local procedure to draw a pixel
      procedure Next_Pixel(code: Natural) is
      pragma Inline(Next_Pixel);
        c : constant Color_type:= Color_type(U32(code) and pixel_mask);
      begin
        --  Actually draw the pixel on screen buffer
        if X < Integer (image.width) then
          if interlaced and mode = nice then
            for i in reverse 0 .. Span loop
              if Y+i < Integer (image.height) then
                Set_X_Y(X, Integer (image.height) - (Y+i) - 1);
                Pixel_with_palette(c);
              end if;
            end loop;
          elsif Y < Integer (image.height) then
            Pixel_with_palette(c);
          end if;
        end if;

        --  Move on to next pixel
        X:= X + 1;

        --  Or next row, if necessary
        if X = brX then
          X:= tlX;
          if interlaced then
            case Interlace_pass is
              when 1 =>
                Y:= Y + 8;
                if Y >= brY then
                  Y:= 4;
                  Interlace_pass:= 2;
                  Span:= 3;
                  Feedback((Interlace_pass*100)/4);
                end if;
              when 2 =>
                Y:= Y + 8;
                if Y >= brY then
                  Y:= 2;
                  Interlace_pass:= 3;
                  Span:= 1;
                  Feedback((Interlace_pass*100)/4);
                end if;
              when 3 =>
                Y:= Y + 4;
                if Y >= brY then
                  Y:= 1;
                  Interlace_pass:= 4;
                  Span:= 0;
                  Feedback((Interlace_pass*100)/4);
                end if;
              when 4 =>
                Y:= Y + 2;
            end case;
            if mode = fast and then Y < Integer (image.height) then
              Set_X_Y(X, Integer (image.height) - Y - 1);
            end if;
          else -- not interlaced
            Y:= Y + 1;
            if Y < Integer (image.height) then
              Set_X_Y(X, Integer (image.height) - Y - 1);
            end if;
            if Y mod 32 = 0 then
              Feedback((Y*100) / Integer (image.height));
            end if;
          end if;
        end if;
      end Next_Pixel;

      --  The string table
      Prefix : array ( 0..4096 ) of Natural:= (others => 0);
      Suffix : array ( 0..4096 ) of Natural:= (others => 0);
      --  Top of Stack was 1024 until files from
      --    https://www.kaggle.com/c/carvana-image-masking-challenge
      --  broke it (July 2017)...
      Stack  : array ( 0..2048 ) of Natural;

      --  Special codes (specific to GIF's flavour of LZW)
      ClearCode : constant Natural:= 2 ** CurrSize; -- Reset code
      EndingCode: constant Natural:= ClearCode + 1; -- End of file
      FirstFree : constant Natural:= ClearCode + 2; -- Strings start here

      Slot         : Natural:= FirstFree;  --  Last read code
      InitCodeSize : constant Code_size_range:= CurrSize + 1;
      TopSlot      : Natural:= 2 ** InitCodeSize; --  Highest code for current size
      Code         : Natural;
      StackPtr     : Integer:= 0;
      Fc           : Integer:= 0;
      Oc           : Integer:= 0;
      C            : Integer;
      BadCodeCount : Natural:= 0;  --  the number of bad codes found

    begin -- GIF_Decode
      --  The decoder source and the cool comments are kindly donated by
      --  André van Splunter.
      --
      CurrSize:= InitCodeSize;
      --  This is the main loop.  For each code we get we pass through the
      --  linked list of prefix codes, pushing the corresponding "character"
      --  for each code onto the stack.  When the list reaches a single
      --  "character" we push that on the stack too, and then start unstacking
      --  each character for output in the correct order.  Special handling is
      --  included for the clear code, and the whole thing ends when we get
      --  an ending code.
      C := Read_Code;
      while C /= EndingCode loop
         --  If the code is a clear code, reinitialize all necessary items.
         if C = ClearCode then
            CurrSize := InitCodeSize;
            Slot     := FirstFree;
            TopSlot  := 2 ** CurrSize;
            --  Continue reading codes until we get a non-clear code
            --  (Another unlikely, but possible case...)
            C := Read_Code;
            while C = ClearCode loop
               C := Read_Code;
            end loop;
            --  If we get an ending code immediately after a clear code
            --  (Yet another unlikely case), then break out of the loop.
            exit when C = EndingCode;
            --  Finally, if the code is beyond the range of already set codes,
            --  (This one had better NOT happen...  I have no idea what will
            --  result from this, but I doubt it will look good...) then set
            --  it to color zero.
            if C >= Slot then
               C := 0;
            end if;
            Oc := C;
            Fc := C;
            --  And let us not forget to output the char...
            Next_Pixel(C);
         else  --  C /= ClearCode
            --  In this case, it's not a clear code or an ending code, so
            --  it must be a code code...  So we can now decode the code into
            --  a stack of character codes. (Clear as mud, right?)
            Code := C;
            --  Here we go again with one of those off chances...  If, on the
            --  off chance, the code we got is beyond the range of those
            --  already set up (Another thing which had better NOT happen...)
            --  we trick the decoder into thinking it actually got the last
            --  code read. (Hmmn... I'm not sure why this works...
            --  But it does...)
            if Code >= Slot then
               if Code > Slot then
                  BadCodeCount := BadCodeCount + 1;
               end if;
               Code := Oc;
               Stack (StackPtr) := Fc rem 256;
               StackPtr := StackPtr + 1;
            end if;
            --  Here we scan back along the linked list of prefixes, pushing
            --  helpless characters (ie. suffixes) onto the stack as we do so.
            while Code >= FirstFree loop
               Stack (StackPtr) := Suffix (Code);
               StackPtr := StackPtr + 1;
               Code := Prefix (Code);
            end loop;
            --  Push the last character on the stack, and set up the new
            --  prefix and suffix, and if the required slot number is greater
            --  than that allowed by the current bit size, increase the bit
            --  size.  (NOTE - If we are all full, we *don't* save the new
            --  suffix and prefix...  I'm not certain if this is correct...
            --  it might be more proper to overwrite the last code...
            Stack (StackPtr) := Code rem 256;
            if Slot < TopSlot then
               Suffix (Slot) := Code rem 256;
               Fc := Code;
               Prefix (Slot) := Oc;
               Slot := Slot + 1;
               Oc := C;
            end if;
            if Slot >= TopSlot then
               if CurrSize < 12 then
                  TopSlot := TopSlot * 2;
                  CurrSize := CurrSize + 1;
               end if;
            end if;
            --  Now that we've pushed the decoded string (in reverse order)
            --  onto the stack, lets pop it off and output it...
            loop
               Next_Pixel(Stack (StackPtr));
               exit when StackPtr = 0;
               StackPtr := StackPtr - 1;
            end loop;
         end if;
         C := Read_Code;
      end loop;
      if full_trace and then BadCodeCount > 0 then
        Ada.Text_IO.Put_Line(
         "Found" & Integer'Image(BadCodeCount) &
         " bad codes"
        );
      end if;
    end GIF_Decode;

    --  Here we have several specialized instances of GIF_Decode,
    --  with parameters known at compile-time -> optimizing compilers
    --  will do expensive tests about interlacing and transparency at compile-time,
    --  not at run-time.
    --
    procedure GIF_Decode_interlaced_transparent_8 is
      new GIF_Decode(interlaced => True,  transparency => True,  pixel_mask => 255);
    procedure GIF_Decode_straight_transparent_8 is
      new GIF_Decode(interlaced => False, transparency => True,  pixel_mask => 255);
    procedure GIF_Decode_interlaced_opaque_8 is
      new GIF_Decode(interlaced => True,  transparency => False, pixel_mask => 255);
    procedure GIF_Decode_straight_opaque_8 is
      new GIF_Decode(interlaced => False, transparency => False, pixel_mask => 255);
    --
    procedure Skip_sub_blocks is
      temp: U8;
    begin
       sub_blocks_sequence:
       loop
        Get_Byte(image.buffer, temp ); -- load sub-block length byte
        exit sub_blocks_sequence when temp = 0;
        --  null sub-block = end of sub-block sequence
        for i in 1..temp loop
          Get_Byte(image.buffer, temp ); -- load sub-block byte
        end loop;
      end loop sub_blocks_sequence;
    end Skip_sub_blocks;

    temp, temp2, label: U8;
    delay_frame: U16;
    c: Character;
    frame_interlaced: Boolean;
    frame_transparency: Boolean:= False;
    local_palette  : Boolean;
    --
    separator :  Character ;
    --  Colour information
    new_num_of_colours : Natural;
    pixel_mask : U32;
    BitsPerPixel  : Natural;

  begin -- Load
    next_frame:= 0.0;
    --  Scan various GIF blocks, until finding an image
    loop
      Get_Byte(image.buffer, temp);
      separator:= Character'Val(temp);
      if full_trace then
        Ada.Text_IO.Put(
          "GIF separator [" & separator &
          "][" & U8'Image(temp) & ']'
        );
      end if;
      case separator is
        when ',' => -- 16#2C#
          exit;
          --  Image descriptor will begin
          --  See: 20. Image Descriptor
        when ';' => -- 16#3B#
          if full_trace then
            Ada.Text_IO.Put(" - End of GIF");
          end if;
          image.next_frame:= 0.0;
          next_frame:= image.next_frame;
          return; -- End of GIF image
        when '!' => -- 16#21# Extensions
          if full_trace then
            Ada.Text_IO.Put(" - Extension");
          end if;
          Get_Byte(image.buffer, label );
          case label is
            when 16#F9# => -- See: 23. Graphic Control Extension
              if full_trace then
                Ada.Text_IO.Put_Line(" - 16#F9#: Graphic Control Extension");
              end if;
              Get_Byte(image.buffer, temp );
              if temp /= 4 then
                raise error_in_image_data with "GIF: error in Graphic Control Extension";
              end if;
              Get_Byte(image.buffer, temp );
              --  Reserved                      3 Bits
              --  Disposal Method               3 Bits
              --  User Input Flag               1 Bit
              --  Transparent Color Flag        1 Bit
              frame_transparency:= (temp and 1) = 1;
              Read_Intel(image.buffer, delay_frame);
              image.next_frame:=
                image.next_frame + Ada.Calendar.Day_Duration(delay_frame) / 100.0;
              next_frame:= image.next_frame;
              Get_Byte(image.buffer, temp );
              Transp_color:= Color_type(temp);
              --  zero sub-block:
              Get_Byte(image.buffer, temp );
            when 16#FE# => -- See: 24. Comment Extension
              if full_trace then
                Ada.Text_IO.Put_Line(" - 16#FE#: Comment Extension");
                sub_blocks_sequence:
                loop
                  Get_Byte(image.buffer, temp ); -- load sub-block length byte
                  exit sub_blocks_sequence when temp = 0;
                  --  null sub-block = end of sub-block sequence
                  for i in 1..temp loop
                    Get_Byte(image.buffer, temp2);
                    c:= Character'Val(temp2);
                    Ada.Text_IO.Put(c);
                  end loop;
                end loop sub_blocks_sequence;
                Ada.Text_IO.New_Line;
              else
                Skip_sub_blocks;
              end if;
            when 16#01# => -- See: 25. Plain Text Extension
              if full_trace then
                Ada.Text_IO.Put_Line(" - 16#01#: Plain Text Extension");
              end if;
              Skip_sub_blocks;
            when 16#FF# => -- See: 26. Application Extension
              if full_trace then
                Ada.Text_IO.Put_Line(" - 16#FF#: Application Extension");
              end if;
              Skip_sub_blocks;
            when others =>
              if full_trace then
                Ada.Text_IO.Put_Line(" - Unused extension:" & U8'Image(label));
              end if;
              Skip_sub_blocks;
          end case;
        when ASCII.NUL =>
          --  Occurs in some buggy GIFs (2016).
          --  Seems a 2nd zero, the 1st marking the end of sub-block list.
          if full_trace then
            Ada.Text_IO.Put_Line(" - Wrong separator, skip and hope for the better...");
          end if;
        when others =>
          raise error_in_image_data with
            "GIF: unknown separator: [" & separator &
            "] code:" & Integer'Image(Character'Pos(separator));
      end case;
    end loop;

    --  Load the image descriptor
    Read_Intel(image.buffer, Descriptor.ImageLeft);
    Read_Intel(image.buffer, Descriptor.ImageTop);
    Read_Intel(image.buffer, Descriptor.ImageWidth);
    Read_Intel(image.buffer, Descriptor.ImageHeight);
    Get_Byte(image.buffer, Descriptor.Depth);

    --  Get image corner coordinates
    tlX := Natural(Descriptor.ImageLeft);
    tlY := Natural(Descriptor.ImageTop);
    brX := tlX + Natural(Descriptor.ImageWidth);
    brY := tlY + Natural(Descriptor.ImageHeight);

    --  Local Color Table Flag        1 Bit
    --  Interlace Flag                1 Bit
    --  Sort Flag                     1 Bit
    --  Reserved                      2 Bits
    --  Size of Local Color Table     3 Bits
    --
    frame_interlaced:= (Descriptor.Depth and 64) = 64;
    local_palette:= (Descriptor.Depth and 128) = 128;
    local.format:= GIF;
    local.stream:= image.stream;
    local.buffer:= image.buffer;
    if local_palette then
      --  Get amount of colours in image
      BitsPerPixel := 1 + Natural(Descriptor.Depth and 7);
      new_num_of_colours:= 2 ** BitsPerPixel;
      --  21. Local Color Table
      local.palette:= new Color_table(0..new_num_of_colours-1);
      Color_tables.Load_palette(local);
      image.buffer:= local.buffer;
    elsif image.palette = null then
      raise error_in_image_data with "GIF: neither local, nor global palette";
    else
      --  Use global palette
      new_num_of_colours:= 2 ** image.subformat_id;
      --  usually <= 2** image.bits_per_pixel
      --  Just copy main palette
      local.palette:= new Color_table'(image.palette.all);
    end if;
    pixel_mask:= U32(new_num_of_colours - 1);

    if full_trace then
      Ada.Text_IO.Put_Line(
        " - Image, interlaced: " & Boolean'Image(frame_interlaced) &
        "; local palette: " & Boolean'Image(local_palette) &
        "; transparency: " & Boolean'Image(frame_transparency) &
        "; transparency index:" & Color_type'Image(Transp_color)
      );
    end if;

    --  Get initial code size
    Get_Byte(image.buffer, temp );
    if Natural(temp) not in Code_size_range then
      raise error_in_image_data with
        "GIF: wrong LZW code size (must be in 2..12), is" &
        U8'Image(temp);
    end if;
    CurrSize := Natural(temp);

    --  Start at top left of image
    X := Natural(Descriptor.ImageLeft);
    Y := Natural(Descriptor.ImageTop);
    Set_X_Y(X, Integer (image.height) - Y - 1);
    --
    if new_num_of_colours < 256 then
      --  "Rare" formats -> no need of best speed
      declare
        --  We create an instance with dynamic parameters
        procedure GIF_Decode_general is
          new GIF_Decode(frame_interlaced, frame_transparency, pixel_mask);
      begin
        GIF_Decode_general;
      end;
    else
      --  8 bit, usual format: we try to make things
      --  faster with specialized instanciations...
      if frame_interlaced then
        if frame_transparency then
          GIF_Decode_interlaced_transparent_8;
        else
          GIF_Decode_interlaced_opaque_8;
        end if;
      else -- straight (non-interlaced)
        if frame_transparency then
          GIF_Decode_straight_transparent_8;
        else
          GIF_Decode_straight_opaque_8;
        end if;
      end if;
    end if;
    Feedback(100);
    --
    Get_Byte(image.buffer, temp ); -- zero-size sub-block
  end Load;

end GID.Decoding_GIF;
