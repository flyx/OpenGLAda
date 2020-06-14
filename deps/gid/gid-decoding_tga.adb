with GID.Buffering;                     use GID.Buffering;
with GID.Color_tables;

package body GID.Decoding_TGA is

  ----------
  -- Load --
  ----------

  procedure Load (image: in out Image_descriptor) is

    procedure Row_start(y: Natural) is
    begin
      if image.top_first then
        Set_X_Y(0, Integer (image.height) - 1 - y);
      else
        Set_X_Y(0, y);
      end if;
    end Row_start;

    -- Run Length Encoding --
    RLE_pixels_remaining: Natural:= 0;
    is_run_packet: Boolean;

    type Pixel is record
      color: RGB_color;
      alpha: U8;
    end record;

    pix, pix_mem: Pixel;

    generic
      bpp: Positive;
      pal: Boolean;
    procedure Get_pixel;
    pragma Inline(Get_pixel);
    --
    procedure Get_pixel is
      idx: Natural;
      p1, p2, c, d: U8;
    begin
      if pal then
        if image.palette'Length <= 256 then
          Get_Byte(image.buffer, p1);
          idx:= Natural(p1);
        else
          Get_Byte(image.buffer, p1);
          Get_Byte(image.buffer, p2);
          idx:= Natural(p1) + Natural(p2) * 256;
        end if;
        idx:= idx + image.palette'First;
        pix.color:= image.palette(idx);
      else
        case bpp is
          when 32 => -- BGRA
            Get_Byte(image.buffer, pix.color.blue);
            Get_Byte(image.buffer, pix.color.green);
            Get_Byte(image.buffer, pix.color.red);
            Get_Byte(image.buffer, pix.alpha);
          when 24 => -- BGR
            Get_Byte(image.buffer, pix.color.blue);
            Get_Byte(image.buffer, pix.color.green);
            Get_Byte(image.buffer, pix.color.red);
          when 16 | 15 => -- 5 bit per channel
            Get_Byte(image.buffer,  c);
            Get_Byte(image.buffer,  d);
            Color_tables.Convert(c, d, pix.color);
            if bpp=16 then
              pix.alpha:= U8((U16(c and 128) * 255)/128);
            end if;
          when 8  => -- Gray
            Get_Byte(image.buffer, pix.color.green);
            pix.color.red:= pix.color.green;
            pix.color.blue:= pix.color.green;
          when others =>
            null;
        end case;
      end if;
    end Get_pixel;

    generic
      bpp: Positive;
      pal: Boolean;
    procedure RLE_Pixel;
    pragma Inline(RLE_Pixel);
    --
    procedure RLE_Pixel is
      tmp: U8;
      procedure Get_pixel_for_RLE is new Get_pixel(bpp, pal);
    begin
      if RLE_pixels_remaining = 0 then -- load RLE code
        Get_Byte(image.buffer, tmp );
        Get_pixel_for_RLE;
        RLE_pixels_remaining:= U8'Pos(tmp and 16#7F#);
        is_run_packet:= (tmp and 16#80#) /= 0;
        if is_run_packet then
          pix_mem:= pix;
        end if;
      else
        if is_run_packet then
          pix:= pix_mem;
        else
          Get_pixel_for_RLE;
        end if;
        RLE_pixels_remaining:= RLE_pixels_remaining - 1;
      end if;
    end RLE_Pixel;

    procedure RLE_pixel_32      is new RLE_Pixel(32, False);
    procedure RLE_pixel_24      is new RLE_Pixel(24, False);
    procedure RLE_pixel_16      is new RLE_Pixel(16, False);
    procedure RLE_pixel_15      is new RLE_Pixel(15, False);
    procedure RLE_pixel_8       is new RLE_Pixel(8,  False);
    procedure RLE_pixel_palette is new RLE_Pixel(1,  True); -- 1: dummy

    procedure Output_Pixel is
    pragma Inline(Output_Pixel);
      function Times_257(x: Primary_color_range) return Primary_color_range is
      pragma Inline(Times_257);
      begin
        return 16 * (16 * x) + x;  --  this is 257 * x, = 16#0101# * x
        --  Numbers 8-bit -> no OA warning at instanciation. Returns x if type Primary_color_range is mod 2**8.
      end Times_257;
    begin
      case Primary_color_range'Modulus is
        when 256 =>
          Put_Pixel(
            Primary_color_range(pix.color.red),
            Primary_color_range(pix.color.green),
            Primary_color_range(pix.color.blue),
            Primary_color_range(pix.alpha)
          );
        when 65_536 =>
          Put_Pixel(
            Times_257(Primary_color_range(pix.color.red)),
            Times_257(Primary_color_range(pix.color.green)),
            Times_257(Primary_color_range(pix.color.blue)),
            Times_257(Primary_color_range(pix.alpha))
            --  Times_257 makes max intensity FF go to FFFF
          );
        when others =>
          raise invalid_primary_color_range with "TGA: color range not supported";
      end case;
    end Output_Pixel;

    procedure Get_RGBA is -- 32 bits : R, G, B, A use 8 bits each.
      procedure Get_pixel_32 is new Get_pixel(32, False);
    begin
      for y in 0 .. Integer (image.height) - 1 loop
        Row_start(y);
        for x in 0 .. Integer (image.width) - 1 loop
          Get_pixel_32;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100) / Integer (image.height));
      end loop;
    end Get_RGBA;

    procedure Get_RGB is -- 24 bits : R, G, B use 8 bits each.
      procedure Get_pixel_24 is new Get_pixel(24, False);
    begin
      for y in 0 .. Integer (image.height) - 1 loop
        Row_start(y);
        for x in 0 .. Integer (image.width) - 1 loop
          Get_pixel_24;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100) / Integer (image.height));
      end loop;
    end Get_RGB;

    procedure Get_16 is -- 16 bits
      procedure Get_pixel_16 is new Get_pixel(16, False);
    begin
      for y in 0 .. Integer (image.height) - 1 loop
        Row_start(y);
        for x in 0 .. Integer (image.width) - 1 loop
          Get_pixel_16;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100) / Integer (image.height));
      end loop;
    end Get_16;

    procedure Get_15 is -- 15 bits
      procedure Get_pixel_15 is new Get_pixel(15, False);
    begin
      for y in 0 .. Integer (image.height) - 1 loop
        Row_start(y);
        for x in 0 .. Integer (image.width) - 1 loop
          Get_pixel_15;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100) / Integer (image.height));
      end loop;
    end Get_15;

    procedure Get_Gray is
      procedure Get_pixel_8  is new Get_pixel(8, False);
    begin
      for y in 0 .. Integer (image.height) - 1 loop
        Row_start(y);
        for x in 0 .. Integer (image.width) - 1 loop
          Get_pixel_8;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100) / Integer (image.height));
      end loop;
    end Get_Gray;

    procedure Get_with_palette is
      procedure Get_pixel_palette  is new Get_pixel(1, True); -- 1: dummy
    begin
      for y in 0 .. Integer (image.height) - 1 loop
        Row_start(y);
        for x in 0 .. Integer (image.width) - 1 loop
          Get_pixel_palette;
          Output_Pixel;
        end loop;
        Feedback(((y+1)*100) / Integer (image.height));
      end loop;
    end Get_with_palette;

  begin
    pix.alpha:= 255; -- opaque is default
    Attach_Stream(image.buffer, image.stream);
    --
    if image.RLE_encoded then
      --  One format check per row
      RLE_pixels_remaining:= 0;
      for y in 0 .. Integer (image.height) - 1 loop
        Row_start(y);
        if image.palette /= null then
          for x in 0 .. Integer (image.width) - 1 loop
            RLE_pixel_palette;
            Output_Pixel;
          end loop;
        else
          case image.bits_per_pixel is
            when 32 =>
              for x in 0..image.width-1 loop
                RLE_pixel_32;
                Output_Pixel;
              end loop;
            when 24 =>
              for x in 0..image.width-1 loop
                RLE_pixel_24;
                Output_Pixel;
              end loop;
            when 16 =>
              for x in 0..image.width-1 loop
                RLE_pixel_16;
                Output_Pixel;
              end loop;
            when 15 =>
              for x in 0..image.width-1 loop
                RLE_pixel_15;
                Output_Pixel;
              end loop;
            when 8  =>
              for x in 0..image.width-1 loop
                RLE_pixel_8;
                Output_Pixel;
              end loop;
            when others => null;
          end case;
        end if;
        Feedback(((y+1)*100) / Integer (image.height));
      end loop;
    elsif image.palette /= null then
      Get_with_palette;
    else
      case image.bits_per_pixel is
        when 32 =>
          Get_RGBA;
        when 24 =>
          Get_RGB;
        when 16 =>
          Get_16;
        when 15 =>
          Get_15;
        when 8  =>
          Get_Gray;
        when others => null;
      end case;
    end if;
  end Load;

end GID.Decoding_TGA;
