with GID.Buffering;                     use GID.Buffering;

package body GID.Decoding_BMP is

  procedure Load (image: in out Image_descriptor) is
    b01, b, br, bg, bb: U8:= 0;
    x, x_max, y: Natural;
    --
    function Times_257(x: Primary_color_range) return Primary_color_range is
    pragma Inline(Times_257);
    begin
      return 16 * (16 * x) + x;  --  this is 257 * x, = 16#0101# * x
      --  Numbers 8-bit -> no OA warning at instanciation. Returns x if type Primary_color_range is mod 2**8.
    end Times_257;
    full_opaque: constant Primary_color_range:= Primary_color_range'Last;
    --
    procedure Pixel_with_palette is
    pragma Inline(Pixel_with_palette);
    begin
      case Primary_color_range'Modulus is
        when 256 =>
          Put_Pixel(
            Primary_color_range(image.palette(Integer(b)).red),
            Primary_color_range(image.palette(Integer(b)).green),
            Primary_color_range(image.palette(Integer(b)).blue),
            full_opaque
          );
        when 65_536 =>
          Put_Pixel(
            Times_257(Primary_color_range(image.palette(Integer(b)).red)),
            Times_257(Primary_color_range(image.palette(Integer(b)).green)),
            Times_257(Primary_color_range(image.palette(Integer(b)).blue)),
            --  Times_257 makes max intensity FF go to FFFF
            full_opaque
          );
        when others =>
          raise invalid_primary_color_range with "BMP: color range not supported";
      end case;
    end Pixel_with_palette;
    --
    pair: Boolean;
    bit: Natural range 0..7;
    --
    line_bits: constant Float:= Float(image.width * Positive_32 (image.bits_per_pixel));
    padded_line_size: constant Positive:= 4 * Integer(Float'Ceiling(line_bits / 32.0));
    unpadded_line_size: constant Positive:= Integer(Float'Ceiling(line_bits / 8.0));
    --  (in bytes)
  begin
    Attach_Stream(image.buffer, image.stream);
    y:= 0;
    while y <= Integer (image.height) - 1 loop
      x:= 0;
      x_max:= Integer (image.width) - 1;
      case image.bits_per_pixel is
        when 1 => -- B/W
          bit:= 0;
          Set_X_Y(x,y);
          while x <= x_max loop
            if bit=0 then
              Get_Byte(image.buffer, b01);
            end if;
            b:= (b01 and 16#80#) / 16#80#;
            Pixel_with_palette;
            b01:= b01 * 2; -- cannot overflow.
            if bit=7 then
              bit:= 0;
            else
              bit:= bit + 1;
            end if;
            x:= x + 1;
          end loop;
        when 4 => -- 16 colour image
          pair:= True;
          Set_X_Y(x,y);
          while x <= x_max loop
            if pair then
              Get_Byte(image.buffer, b01);
              b:= (b01 and 16#F0#) / 16#10#;
            else
              b:= (b01 and 16#0F#);
            end if;
            pair:= not pair;
            Pixel_with_palette;
            x:= x + 1;
          end loop;
        when 8 => -- 256 colour image
          Set_X_Y(x,y);
          while x <= x_max loop
            Get_Byte(image.buffer, b);
            Pixel_with_palette;
            x:= x + 1;
          end loop;
        when 24 => -- RGB, 256 colour per primary colour
          Set_X_Y(x,y);
          while x <= x_max loop
            Get_Byte(image.buffer, bb);
            Get_Byte(image.buffer, bg);
            Get_Byte(image.buffer, br);
            case Primary_color_range'Modulus is
              when 256 =>
                Put_Pixel(
                  Primary_color_range(br),
                  Primary_color_range(bg),
                  Primary_color_range(bb),
                  full_opaque
                );
              when 65_536 =>
                Put_Pixel(
                  Times_257(Primary_color_range(br)),
                  Times_257(Primary_color_range(bg)),
                  Times_257(Primary_color_range(bb)),
                  --  Times_257 makes max intensity FF go to FFFF
                  full_opaque
                );
              when others =>
                raise invalid_primary_color_range with "BMP: color range not supported";
            end case;
            x:= x + 1;
          end loop;
        when others =>
          null;
      end case;
      for i in unpadded_line_size + 1 .. padded_line_size loop
        Get_Byte(image.buffer, b);
      end loop;
      y:= y + 1;
      Feedback((y*100) / Integer (image.height));
    end loop;
  end Load;

end GID.Decoding_BMP;
