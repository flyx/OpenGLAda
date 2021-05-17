package body GL.Text.UTF8 is
   type Byte is mod 2**8;

   subtype Surrogate_Halves is UTF8_Code_Point range 16#D800# .. 16#DFFF#;

   procedure Read (Buffer : String; Position : in out Positive;
                  Result : out UTF8_Code_Point) is
      Cur : Byte := Character'Pos (Buffer (Position));
      Additional_Bytes : Positive;
   begin
      if (Cur and 2#10000000#) = 0 then
         Result := UTF8_Code_Point (Cur);
         Position := Position + 1;
         return;
      elsif (Cur and 2#01000000#) = 0 then
         raise Rendering_Error with "Encoding error at code point starting byte"
           & Position'Img;
      elsif (Cur and 2#00100000#) = 0 then
         Additional_Bytes := 1;
         Result := UTF8_Code_Point (Cur and 2#00011111#) * 2**6;
      elsif (Cur and 2#00010000#) = 0 then
         Additional_Bytes := 2;
         Result := UTF8_Code_Point (Cur and 2#00001111#) * 2**12;
      elsif (Cur and 2#00001000#) = 0 then
         Additional_Bytes := 3;
         Result := UTF8_Code_Point (Cur and 2#00000111#) * 2**18;
      else
         raise Rendering_Error with "Encoding error at code point starting byte"
           & Position'Img;
      end if;
      for Index in 1 .. Additional_Bytes loop
         Cur := Character'Pos (Buffer (Position + Index));
         if (Cur and 2#11000000#) /= 2#10000000# then
            raise Rendering_Error with
              "Encoding error at code point continuation byte" &
              Positive'Image (Position + Index);
         end if;
         Result := Result + UTF8_Code_Point (Cur and 2#00111111#) *
           2**(6 * (Additional_Bytes - Index));
      end loop;
      if Result in Surrogate_Halves then
         raise Rendering_Error with
           "Surrogate half not valid in UTF-8 at position" & Position'Img;
      end if;
      Position := Position + Additional_Bytes + 1;
   end Read;
end GL.Text.UTF8;
