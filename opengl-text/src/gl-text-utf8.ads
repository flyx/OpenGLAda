private package GL.Text.UTF8 is
   type Code_Point is mod 2**32;
   subtype UTF8_Code_Point is Code_Point range 0 .. 16#10FFFF#;

   procedure Read (Buffer : String; Position : in out Positive;
                   Result : out UTF8_Code_Point);
end GL.Text.UTF8;
