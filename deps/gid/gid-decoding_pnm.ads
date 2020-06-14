--  Portable pixmap format (PPM)
--  Portable graymap format (PGM)
--  Portable bitmap format (PBM)

private package GID.Decoding_PNM is

  --------------------
  -- Image decoding --
  --------------------

  generic
    type Primary_color_range is mod <>;
    with procedure Set_X_Y (x, y: Natural);
    with procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    );
    with procedure Feedback (percents: Natural);
  --
  procedure Load (image: in out Image_descriptor);

  function Get_Token(
    stream      : Stream_Access;
    needs_EOL   : Boolean:= False;
    single_char : Boolean:= False
  )
  return String;

  function Get_Integer(
    stream      : Stream_Access;
    needs_EOL   : Boolean:= False;
    single_char : Boolean:= False
  )
  return Integer;

  function Get_Positive_32(
    stream      : Stream_Access;
    needs_EOL   : Boolean:= False;
    single_char : Boolean:= False
  )
  return Positive_32;

end GID.Decoding_PNM;
