--
--  Minimal steganography tool.
--
--  This demo is derived from mini.adb.
--

--  To do:
--  - encryption

with GID;

with Ada.Calendar;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;

procedure Steg is

  procedure Blurb is
  begin
    Put_Line(Standard_Error, "Steg * Minimal steganography tool");
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "  Encoding: converts any image file to a PPM image file, with");
    Put_Line(Standard_Error, "            inclusion of a hidden data file. The PPM image can then");
    Put_Line(Standard_Error, "            be converted to a lossless-compressed format like PNG.");
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "  Decoding: extracts a hidden file from an image.");
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "GID version " & GID.version & " dated " & GID.reference);
    Put_Line(Standard_Error, "URL: " & GID.web);
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "Syntax:");
    Put_Line(Standard_Error, " steg [e|d] <image> <file>");
    New_Line(Standard_Error);
  end Blurb;

  use Interfaces;

  type Byte_Array is array(Integer range <>) of Unsigned_8;
  type p_Byte_Array is access Byte_Array;
  procedure Dispose is new Ada.Unchecked_Deallocation(Byte_Array, p_Byte_Array);

  img_buf: p_Byte_Array:= null;

  --  Load image into a 24-bit truecolor RGB raw bitmap (for a PPM output)
  procedure Load_raw_image(
    image : in out GID.Image_descriptor;
    buffer: in out p_Byte_Array;
    next_frame: out Ada.Calendar.Day_Duration
  )
  is
    subtype Primary_color_range is Unsigned_8;
    image_width : constant Positive:= GID.Pixel_width(image);
    image_height: constant Positive:= GID.Pixel_height(image);
    idx: Natural;
    --
    procedure Set_X_Y (x, y: Natural) is
    begin
      idx:= 3 * (x + image_width * (image_height - 1 - y));
    end Set_X_Y;
    --
    procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    )
    is
    pragma Warnings(off, alpha); -- alpha is just ignored
    begin
      buffer(idx..idx+2):= (red, green, blue);
      idx:= idx + 3;
      --  ^ GID requires us to look to next pixel on the right for next time.
    end Put_Pixel;

    stars: Natural:= 0;
    procedure Feedback(percents: Natural) is
      so_far: constant Natural:= percents / 5;
    begin
      for i in stars+1..so_far loop
        Put( Standard_Error, '*');
      end loop;
      stars:= so_far;
    end Feedback;

    procedure Load_image is
      new GID.Load_image_contents(
        Primary_color_range, Set_X_Y,
        Put_Pixel, Feedback, GID.fast
      );

  begin
    Dispose(buffer);
    buffer:= new Byte_Array(0..3 * image_width * image_height - 1);
    Load_image(image, next_frame);
  end Load_raw_image;

  procedure Dump_PPM(name: String; i: GID.Image_descriptor) is
    f: Ada.Streams.Stream_IO.File_Type;
    ppm_name: constant String:= name & ".ppm";
  begin
    Create(f, Out_File, ppm_name);
    Put_Line(Standard_Error, "Creating PPM image, name = " & ppm_name & " ...");
    --  PPM Header:
    String'Write(
      Stream(f),
      "P6 " &
      Integer'Image(GID.Pixel_width(i)) &
      Integer'Image(GID.Pixel_height(i)) & " 255" & ASCII.LF
    );
    --  PPM raw BGR image:
    Byte_Array'Write(Stream(f), img_buf.all);
    --  ^ slow on some Ada systems, see to_bmp to have a faster version
    Close(f);
  end Dump_PPM;

  type Operation is (encoding, decoding);

  Data_too_large: exception;

  procedure Process(image_name, data_name: String; op: Operation) is
    f_im, f_dt: Ada.Streams.Stream_IO.File_Type;
    --
    procedure Encode is
      idx: Natural:= img_buf'Last;
      --  Start with buffer's end (image's bottom), with the hope it is "noisier":
      --  often there is a blue sky or something like that on the top...
      procedure Encode_byte(b: Unsigned_8) is
      begin
        img_buf(idx):= (img_buf(idx) and 2#1111_1100#) or (b and 2#0000_0011#);                --  B
        idx:= idx - 1;
        img_buf(idx):= (img_buf(idx) and 2#1111_1000#) or Shift_Right(b and 2#0001_1100#, 2);  --  G
        idx:= idx - 1;
        img_buf(idx):= (img_buf(idx) and 2#1111_1000#) or Shift_Right(b, 5);                   --  R
        idx:= idx - 1;
      end Encode_byte;
      b: Unsigned_8;
      data_size: Unsigned_64;
      needed_size: Unsigned_64;
      available_size: constant Unsigned_64:= img_buf'Length / 3;  --  1 byte per pixel;
      factor: Float;
    begin
      Open(f_dt, In_File, data_name);
      data_size:= Unsigned_64(Size(f_dt));
      needed_size:= data_size + 8;
      factor:= Float(needed_size) / Float(available_size);
      if needed_size > available_size then
        raise Data_too_large with
          "Needs a" & Integer'Image(1 + Integer(100.0 * factor)) &
          "% raw size scaling, i.e. a" &
          Integer'Image(1 + Integer(100.0 * Sqrt(factor))) &
          "% image scaling in both dimensions";
      end if;
      Put_Line(Standard_Error,
        "Data size:" & Unsigned_64'Image(data_size) &
        ", using" & Integer'Image(Integer(100.0 * factor)) &
        "% of image data"
      );
      for i in 1..8 loop
        Encode_byte(Unsigned_8(data_size and 16#FF#));
        data_size:= Shift_Right(data_size, 8);
      end loop;
      while not End_Of_File(f_dt) loop
        Unsigned_8'Read(Stream(f_dt), b);
        Encode_byte(b);
      end loop;
      Close(f_dt);
    end Encode;
    --
    procedure Decode is
      idx: Natural:= img_buf'Last;
      procedure Decode_byte(b: out Unsigned_8) is
      begin
        b:= img_buf(idx) and 2#0000_0011#;                     --  B
        idx:= idx - 1;
        b:= b + Shift_Left(img_buf(idx) and 2#0000_0111#, 2);  --  G
        idx:= idx - 1;
        b:= b + Shift_Left(img_buf(idx) and 2#0000_0111#, 5);  --  R
        idx:= idx - 1;
      end Decode_byte;
      b: Unsigned_8;
      data_size: Unsigned_64:= 0;
    begin
      for i in 0..7 loop
        Decode_byte(b);
        data_size:= data_size + Shift_Left(Unsigned_64(b), i * 8);
      end loop;
      Create(f_dt, Out_File, data_name);
      for i in 1..data_size loop
        Decode_byte(b);
        Unsigned_8'Write(Stream(f_dt), b);
      end loop;
      Close(f_dt);
    end Decode;
    --
    i: GID.Image_descriptor;
    up_name: constant String:= To_Upper(image_name);
    --
    next_frame: Ada.Calendar.Day_Duration;
  begin
    --
    --  Load the image in its original format
    --
    Open(f_im, In_File, image_name);
    Put_Line(Standard_Error, "Processing " & image_name & "...");
    --
    GID.Load_image_header(
      i,
      Stream(f_im).all,
      try_tga =>
        image_name'Length >= 4 and then
        up_name(up_name'Last-3..up_name'Last) = ".TGA"
    );
    Put_Line(Standard_Error, ".........v.........v");
    --
    Load_raw_image(i, img_buf, next_frame);
    New_Line(Standard_Error);
    Close(f_im);
    case op is
      when encoding =>
        Encode;
        Dump_PPM(image_name, i);  --  Output encoded image
      when decoding =>
        Decode;
    end case;
  end Process;

  op: Operation;

begin
  if Argument_Count /= 3 then
    Blurb;
    return;
  end if;
  if To_Lower(Argument(1))="e" then
    op:= encoding;
  elsif To_Lower(Argument(1))="d" then
    op:= decoding;
  else
    Blurb;
    return;
  end if;
  Process(Argument(2), Argument(3), op);
end Steg;
