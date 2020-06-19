--
--  Check if an image is opaque (fully non-transparent).
--
--  Small-size demo for the GID (Generic Image Decoder) package.
--  For a larger example, look for to_bmp.adb .
--

with GID;

with Ada.Calendar;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;

with Interfaces;

procedure Is_opaque is

  procedure Blurb is
  begin
    Put_Line(Current_Error, "Is_opaque * check if an image is opaque (fully non-transparent)");
    Put_Line(Current_Error, "GID (Generic Image Decoder) package version " &
      GID.version & " dated " & GID.reference);
    Put_Line(Current_Error, "URL: " & GID.web);
    New_Line(Current_Error);
    Put_Line(Current_Error, "Syntax:");
    Put_Line(Current_Error, "is_opaque <image_1> [<image_2>...]");
    New_Line(Current_Error);
  end Blurb;

  procedure Check_raw_image(
    image     : in out GID.Image_descriptor;
    next_frame:    out Ada.Calendar.Day_Duration;
    opaque    :    out Boolean
  )
  is
    use Interfaces;
    subtype Primary_color_range is Unsigned_8;
    --
    procedure Set_X_Y (x, y: Natural) is
    begin
      null;
    end Set_X_Y;
    --
    procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    )
    is
    pragma Unreferenced (blue, green, red);
    begin
      opaque:= opaque and alpha = Primary_color_range'Last;
    end Put_Pixel;

    stars: Natural:= 0;
    procedure Feedback(percents: Natural) is
      so_far: constant Natural:= percents / 5;
    begin
      for i in stars+1..so_far loop
        Put( Current_Error, '*');
      end loop;
      stars:= so_far;
    end Feedback;

    procedure Load_image is
      new GID.Load_image_contents(
        Primary_color_range, Set_X_Y,
        Put_Pixel, Feedback, GID.fast
      );

  begin
    opaque:= True;
    Load_image(image, next_frame);
  end Check_raw_image;

  procedure Process(name: String) is
    f: Ada.Streams.Stream_IO.File_Type;
    i: GID.Image_descriptor;
    up_name: constant String:= To_Upper(name);
    --
    next_frame: Ada.Calendar.Day_Duration:= 0.0;
    opaque_frame: Boolean;
  begin
    --
    --  Load the image in its original format
    --
    Open(f, In_File, name);
    Put_Line(Current_Error, "Checking " & name & "...");
    --
    GID.Load_image_header(
      i,
      Stream(f).all,
      try_tga =>
        name'Length >= 4 and then
        up_name(up_name'Last-3..up_name'Last) = ".TGA"
    );
    if GID.Expect_transparency(i) then
      Put_Line(Current_Error, ".........v.........v");
      --
      loop
        Check_raw_image(i, next_frame, opaque_frame);
        New_Line(Current_Error);
        exit when next_frame = 0.0 or not opaque_frame;
      end loop;
      if opaque_frame then
        Put_Line(Current_Error, "  Opaque: all pixels of all frames are opaque.");
      else
        Put_Line(Current_Error, "  Not opaque: at least one pixel of one frame is not opaque.");
      end if;
    else
      Put_Line(Current_Error, "  Opaque: no transparency information.");
    end if;
    Close(f);
  end Process;

begin
  if Argument_Count=0 then
    Blurb;
    return;
  end if;
  for i in 1..Argument_Count loop
    Process(Argument(i));
  end loop;
end Is_opaque;
