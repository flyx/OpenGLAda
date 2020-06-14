--  Recurve  -  recover curves from a chart (in JPEG, PNG, or other image format)
--
--  Currently supports only charts on a white background
--
--  By David Malinge and Gautier de Montmollin
--
--  Started 28-Jun-2016

with GID;

with Ada.Calendar;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;

procedure Recurve is

  --  Parameters

  thres_grid           : constant := 0.925;      --  avg intensity below thres_grid => grid line
  thres_curve          : constant := 0.8;        --  intensity below thres_curve => curve
  thres_simil_2        : constant := 0.16 ** 2;  --  similarity within curve
  thres_simil_start_2  : constant := 0.40 ** 2;  --  similarity when scanning for curves
  radius               : constant := 0.08;       --  in proportion of image width
  full_disc_radius     : constant := 0.003;
  full_disc_radius_pix : constant := 3;
  interval_verticals   : constant := 15;
  start_verticals      : constant := 0;          --  > 0 for more vertical initial scans

  sep: constant Character:= ';';

  procedure Blurb is
  begin
    Put_Line(Standard_Error, "Recurve * Recover from a chart in any image format");
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "GID version " & GID.version & " dated " & GID.reference);
    Put_Line(Standard_Error, "URL: " & GID.web);
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "Syntax:");
    Put_Line(Standard_Error, " recurve <image>");
    New_Line(Standard_Error);
    Put_Line(Standard_Error, "Output:");
    Put_Line(Standard_Error, " <image>.csv");
    New_Line(Standard_Error);
  end Blurb;

  use Interfaces;

  subtype Primary_color_range is Unsigned_8;

  subtype Real is Long_Float;

  type RGB is record
    r,g,b: Real;
  end record;

  function Grey(c: RGB) return Real is
  begin
    return (c.r + c.g + c.b) / 3.0;
  end Grey;

  function Dist2(c1,c2: RGB) return Real is
  begin
    return
      (c1.r - c2.r) ** 2 +
      (c1.g - c2.g) ** 2 +
      (c1.b - c2.b) ** 2;
  end Dist2;

  function Img(c: RGB) return String is
  begin
    return "  R:" & Integer'Image(Integer(c.r * 255.0)) &
           "  G:" & Integer'Image(Integer(c.g * 255.0)) &
           "  B:" & Integer'Image(Integer(c.b * 255.0));
  end Img;

  --  Bidimensional array. Slower than unidimensional, but fits our purpose.
  type Bitmap is array(Integer range <>, Integer range <>) of RGB;
  type p_Bitmap is access Bitmap;
  procedure Dispose is new Ada.Unchecked_Deallocation(Bitmap, p_Bitmap);

  --  Load image
  procedure Load_raw_image(
    image : in out GID.Image_descriptor;
    bmp   : in out p_Bitmap;
    next_frame: out Ada.Calendar.Day_Duration
  )
  is
    image_width : constant Positive:= GID.Pixel_width(image);
    image_height: constant Positive:= GID.Pixel_height(image);
    pos_x, pos_y: Natural;
    --
    --  Generic parameters to feed GID.Load_image_contents with.
    --
    procedure Set_X_Y (x, y: Natural) is
    begin
      pos_x:= x;
      pos_y:= y;
    end Set_X_Y;
    --
    procedure Put_Pixel (
      red, green, blue : Primary_color_range;
      alpha            : Primary_color_range
    )
    is
    pragma Warnings(off, alpha); -- alpha is just ignored
    begin
      bmp(pos_x, bmp'Last(2) - pos_y):=
        (Real(red)   / 255.0,
         Real(green) / 255.0,
         Real(blue)  / 255.0
        );
      pos_x:= pos_x + 1;
      --  ^ GID requires us to look to next pixel on the right for next time.
    end Put_Pixel;
    --
    stars: Natural:= 0;
    procedure Feedback(percents: Natural) is
      so_far: constant Natural:= percents / 5;
    begin
      for i in stars+1..so_far loop
        Put( Standard_Error, '*');
      end loop;
      stars:= so_far;
    end Feedback;
    --
    --  Instantiation of GID.Load_image_contents.
    --
    procedure Load_image is
      new GID.Load_image_contents(
        Primary_color_range, Set_X_Y,
        Put_Pixel, Feedback, GID.fast
      );
    --
  begin
    Dispose(bmp);
    bmp:= new Bitmap(0..image_width-1, 0..image_height-1);
    Load_image(image, next_frame);
  end Load_raw_image;

  bmp: p_Bitmap:= null;

  --------------------------------------------------------------------------------
  --  Identify curves in an image file; write a .csv file with the data points  --
  --------------------------------------------------------------------------------

  procedure Detect_curves(file_name: String) is
    grid_hor: array(bmp'Range(2)) of Boolean:= (others => False);
    grid_ver: array(bmp'Range(1)) of Boolean:= (others => False);
    v: Real;
    done: array(bmp'Range(1), bmp'Range(2)) of Boolean:= (others => (others => False));
    --  color_scanned: array(0..255, 0..255, 0..255) of Boolean:= ... mmmh too big

    type Curve_ys is array(bmp'Range(1)) of Real;
    type Curve_descr is record
      ys: Curve_ys:= (others => -1.0);  --  Convention: undefined y-value is < 0
      min_x: Integer:= Integer'Last;
      max_x: Integer:= Integer'First;
      color: RGB;
    end record;

    procedure Interpolate(c: in out Curve_descr) is
      --  We will interpolate between (x1, c.ys(x1)) and (x2, c.ys(x2)).
      --
      --  y1  none  [...] y2
      --
      --  x1  x1+1  [...] x2
      y1, y2: Real;
    begin
      for x1 in c.min_x .. c.max_x loop
        y1:= c.ys(x1);
        if y1 >= 0.0 and then x1+1 <= c.max_x and then c.ys(x1+1) < 0.0 then
          for x2 in x1+2 .. c.max_x loop
            y2:= c.ys(x2);
            if y2 >= 0.0 then
              --  Linear interpolation is happening here.
              for x in x1+1 .. x2-1 loop
                c.ys(x):= (Real(x-x1) / Real(x2-x1)) * (y2-y1) + y1;
              end loop;
              exit;
            end if;
          end loop;
        end if;
      end loop;
    end Interpolate;

    Curve_Stack: array(1..bmp'Length(2)) of Curve_descr;
    curve_top: Natural:= 0;

    procedure Scan_curve(x0, y0, xd: Integer) is
      curv: Curve_descr renames Curve_Stack(curve_top);
      c: RGB renames curv.color;
      --
      procedure Mark_point(x, y: Integer) is
      begin
        done(x,y):= True;
        curv.min_x:= Integer'Min(curv.min_x, x);
        curv.max_x:= Integer'Max(curv.max_x, x);
      end Mark_point;
      --
      x_sum, y_sum: Natural;
      found: Natural;
      procedure Test_point(xt, yt: Integer) is
      begin
        if xt in bmp'Range(1)
          and then yt in bmp'Range(2)
          and then (not done(xt, yt))
          and then Dist2(bmp(xt,yt), c) < thres_simil_2
        then
          x_sum:= x_sum + xt;
          y_sum:= y_sum + yt;
          Mark_point(xt, yt);
          found:= found + 1;
        end if;
      end Test_point;
      --
      x: Integer:= x0;
      y: Integer:= y0;
      --
      procedure Check_single_radius(r: Positive) is
      begin
        for xs in 1..r loop
          for ys in 0..r loop
            if xs**2 + ys**2 in (r-1)**2 .. r**2 then
              Test_point(
                x + xs * xd,  --  xd = direction, left or right
                y - ys        --  Below
              );
              Test_point(
                x + xs * xd,  --  xd = direction, left or right
                y + ys        --  Above
              );
            end if;
          end loop;
        end loop;
      end Check_single_radius;
      --
      ring_rad: constant Integer:= Integer(radius*Real(bmp'Length(1)));
      disc_rad: constant Integer:=
        Integer'Max (
          full_disc_radius_pix,
          Integer (full_disc_radius * Real(bmp'Length(1)))
        );
      y_subpixel : Real := Real (y);
    begin
      Mark_point (x,y);
      Scan: loop
        --  We register (x, y) into the curve information.
        --  It is either the starting point, or the average
        --  matching point of previous iteration.
        curv.ys (x):= Real(bmp'Last(2)) - y_subpixel;
        --  Now, try to find the next point of the curve in the direction xd.
        found := 0;
        x_sum := 0;
        y_sum := 0;
        --  Explore a half-disc
        for rad in 1 .. disc_rad loop
          Check_single_radius (rad);
        end loop;
        if found = 0 then
          --  Continue searching, but stop when one half-ring is successful
          for rad in disc_rad+1 .. ring_rad loop
            Check_single_radius (rad);
            exit when found > 0;
          end loop;
        end if;
        exit Scan when found = 0;  --  No matching point anywhere in search half-disc.
        --  Next (x,y) point will be the average of near matching points found
        x := x_sum / found;
        y := y_sum / found;
        y_subpixel := Real (y_sum) / Real (found);  --  Non-rounded average y value.
        --  At this point, we are ready to scan next pixel (x, y) of the curve.
        exit Scan when x not in bmp'Range(1);
      end loop Scan;
    end Scan_curve;

    x0: Integer;
    color0: RGB;
    f: File_Type;
    min_min_x: Integer:= Integer'Last;
    max_max_x: Integer:= Integer'First;
    mid: constant Integer:= bmp'Last(1) / 2;
  begin
    New_Line;
    --
    --  Detect vertical gridlines - and some noise...
    --
    for x in bmp'Range(1) loop
      v:= 0.0;
      for y in bmp'Range(2) loop
        v:= v + Grey(bmp(x,y));
      end loop;
      v:= v / Real(bmp'Length(2));
      if v < thres_grid then
        grid_ver(x):= True;
        Put_Line("Vertical: " & Integer'Image(x));
      end if;
    end loop;
    --
    --  Detect horizontal gridlines - and some noise...
    --
    for y in bmp'Range(2) loop
      v:= 0.0;
      for x in bmp'Range(1) loop
        v:= v + Grey(bmp(x,y));
      end loop;
      v:= v / Real(bmp'Length(1));
      if v < thres_grid then
        grid_hor(y):= True;
        Put_Line("Horizontal: " & Integer'Image(y));
      end if;
    end loop;
    --
    --  Main scan for curves, start in a band in the middle
    --  Why not just a single vertical line ?
    --  A curve could be hidden by another one just in that place.
    --
    for sv in -start_verticals/2 .. start_verticals/2 loop
      x0:= mid + sv * interval_verticals;
      if x0 in bmp'Range(1) and then not grid_ver(x0) then
        for y in bmp'Range(2) loop
          color0:= bmp(x0,y);
          if (not grid_hor(y)) and then Grey(color0) < thres_curve and then not done(x0,y) then
            if y > 0 and then done(x0,y-1) and then Dist2(bmp(x0,y-1), color0) < thres_simil_start_2 then
              done(x0,y):= True;  --  Actually the same, fat curve as one pixel above
            --  elsif x0 > 0 and then done(x0-1,y) and then Dist2(bmp(x0-1,y), color0) < thres_simil_start_2 then
            --  done(x0,y):= True;  --  Actually the same curve as one pixel left
            else
              Put_Line("curve: " & Integer'Image(x0) & Integer'Image(y));
              curve_top:= curve_top + 1;
              Curve_Stack(curve_top).color:= color0;
              --  Following idea is from a humanitarian star who used to send
              --  two camera teams in opposite directions in conflict areas:
              Scan_curve(x0, y, -1);
              Scan_curve(x0, y, +1);
            end if;
          end if;
        end loop;
      end if;
    end loop;
    --
    --  Finalization
    --
    for i in 1..curve_top loop
      min_min_x:= Integer'Min(min_min_x, Curve_Stack(i).min_x);
      max_max_x:= Integer'Max(max_max_x, Curve_Stack(i).max_x);
      Interpolate(Curve_Stack(i));
    end loop;
    --
    --  Output curves
    --
    Create(f, Out_File, file_name & ".csv");
    Put_Line(f, "Recurve output");
    Put(f, "Color");
    for i in 1..curve_top loop
      Put(f, sep & Img(Curve_Stack(i).color));
    end loop;
    New_Line(f);
    Put(f, 'x');
    for i in 1..curve_top loop
      Put(f, sep & 'y' & Integer'Image(i));
    end loop;
    New_Line(f);
    for x in min_min_x .. max_max_x loop
      Put(f, Integer'Image(x));
      for i in 1..curve_top loop
        Put(f, sep);
        if x in Curve_Stack(i).min_x .. Curve_Stack(i).max_x and then Curve_Stack(i).ys(x) >= 0.0 then
          Put(f, Real'Image(Curve_Stack(i).ys(x)));
        end if;
      end loop;
      New_Line(f);
    end loop;
    Close(f);
  end Detect_curves;

  procedure Process(file_name: String) is
    use Ada.Streams.Stream_IO;
    f: Ada.Streams.Stream_IO.File_Type;
    i: GID.Image_descriptor;
    up_name: constant String:= To_Upper(file_name);
    --
    next_frame: Ada.Calendar.Day_Duration:= 0.0;
  begin
    --
    --  Load the image in its original format
    --
    Open(f, In_File, file_name);
    Put_Line(Standard_Error, "Processing " & file_name & "...");
    --
    GID.Load_image_header(
      i,
      Stream(f).all,
      try_tga =>
        file_name'Length >= 4 and then
        up_name(up_name'Last-3..up_name'Last) = ".TGA"
    );
    Put_Line(Standard_Error, ".........v.........v");
    --
    Load_raw_image(i, bmp, next_frame);
    Detect_curves(file_name);
    New_Line(Standard_Error);
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
end Recurve;
