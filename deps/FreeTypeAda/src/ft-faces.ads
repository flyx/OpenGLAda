--  part of FreeTypeAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package FT.Faces is
   pragma Preelaborate;

   --  reference-counted smart pointer
   type Face_Reference is new Ada.Finalization.Controlled with private;

   type Face_Index_Type is new Interfaces.C.long;

   type Char_Index_Type is new UInt;

   type Encoding is (None, Adobe_Custom, Adobe_Expert, Adobe_Standard,
                     Apple_Roman, Big5, GB2312, Johab, Adobe_Latin_1,
                     Old_Latin_2, SJIS, MS_Symbol, Unicode, Wansung);

   type Load_Flag is (Load_Default, Load_No_Scale, Load_No_Hinting, Load_Render,
                      Load_No_Bitmap, Load_Vertical_Layout, Load_Force_Autohint,
                      Load_Crop_Bitmap, Load_Pedantic, Load_Advance_Only,
                      Load_Ignore_Global_Advance_Width, Load_No_Recourse,
                      Load_Ignore_Transform, Load_Monochrome, Load_Linear_Design,
                      Load_SBits_Only, Load_No_Autohint, Load_Load_Colour,
                      Load_Compute_Metrics, Load_Bitmap_Metrics_Only);

   type Render_Mode is (Render_Mode_Normal, Render_Mode_Light,
                        Render_Mode_Mono, Render_Mode_LCD,
                        Render_Mode_LCD_V, Render_Mode_Max);

   procedure New_Face (Library    : Library_Reference; File_Path_Name : String;
                       Face_Index : Face_Index_Type;
                       Object : in out Face_Reference);

   function Initialized (Object : Face_Reference) return Boolean;

   --  may be called manually to remove the managed pointer and decrease the
   --  reference-count early. idempotent.
   --
   --  post-condation : Object.Initialized = False
   overriding procedure Finalize (Object : in out Face_Reference);

   function Size (Object : Face_Reference) return Bitmap_Size;
   procedure Kerning (Object      : Face_Reference; Left_Glyph : UInt;
                      Right_Glyph : UInt; Kern_Mode : UInt;
                      aKerning    : access Vector);
   function Character_Index (Object : Face_Reference; Char_Code : ULong)
                             return Char_Index_Type;
   procedure Load_Glyph (Object : Face_Reference; Glyph_Index : Char_Index_Type;
                         Flags : Load_Flag);
   procedure Load_Character (Object : Face_Reference; Char_Code : ULong;
                             Flags  : Load_Flag);
   function Metrics (Object : Face_Reference) return Size_Metrics;
   procedure Set_Pixel_Sizes (Object       : Face_Reference;
                              Pixel_Width  : UInt;
                              Pixel_Height : UInt);

   function Glyph_Slot (Object : Face_Reference) return Glyph_Slot_Reference;

   Image_Error : exception;

   Undefined_Character_Code : constant Char_Index_Type;
private
   for Face_Index_Type'Size use Interfaces.C.long'Size;
   for Char_Index_Type'Size use UInt'Size;

   procedure Check_Face_Ptr (Object : Face_Reference);

   type Face_Reference is new Ada.Finalization.Controlled with record
      Data : aliased Face_Ptr;
      --  deallocation of the library will trigger deallocation of all Face_Ptr
      --  objects. therefore, we keep a reference to the library here to make
      --  sure the library outlives the Face_Reference.
      Library : Library_Reference;
   end record;

   overriding procedure Adjust (Object : in out Face_Reference);

   --  Encoding courtesy of OpenGLAda.src.ftgl.ftgl.ads type Charset
   --  (Felix Krause <contact@flyx.org>, 2013)
   for Encoding use (None      => 0,
                     MS_Symbol => Character'Pos ('s') * 2 ** 24 +
                       Character'Pos ('y') * 2 ** 16 +
                       Character'Pos ('m') * 2 ** 8  +
                       Character'Pos ('b'),

                     Unicode   => Character'Pos ('u') * 2 ** 24 +
                       Character'Pos ('n') * 2 ** 16 +
                       Character'Pos ('i') * 2 ** 8 +
                       Character'Pos ('c'),

                     SJIS      => Character'Pos ('s') * 2 ** 24 +
                       Character'Pos ('j') * 2 ** 16 +
                       Character'Pos ('i') * 2 ** 8 +
                       Character'Pos ('s'),

                     GB2312    => Character'Pos ('g') * 2 ** 24 +
                       Character'Pos ('b') * 2 ** 16 +
                       Character'Pos (' ') * 2 ** 8 +
                       Character'Pos (' '),

                     Big5      => Character'Pos ('b') * 2 ** 24 +
                       Character'Pos ('i') * 2 ** 16 +
                       Character'Pos ('g') * 2 ** 8 +
                       Character'Pos ('5'),

                     Wansung   => Character'Pos ('w') * 2 ** 24 +
                       Character'Pos ('a') * 2 ** 16 +
                       Character'Pos ('n') * 2 ** 8 +
                       Character'Pos ('s'),

                     Johab     => Character'Pos ('j') * 2 ** 24 +
                       Character'Pos ('o') * 2 ** 16 +
                       Character'Pos ('h') * 2 ** 8 +
                       Character'Pos ('a'),

                     Adobe_Standard => Character'Pos ('A') * 2 ** 24 +
                       Character'Pos ('D') * 2 ** 16 +
                       Character'Pos ('O') * 2 ** 8 +
                       Character'Pos ('B'),

                     Adobe_Expert   => Character'Pos ('A') * 2 ** 24 +
                       Character'Pos ('D') * 2 ** 16 +
                       Character'Pos ('B') * 2 ** 8 +
                       Character'Pos ('E'),

                     Adobe_Custom   => Character'Pos ('A') * 2 ** 24 +
                       Character'Pos ('D') * 2 ** 16 +
                       Character'Pos ('B') * 2 ** 8 +
                       Character'Pos ('C'),

                     Adobe_Latin_1  => Character'Pos ('l') * 2 ** 24 +
                       Character'Pos ('a') * 2 ** 16 +
                       Character'Pos ('t') * 2 ** 8 +
                       Character'Pos ('1'),

                     Old_Latin_2    => Character'Pos ('l') * 2 ** 24 +
                       Character'Pos ('a') * 2 ** 16 +
                       Character'Pos ('t') * 2 ** 8 +
                       Character'Pos ('2'),

                     Apple_Roman    => Character'Pos ('a') * 2 ** 24 +
                       Character'Pos ('r') * 2 ** 16 +
                       Character'Pos ('m') * 2 ** 8 +
                       Character'Pos ('n'));
   for Load_Flag use
     (Load_Default                     => 16#000000#,
      Load_No_Scale                    => 16#000001#,
      Load_No_Hinting                  => 16#000002#,
      Load_Render                      => 16#000004#,
      Load_No_Bitmap                   => 16#000008#,
      Load_Vertical_Layout             => 16#000010#,
      Load_Force_Autohint              => 16#000020#,
      Load_Crop_Bitmap                 => 16#000040#,
      Load_Pedantic                    => 16#000080#,
      Load_Advance_Only                => 16#000100#,
      Load_Ignore_Global_Advance_Width => 16#000200#,
      Load_No_Recourse                 => 16#000400#,
      Load_Ignore_Transform            => 16#000800#,
      Load_Monochrome                  => 16#001000#,
      Load_Linear_Design               => 16#002000#,
      Load_SBits_Only                  => 16#0004000#,
      Load_No_Autohint                 => 16#008000#,
      Load_Load_Colour                 => 16#100000#,
      Load_Compute_Metrics             => 16#200000#,
      Load_Bitmap_Metrics_Only         => 16#400000#);
   for Load_Flag'Size use 32;

   Undefined_Character_Code : constant Char_Index_Type := 0;
end FT.Faces;
