--  part of FreeTypeAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Finalization;
with Interfaces.C;
with System;

private with Interfaces.C.Strings;

package FT is
   pragma Preelaborate;

   --  reference-counted smart pointer
   type Library_Reference is new Ada.Finalization.Controlled with private;

   subtype Fixed is Interfaces.C.long;
   subtype Short is Interfaces.C.short;
   subtype UShort is Interfaces.C.unsigned_short;
   subtype Int is Interfaces.C.int;
   subtype UInt is Interfaces.C.unsigned;
   subtype Long is Interfaces.C.long;
   subtype ULong is Interfaces.C.unsigned_long;
   subtype Position is Interfaces.C.long;
   subtype Pixel_Distance is Interfaces.C.short range
     0 .. Interfaces.C.short'Last;
   subtype Pixels_Per_EM is Interfaces.C.unsigned_short;

   FreeType_Exception : exception;

   --  instantiates a new library object and makes the given reference point to
   --  that object.
   procedure Init (Object : in out Library_Reference);

   --  true iff the reference points to a valid library object (i.e. has been
   --  initialized).
   function Initialized (Object : Library_Reference) return Boolean;

   --  you may call this manually if you want to make the given reference
   --  uninitialized. the actual object it pointed to will only be deallocated
   --  if the reference count reaches zero. idempotent.
   --
   --  post-condition : Object.Initialized = False
   overriding procedure Finalize (Object : in out Library_Reference);


   type Bounding_Box is record
      X_Min, Y_Min, X_Max, Y_Max : Position;
   end record;
   pragma Convention (C_Pass_By_Copy, Bounding_Box);

   type Vector is record
      X, Y : Position;
   end record;
   pragma Convention (C_Pass_By_Copy, Vector);

   type Glyph_Format is (Format_None, Bitmap_Format, Composite_Format,
                         Outline_Format, Plotter_Format);


   type Glyph_Metrics is record
      Width, Height : Position;
      Horiz_Bearing_X, Horiz_Bearing_Y, Horiz_Advance : Position;
      Vert_Bearing_X, Vert_Bearing_Y, Vert_Advance : Position;
   end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Metrics);

   type Bitmap_Size is record
      Height : Pixel_Distance;
      Width  : Pixel_Distance;
      Size   : Position;
      X_Ppem : Position;
      Y_Ppem : Position;
   end record;
   pragma Convention (C_Pass_By_Copy, Bitmap_Size);

   type Size_Metrics is record
      X_Ppem      : Pixels_Per_EM;
      Y_Ppem      : Pixels_Per_EM;
      X_Scale     : Fixed;
      Y_Scale     : Fixed;
      Ascender    : Position;
      Descender   : Position;
      Height      : Position;
      Max_Advance : Position;
   end record;
   pragma Convention (C_Pass_By_Copy, Size_Metrics);

   type Palette_Type is private;

   type Bitmap_Record is record
      Rows         : Interfaces.C.unsigned;
      Width        : Interfaces.C.unsigned;
      Pitch        : Interfaces.C.int;
      Buffer       : System.Address;
      Num_Grays    : Interfaces.C.short;
      Pixel_Mode   : Interfaces.C.unsigned_char;
      Palette_Mode : Interfaces.C.unsigned_char;
      Palette      : Palette_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Bitmap_Record);

   --  this type is designed not to be store anywhere besides local variables.
   --  it is basically a pointer into a Face_Reference and therefore must not
   --  outlive that reference.
   --
   --  this type is not reference-counted since it cannot be copied and cannot
   --  outlive the lifespan of the parent Face_Reference.
   type Glyph_Slot_Reference (<>) is limited private;
private
   subtype Library_Ptr is System.Address;

   type Palette_Type is new System.Address;

   type Library_Reference is new Ada.Finalization.Controlled with record
      Data : aliased Library_Ptr := System.Null_Address;
   end record;

   overriding procedure Adjust (Object : in out Library_Reference);


   -----------------------------------------------------------------------------
   --  the following types are declared here so that it is visible in FT.API
   --  without being public.

   type Generic_Finalizer is access procedure (theFinalizer : System.Address);
   pragma Convention (C, Generic_Finalizer);

   type Generic_Record is record
      Data      : System.Address;
      Finalizer : Generic_Finalizer;
   end record;
   pragma Convention (C_Pass_By_Copy, Generic_Record);

   type Size_Record;
   type Size_Ptr is access Size_Record;

   subtype Character_Map_Ptr is System.Address;
   subtype Driver_Ptr is System.Address;
   subtype Face_Internal_Ptr is System.Address;
   subtype Memory_Ptr is System.Address;
   subtype Size_Internal_Ptr is System.Address;
   subtype Slot_Internal_Ptr is System.Address;
   subtype Subglyph_Ptr is System.Address;
   subtype List_Node is System.Address;
   subtype Stream_Ptr is System.Address;

   type Face_Record;
   type Face_Ptr is access Face_Record;

   type Glyph_Slot_Record;

   type Glyph_Slot_Ptr is access Glyph_Slot_Record;
   pragma Convention (C, Glyph_Slot_Ptr);

   type Glyph_Slot_Reference is limited record
      Data : not null Glyph_Slot_Ptr;
   end record;

   type Outline_Record is record
      Num_Contours : Interfaces.C.short;
      Num_Points   : Interfaces.C.short;
      Points       : access Vector;
      Tags         : Interfaces.C.Strings.chars_ptr;
      Contours     : access Interfaces.C.short;
      Flags        : Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Outline_Record);

   for Glyph_Format use (Format_None        => 0000000000,
                         Bitmap_Format      => 1651078259,
                         Composite_Format   => 1668246896,
                         Outline_Format     => 1869968492,
                         Plotter_Format     => 1886154612);
   for Glyph_Format'Size use Interfaces.C.unsigned'Size;

   type Glyph_Slot_Record is record
      Library              : Library_Ptr;
      Face                 : Face_Ptr;
      Next                 : Glyph_Slot_Ptr;
      Reserved             : Interfaces.C.unsigned;
      C_Generic            : Generic_Record;
      Metrics              : Glyph_Metrics;
      Linear_Horiz_Advance : Fixed;
      Linear_Vert_Advance  : Fixed;
      Advance              : Vector;
      Format               : Glyph_Format;
      Bitmap               : Bitmap_Record;
      Bitmap_Left          : Interfaces.C.int;
      Bitmap_Top           : Interfaces.C.int;
      Outline              : Outline_Record;
      Num_Subglyphs        : Interfaces.C.unsigned;
      Subglyphs            : Subglyph_Ptr;
      Control_Data         : System.Address;
      Control_Length       : Interfaces.C.long;
      Lsb_Delta            : Position;
      Rsb_Delta            : Position;
      Other                : System.Address;
      Internal             : Slot_Internal_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Slot_Record);

   type List_Record is record
      head : List_Node;
      tail : List_Node;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Record);

   type Face_Record is record
      Num_Faces               : Long;
      --  Face_Index holds two different values.
      --  Bits 0-15 are the index of the face in the font file (starting with ~0)
      --  and are set to ~0 if there is only one face in the font file.
      Face_Index              : Long;
      Face_Flags              : Long;
      Style_Flags             : Long;
      Num_Glyphs              : Long;
      Family_Name             : Interfaces.C.Strings.chars_ptr;
      Style_Name              : Interfaces.C.Strings.chars_ptr;
      --  Num_Fixed_Sizes is the number of bitmap strikes in the face.
      --  Even if the face is scalable, there might still be bitmap strikes,
      --  which are called `sbits' in that case.

      Num_Fixed_sizes         : Int;
      --  Available_Sizes is an array of Bitmap_Size records for all bitmap
      --  strikes in the face.  It is NULL if there is no bitmap strike.
      Available_Sizes         : access Bitmap_Size;
      Num_Charmaps            : Int;
      Character_Map_List      : System.Address;
      C_Generic               : Generic_Record;
      --  The following member variables (down to `underline_thickness')
      --  are only relevant to scalable outlines.

      --  Bounding_Box coordinates are expressed in font units.
      --  The box is large enough to contain any glyph from the font.
      --  Thus, bbox.yMax can be seen as the maximum  ascender' and
      --  bbox.yMin as the `minimum descender.
      --   Bbox is only relevant for scalable   formats.

      Bbox                    : Bounding_Box;
      --  Units_per_EM is the number of font units per EM square for  this face.
      --  This is typically 2048 for TrueType fonts and 1000 for Type~1 fonts.
      --  Units_per_EM is only relevant for scalable formats.

      Units_Per_EM            : UShort;
      --  Ascender and descender are the typographic ascender  and descender of
      --  the face expressed in font units.
      --  For font formats not having this information, they are set to
      --  bbox.yMax and bbox.yMin.
      --  Ascender is only relevant for scalable formats.

      Ascender                : Short;
      Descender               : Short;
      --  Height is the vertical distance   between two consecutive baselines,
      --  expressed in font units and is always positive.
      --  Height is only relevant for scalable formats.
      --  For the global glyph height use  ascender - descender.

      Height                  : Short;
      --  Max_Advance_Width and Max_Advance_Height are the maximum and advance
      --  width in font units for all glyphs in this face.
      --  They are only relevant for scalable formats.
      --  They can be used to make word wrapping computations faster.

      Max_Advance_Width       : Short;
      Max_Advance_Height      : Short;
      Underline_Position      : Short;
      Underline_Thickness     : Short;
      Glyph_Slot              : Glyph_Slot_Ptr;
      --  Size is the current active size for this face.
      Size                    : Size_Ptr;          -- Ptr to a Size_Record
      Character_Map           : Character_Map_Ptr;
      Driver                  : Driver_Ptr;
      Memory                  : Memory_Ptr;
      Stream                  : Stream_Ptr;
      Sizes_List              : List_Record;
      Autohint                : Generic_Record;
      Extensions              : System.Address := System.Null_Address;
      Internal                : Face_Internal_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Face_Record);

   type Size_Record is record
      Face       : Face_Record;
      C_Generic  : Generic_Record;
      Metrics    : Size_Metrics;
      Internal   : Size_Internal_Ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Size_Record);

   type Glyph_Record is record
      Library : FT.Library_Ptr;
      Clazz   : System.Address;
      Format  : Glyph_Format;
      Advance : Vector;
   end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Record);

   type Glyph_Ptr is access Glyph_Record;
   pragma Convention (C, Glyph_Ptr);
end FT;
