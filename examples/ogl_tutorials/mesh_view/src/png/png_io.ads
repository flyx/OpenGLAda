-- $Id: png_io.ads,v 1.12 2011/11/23 21:42:25 sangwine Exp $
---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO  - Ada95 Portable Network Graphics Input/Output Package  --
--                                                                 --
--                http://png-io.sourceforge.net/                   --
--                                                                 --
--        Copyright (c) 1999-2009 Dr Stephen J. Sangwine           --
--                                S.Sangwine@IEEE.org              --
--                                                                 --
-- This software was created by Stephen J. Sangwine. He hereby     --
-- asserts his Moral Right to be identified as author of this      --
-- software.                                                       --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO is free software; you can redistribute it and/or modify  --
-- it under the terms of the GNU General Public License as         --
-- published by the Free Software Foundation; either version 3 of  --
-- the License, or (at your option) any later version.             --
--                                                                 --
-- PNG_IO is distributed in the hope that it will be useful, but   --
-- WITHOUT ANY WARRANTY; without even the implied warranty of      --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    --
-- GNU General Public License for more details.                    --
--                                                                 --
-- You should have received a copy of the GNU General Public       --
-- License along with this software (in the file gpl.txt).         --
-- If not access  http://www.gnu.org/licenses/                     --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- This package supports input/output of PNG                       --
-- (Portable Network Graphics) files.                              --
-- It is based on Version 1.2 of the PNG Specification (July 1999) --
-- which was the basis of the ISO standard for PNG:                --
--                                                                 --
-- Portable Network Graphics (PNG) Specification (Second Edition)  --
-- Information technology - Computer graphics and image processing --
-- - Portable Network Graphics (PNG): Functional specification.    --
-- ISO/IEC 15948:2003 (E)                                          --
-- (available at: http://www.w3.org/TR/2003/REC-PNG-20031110)      --
-- This document is referred to in the code as 'the ISO standard'. --
--                                                                 --
-- For other information about PNG see http://www.libpng.org/      --
---------------------------------------------------------------------
---------------------------------------------------------------------

with Ada.Streams, Ada.IO_Exceptions;
use Ada.Streams;

with ZLib; -- This means Zlib_Ada, the Ada binding to Zlib.

package PNG_IO is

   function Version return String; -- The package version, e.g. "4.5" or "4.5.1".

   ---------------------------------------------------------

   function Zlib_Version return String renames ZLib.Version;

   subtype Compression_Level is ZLib.Compression_Level; -- i.e. -1 .. 9.

   No_Compression : Compression_Level renames ZLib.No_Compression;
   Best_Speed : Compression_Level renames ZLib.Best_Speed;
   Best_Compression : Compression_Level renames ZLib.Best_Compression;
   Default_Compression : Compression_Level renames ZLib.Default_Compression;

   ---------------------------------------------------------------------

   -- To allow for multiple concurrent file reads, each open PNG file is
   -- denoted by a variable of private type PNG_File.

   type PNG_File is limited private;

   -- The Open procedure reads the entire file and stores the image
   -- information and pixel data in an internal file descriptor for
   -- access using the various interrogation functions provided below.

   procedure Open (F : in out PNG_File; Filename : in String);

   -- The Close procedure deallocates the file descriptor and all the
   -- image information therein.

   procedure Close (F : in out PNG_File);

   -----------------------------------------------------------------------------

   subtype Dimension is Positive range 1 .. (2**31) - 1; -- The image sizes.

   -- Note on image indexing: this package numbers rows and columns from the
   -- top and left of the image. Row 0 is the topmost row of the image, column 0
   -- is the leftmost column. This is conventional for graphics and is often the
   -- convention for image processing.

   subtype Coordinate is Natural range 0 .. (2**31) - 2; -- Image indices.

   -----------------------------------------------------------------------------

   function Width (F : PNG_File) return Dimension;
   function Height (F : PNG_File) return Dimension;

   -----------------------------------------------

   function Interlaced (F : PNG_File) return Boolean;

   -------------------------------------------------

   -- A PNG file can contain five types of image with various bit depths allowed
   -- for each type. Two enumerated types indicate the type of image in a file,
   -- and the number of bits per sample.

   type Depth is (One, Two, Four, Eight, Sixteen); -- The number of bits per sample.

   subtype Depth_1_2_4 is Depth range One .. Four;
   subtype Depth_8_16 is Depth range Eight .. Sixteen;

   function Bit_Depth (F : PNG_File) return Depth;
   function Sample_Depth (F : PNG_File) return Positive;

   -------------------------------------------------------------------------------

   type Colour_Type_Code is (Zero, Two, Three, Four, Six); -- The PNG Colour Type.

   function Colour_Type (F : PNG_File) return Colour_Type_Code;

   -- For convenience, the following tables indicate whether a file of a particular
   -- colour type has alpha information, and/or contains a colour image (including
   -- a palette colour image).

   Alpha  : constant array (Colour_Type_Code) of Boolean := (False, False, False, True, True);
   Colour : constant array (Colour_Type_Code) of Boolean := (False, True, True, False, True);

   -------------------------------------------------------------------------------------------

   -- Files of colour type 3 always have a palette, but a palette is optionally
   -- present in files of colour types 2 and 6. When a file of colour type 3 is
   -- read, the user may choose to read the pixel values, and allow the package
   -- to look up and provide the RGB values. If this is done, the user need not
   -- read the palette itself. However, the following functions provide access
   -- to the palette values if these are needed.

   function Palette (F : PNG_File) return Boolean; -- True if the file has a palette.
   function Palette_Size (F : PNG_File) return Positive;

   -- Palette indices run from 0 .. Palette_Size - 1;

   function Palette_R_Value
     (F     : PNG_File;
      Index : Natural)
      return Natural;
   function Palette_G_Value
     (F     : PNG_File;
      Index : Natural)
      return Natural;
   function Palette_B_Value
     (F     : PNG_File;
      Index : Natural)
      return Natural;

   -----------------------------------------------------------------------

   -- Functions to access the pixel values (including alpha values) of an
   -- open file. The high-order bits are zero-filled. Since the user can
   -- find out the actual bit depth in the PNG file, these values can be
   -- converted to a user type with fewer bits if required.

   -- Because these functions must be called for every pixel in an image
   -- (therefore many times for a given image) they are implemented for
   -- efficiency and perform almost no error checking.  It is the
   -- responsibility of the user to verify that the PNG file contains the
   -- correct type of data. Since this package is intended as a low-level
   -- foundation for a user-level package this should not be a problem.

   -- Note that, in the case of a Type 3 (palette) image, the function
   -- Pixel_Value returns the index into the palette, whereas the functions
   -- Red_Value etc return the appropriate component of the palette.

   function Pixel_Value
     (F    : PNG_File;
      R, C : Coordinate)
      return Natural;
   function Red_Value
     (F    : PNG_File;
      R, C : Coordinate)
      return Natural;
   function Green_Value
     (F    : PNG_File;
      R, C : Coordinate)
      return Natural;
   function Blue_Value
     (F    : PNG_File;
      R, C : Coordinate)
      return Natural;
   function Alpha_Value
     (F    : PNG_File;
      R, C : Coordinate)
      return Natural;

   pragma Inline (Pixel_Value, Alpha_Value, Red_Value, Green_Value, Blue_Value);

   ----------------------------------------------------------------------------

   -- Functions to provide information from the supported ancillary chunks,
   -- if present. There is a function to tell whether the chunk appeared,
   -- and functions to return the value(s) encountered if it did. Obviously
   -- Call_Error will be raised by an attempt to read the value(s) of a chunk
   -- which did not appear in the file.

   -- The following table lists the directly supported ancillary chunks.
   -- (Unsupported chunks are not analyzed, but they are read in and their
   -- CRCs are verified, and the raw data in them is accessible. This allows
   -- easy extension of PNG_IO to support additional ancillary chunks.)
   --
   -- gAMA  See the Gamma functions.
   -- cHRM  See the Chromaticity functions.
   -- sRGB  See the Standard_RGB functions.
   -- pHYs  See the Physical functions.
   -- tEXt  The number of tEXt and zTXt chunks encountered is returned
   -- zTXt  by the NText function, and the string lengths and the strings
   --       themselves may be obtained by the Text function. From the user
   --       viewpoint there is no distinction between the compressed and
   --       uncompressed chunks.
   --
   -- Unsigned 4-byte values are used in several ancillary chunk types.
   -- They are returned here as Natural.

   type Pair is record
      X, Y : Natural;
   end record;

   function Gamma (F : PNG_File) return Boolean;
   function Gamma_Value (F : PNG_File) return Natural;

   function Chromaticity (F : PNG_File) return Boolean;
   function White_Point (F : PNG_File) return Pair;
   function Red_Primary (F : PNG_File) return Pair;
   function Green_Primary (F : PNG_File) return Pair;
   function Blue_Primary (F : PNG_File) return Pair;

   type Rendering_Intent is (Perceptual, Relative_Colorimetric, Saturation, Absolute_Colorimetric);

   function Standard_RGB (F : PNG_File) return Boolean;
   function SRGB_Rendering (F : PNG_File) return Rendering_Intent;

   function Physical (F : PNG_File) return Boolean;
   function Unit_Unknown (F : PNG_File) return Boolean;
   function Unit_Meter (F : PNG_File) return Boolean;
   function Physical_Value (F : PNG_File) return Pair;

   function NTexT (F : PNG_File) return Natural; -- Zero if no text chunks.
   function Text_Keyword
     (F : PNG_File;
      N : Positive)
      return String;
   function Text_String
     (F : PNG_File;
      N : Positive)
      return String;

   ------------------------------------------------------------------------------------

   -- Access to unrecognised or unsupported ancillary chunks found in a PNG file on
   -- read. The name of, and raw data within, these chunks is not understood by PNG_IO,
   -- although it is verified that the chunks are ancillary.  All such chunks
   -- encountered in a PNG file on read are available through the following functions.
   -- They may be passed back into PNG_IO for writing to an output file but the user
   -- code should check that the chunk is safe-to-copy if it does not understand the
   -- chunk.

   type Chunk (<>) is private;

   function Ancillary_Chunk_Count (F : PNG_File) return Natural; -- There may be none.
   function Ancillary_Chunk
     (F : PNG_File;
      N : Positive)
      return Chunk;

   function Safe_to_Copy (C : Chunk) return Boolean;

   subtype Chunk_Name is String (1 .. 4);

   -- Since PNG_IO does not understand the data within the chunk, it can only be passed
   -- in 'raw' form. Prior to version 4, the data was passed as an array of Unsigned_8
   -- values, but from version 4 onwards this was changed to use Ada.Streams.Stream_Elements
   -- since this is the form in which the raw data is handled internally.

   function Name (C : Chunk) return Chunk_Name;
   function Data (C : Chunk) return Stream_Element_Array;

   -- Ancillary chunks may be written to files. The mechanism provided is very general
   -- and is designed to be readily extensible and compatible with that provided for
   -- handling chunks read from a file (see immediately above).  Each generic write
   -- procedure (see below) takes a list of chunks as a parameter. The list may include
   -- those created by functions in this package (e.g. gamma) and/or chunks created by
   -- child packages using the primitives provided here (the full type Chunk is visible
   -- to children of this package to allow extension of the chunk handling defined here)
   -- and/or chunks read from a PNG file.

   type Chunk_List is private;               -- NB The type is dynamically allocated.
   Null_Chunk_List : constant Chunk_List;

   procedure Destroy (L : in out Chunk_List); -- Deallocates the list.

   -- For notational convenience we use a concatenation operator to construct
   -- lists of chunks.  The ordering is significant and chunks are written to
   -- file in left to right order along the list (but subject to the position
   -- relative to PLTE and IDAT chunks).

   function To_Chunk_List (C : Chunk) return Chunk_List;

   function "&" (Left, Right : Chunk) return Chunk_List;
   function "&"
     (Left  : Chunk;
      Right : Chunk_List)
      return Chunk_List;
   function "&"
     (Left  : Chunk_List;
      Right : Chunk)
      return Chunk_List;
   function "&"
     (Left  : Chunk_List;
      Right : Chunk_List)
      return Chunk_List;

   type Chunk_Position is (Before_PLTE, -- and therefore before IDAT too,
   Before_IDAT, -- but after PLTE if present,
   Anywhere);   -- interpreted as after IDAT.

   -- PNG_IO has some built-in knowledge of ancillary chunks (those defined in the
   -- ISO standard, section 11.3). To allow the user to look up the position of such
   -- chunks the following two functions are provided. The Position function raises
   -- Argument_Error if called with a chunk name unknown to PNG_IO.

   function Known_Chunk (Name : Chunk_Name) return Boolean;
   function Position (Name : Chunk_Name) return Chunk_Position;

   -- Function for constructing arbitrary chunks. The chunk length is inferred from
   -- the length of the data supplied. 'Where' specifies the positioning of the
   -- chunk relative to PLTE and IDAT and is verified for chunk names known to
   -- PNG_IO.

   function Ancillary_Chunk
     (Name  : Chunk_Name; -- This must denote an ancillary chunk.
      Data  : Stream_Element_Array;
      Where : Chunk_Position)
      return Chunk;

   ---------------------------------------------------------------------
   -- Constants for the two most common gamma values, represented as
   -- (1/gamma * 1.0e5). (This is also the form in which the gamma value
   -- is represented internally in PNG files.)

   Unity_Gamma : constant := 100_000;
   Gamma_2_2   : constant := 45_455; -- Gamma of 2.2.

   ---------------------------------------------------------------------

   -- Functions for constructing the commonly used PNG ancillary chunks.

   function Gamma_Chunk (Gamma : Natural := Unity_Gamma) return Chunk;
   function Text_Chunk (Keyword, Text : String) return Chunk;

   function Chromaticity_Chunk (White_Point, Red_Primary, Green_Primary, Blue_Primary : Pair) return Chunk;
   function Physical_Chunk
     (Value : Pair;
      Metre : Boolean)
      return Chunk;

   function Standard_RGB_Chunk (R : Rendering_Intent) return Chunk;

   -- sRGB PNG files should be written with the specified chromaticity and
   -- gamma chunks as recommended in the PNG specification. To facilitate
   -- this, these chunks are returned by the following two functions.

   function Standard_RGB_Chroma return Chunk;
   function Standard_RGB_Gamma return Chunk;

   ---------------------------------------------------------------------------------

   -- Procedures for writing PNG files. There is one procedure for each colour type.
   -- The pixel values are accessed using generic function parameter(s) supplied by
   -- the user. This allows the procedures to be independent of the user's pixel
   -- representations. Instantiations of these procedures will write a particular
   -- type of PNG file, but one instantiation must be able to output files from
   -- multiple image sources. For this reason it is necessary for the generic
   -- functions which access the pixel values to have some form of image handle.
   -- Since this is a private type, the user can supply anything here that will
   -- allow the generic pixel access functions to identify the image from which
   -- the pixel is to be supplied. This could be the image array itself, or a
   -- pointer/handle or similar, including a PNG_File handle as in the functions
   -- above which implement the corresponding operation for reading PNG files. The
   -- actual supplied may, but need not be, unconstrained (a change made on 13 August
   -- 2006, thanks to Samuel Tardieu for pointing out that it was possible).
   -- Pixel/sample values are passed as discretes and the range of the type supplied
   -- must be consistent with the bit depth given when the procedure was instantiated
   -- (that is, having at least the range demanded by the bit depth). The values
   -- supplied must not exceed the largest value allowed by the bit depth. The formal
   -- type Sample was changed on 25 November 2004 from "range <>" to "(<>)", thus
   -- allowing the actual to be a modular type or a signed integer type. Of course,
   -- a consequence is that an enumerated type is also permitted, but why not? One
   -- could define (Black, White) and use it to write a 1-bit greyscale image.

   -- The Ancillary parameter was added at the end of the list of parameters for
   -- backward compatibility with code written for previous versions of PNG_IO.
   -- This means that if a value is supplied for the Ancillary parameter, a value
   -- has to be specified for Interlace, or named notation used for Ancillary.

   -- The compression parameter was added 13 August 2006, at the end of the parameters,
   -- again for backward compatibility.

   generic
      type Image_Handle (<>) is limited private;
      type Sample is (<>);
      with function Grey_Sample
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
   procedure Write_PNG_Type_0
     (Filename  : in String;
      I         : in Image_Handle;
      X, Y      : in Dimension;
      Bit_Depth : in Depth             := Eight;
      Interlace : in Boolean           := False;
      Ancillary : in Chunk_List        := Null_Chunk_List;
      Level     : in Compression_Level := Default_Compression);
   generic
      type Image_Handle (<>) is limited private;
      type Sample is (<>);
      with function Red_Sample
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
      with function Green_Sample
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
      with function Blue_Sample
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
   procedure Write_PNG_Type_2
     (Filename  : in String;
      I         : in Image_Handle;
      X, Y      : in Dimension;
      Bit_Depth : in Depth_8_16        := Eight;
      Interlace : in Boolean           := False;
      Ancillary : in Chunk_List        := Null_Chunk_List;
      Level     : in Compression_Level := Default_Compression);

      -- For colour type 3 the bit depth written to the file is inferred from
      -- the supplied palette length, the minimum possible being used. The
      -- palette length must not exceed 256.

   generic
      type Palette_Handle is limited private;
      with function Palette_Length (P : Palette_Handle) return Positive;
      type Palette_Index is range <>; -- Must run from 0 .. Palette_Length;
      type Palette_Sample is range <>;
      with function Palette_R_Value
        (P : Palette_Handle;
         I : Palette_Index)
         return Palette_Sample;
      with function Palette_G_Value
        (P : Palette_Handle;
         I : Palette_Index)
         return Palette_Sample;
      with function Palette_B_Value
        (P : Palette_Handle;
         I : Palette_Index)
         return Palette_Sample;
      type Image_Handle (<>) is limited private;
      type Sample is (<>);
      with function Pixel_Value
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
   procedure Write_PNG_Type_3
     (Filename  : in String;
      P         : in Palette_Handle;
      I         : in Image_Handle;
      X, Y      : in Dimension;
      Interlace : in Boolean           := False;
      Ancillary : in Chunk_List        := Null_Chunk_List;
      Level     : in Compression_Level := Default_Compression);
   generic
      type Image_Handle (<>) is limited private;
      type Sample is (<>);
      with function Grey_Sample
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
      with function Alpha_Sample
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
   procedure Write_PNG_Type_4
     (Filename  : in String;
      I         : in Image_Handle;
      X, Y      : in Dimension;
      Bit_Depth : in Depth_8_16        := Eight;
      Interlace : in Boolean           := False;
      Ancillary : in Chunk_List        := Null_Chunk_List;
      Level     : in Compression_Level := Default_Compression);
   generic
      type Image_Handle (<>) is limited private;
      type Sample is (<>);
      with function Red_Sample
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
      with function Green_Sample
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
      with function Blue_Sample
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
      with function Alpha_Sample
        (I    : Image_Handle;
         R, C : Coordinate)
         return Sample;
   procedure Write_PNG_Type_6
     (Filename  : in String;
      I         : in Image_Handle;
      X, Y      : in Dimension;
      Bit_Depth : in Depth_8_16        := Eight;
      Interlace : in Boolean           := False;
      Ancillary : in Chunk_List        := Null_Chunk_List;
      Level     : in Compression_Level := Default_Compression);

      -------------------------------------------------------------------------

      -- The package Ada.Exceptions may be used to access diagnostic
      -- messages for some occurrences of these exceptions.

   Call_Error : exception;  -- Indicates incorrect use of the
   -- procedures/functions in the package,
   -- for example, by attempting to read
   -- the width of a PNG image before the
   -- PNG file has been opened.
   Argument_Error : exception;  -- Indicates an illegal parameter value
   -- supplied to a PNG_IO procedure/function.
   Signature_Error : exception;  -- Indicates (on open) that the PNG
   -- file had an incorrect signature
   -- (first eight bytes) and may not be
   -- a PNG file at all.
   Format_Error : exception; -- Indicates (on open) that the file
   -- contained incorrect data (e.g. in
   -- a chunk length or type field.
   CRC_Error : exception; -- Indicates (on open) an error in
   -- a cyclic redundancy check (CRC).
   Unhandled_Format : exception; -- Indicates (on open) that the PNG
   -- file contained data not handled by
   -- the current implementation of this
   -- package.

   Zlib_Error        : exception renames ZLib.ZLib_Error;
   Zlib_Status_Error : exception renames ZLib.Status_Error;

   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
   Mode_Error   : exception renames Ada.IO_Exceptions.Mode_Error;
   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;
   End_Error    : exception renames Ada.IO_Exceptions.End_Error;
   Data_Error   : exception renames Ada.IO_Exceptions.Data_Error;

private

   type PNG_File_Descriptor;
   type PNG_File is access PNG_File_Descriptor;

   ------------------------------------------------------------------------

   -- By placing the full declaration for Chunk_Descriptor here, it is
   -- visible to children of PNG_IO. Thus PNG_IO can be extended to
   -- handle other ancillary chunk types (including user defined chunks
   -- as well as less-often used standard chunks) by adding child packages.

   type Chunk_List_Element;
   type Chunk_List is access Chunk_List_Element;
   Null_Chunk_List : constant Chunk_List := null;

   ----------------------------------------------

   type Chunk (Size : Stream_Element_Count) is record
      Name  : Chunk_Name;
      Data  : Stream_Element_Array (1 .. Size);
      Where : Chunk_Position := Anywhere;
   end record;

   type Chunk_List_Element (Size : Stream_Element_Count) is record
      Chnk : Chunk (Size);
      Link : Chunk_List;
   end record;

   -- We define the encodings for the sRGB rendering intent so that no
   -- explicit conversion is necessary between the binary representation in
   -- a PNG file, and the internal representation used in this package.

   for Rendering_Intent use (Perceptual => 0, Relative_Colorimetric => 1, Saturation => 2, Absolute_Colorimetric => 3);
end PNG_IO;
