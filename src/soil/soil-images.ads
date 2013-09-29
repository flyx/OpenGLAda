--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Ada.Finalization;

private with System;

package SOIL.Images is
   -- reference counted
   type Image is new Ada.Finalization.Controlled with private;

   overriding
   procedure Initialize (Object : in out Image);

   overriding
   procedure Adjust (Object : in out Image);

   overriding
   procedure Finalize (Object : in out Image);

   procedure Load (Object : in out Image; File_Name : String);
   -- force the image to be loaded in a certain format:
   procedure Load (Object : in out Image; File_Name : String;
                   Force_Format    : Explicit_Image_Format;
                   Original_Format : out Explicit_Image_Format);
   procedure Save (Object : in out Image; File_Name : String;
                   Image_Type : Image_Save_Type);

   function Width    (Object : Image) return GL.Types.Int;
   function Height   (Object : Image) return GL.Types.Int;
   function Channels (Object : Image) return Explicit_Image_Format;
   function Loaded   (Object : Image) return Boolean;

   procedure Clear (Object : in out Image);

   procedure To_Texture (Object  : Image;
                         Texture : in out GL.Objects.Textures.Texture'Class;
                         Flags   : Texture_Flags);

   procedure To_Cubemap (Object     : Image;
                         Texture    : in out GL.Objects.Textures.Texture'Class;
                         Face_Order : Cubemap_Layout;
                         Flags      : Texture_Flags);

private
   type Image_Data is record
      Pointer         : System.Address;
      Width, Height   : aliased GL.Types.Int;
      Channels        : aliased Explicit_Image_Format;
      Reference_Count : Positive;
   end record;

   type Image_Data_Access is access all Image_Data;

   type Image is new Ada.Finalization.Controlled with record
      Reference : Image_Data_Access;
   end record;

end SOIL.Images;
