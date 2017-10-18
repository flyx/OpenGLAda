--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Finalization;

package SOIL.Images is
   pragma Preelaborate;

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
   function Data     (Object : Image) return GL.Objects.Textures.Image_Source;

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
      Pointer         : GL.Objects.Textures.Image_Source;
      Width, Height   : aliased GL.Types.Int;
      Channels        : aliased Explicit_Image_Format;
      Reference_Count : Positive;
   end record;

   type Image_Data_Access is access all Image_Data;

   type Image is new Ada.Finalization.Controlled with record
      Reference : Image_Data_Access;
   end record;

end SOIL.Images;
