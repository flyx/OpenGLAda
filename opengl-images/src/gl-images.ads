with Ada.Streams;
with GL.Objects.Textures;
with GL.Pixels;

package GL.Images is
   -- This procedure loads the contents of the given Source to the given
   -- texture object.
   -- The texture object will be initialized if it is uninitialized.
   -- The texture's image is created with the given internal format.
   --
   -- The image type is determined from its signature. TGA images do not have a
   -- signature, so if the source contains TGA data, you need to set Try_TGA to
   -- True.
   --
   -- If the given Texture_Format contains components with more than 8 bits,
   -- the image is loaded with 16-bit components before it is given to OpenGL.
   procedure Load_Image_To_Texture (
     Source         : in out Ada.Streams.Root_Stream_Type'Class;
     Texture        : in out GL.Objects.Textures.Texture'Class;
     Texture_Format : GL.Pixels.Internal_Format;
     Try_TGA        : Boolean := False);
     
   -- Like Load_Image_To_Texture, but takes the path to a file as input.
   procedure Load_File_To_Texture (
     Path           : String;
     Texture        : in out GL.Objects.Textures.Texture'Class;
     Texture_Format : GL.Pixels.Internal_Format;
     Try_TGA        : Boolean := False);
end GL.Images;