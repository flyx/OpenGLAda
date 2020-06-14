with Ada.Streams;
with GL.Objects.Textures;
with GL.Pixels;

package GL.Images is
   -- This procedure loads the contents of the given Source to the given
   -- texture object. The texture's image is created with the given internal
   -- format.
   --
   -- The image type is determined from its signature. TGA images do not have a
   -- signature, so if the source contains TGA data, you need to set Try_TGA to
   -- True. 
   procedure Load_Image_To_Texture (
     Source         : in out Ada.Streams.Root_Stream_Type'Class;
     Texture        : in out GL.Objects.Textures.Texture'Class;
     Texture_Format : GL.Pixels.Internal_Format;
     Try_TGA        : Boolean := False);
end GL.Images;