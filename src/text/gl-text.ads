--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

private with Ada.Containers.Hashed_Maps;
private with Ada.Finalization;
with FT.Faces;
with GL.Objects.Textures;
private with GL.Objects.Vertex_Arrays;
private with GL.Objects.Buffers;
private with GL.Objects.Programs;
with GL.Types.Colors;
private with GL.Uniforms;

package GL.Text is
   Rendering_Error : exception;

   --  This type holds a reference to the compiled shader program used to render
   --  text as bitmaps. It needs to be created once and can be shared amongst
   --  all renderers. The shader program can only be initialized once the target
   --  OpenGL context has been created.
   --
   --  Since this type only holds references to OpenGL objects, it has reference
   --  semantics when copying; the references are automatically
   --  reference-counted.
   type Shader_Program_Reference is private;

   --  A renderer instance needs to be created for each font family, size and
   --  variant you want to render. A renderer renders text to a monochrome
   --  texture which can then be used als alpha-channel to render the text with
   --  arbitrary color / texture to the screen.
   type Renderer_Reference is tagged private;

   subtype UTF_8_String is String;
   type Pixel_Difference is new Interfaces.C.int;
   subtype Pixel_Size is Pixel_Difference range 0 .. Pixel_Difference'Last;

   --  Compiles the necessary shaders and links them into a program. OpenGL
   --  context must be initialized before creating the program!
   procedure Create (Object : in out Shader_Program_Reference);

   --  Returns True iff Create has been called successfully on the given
   --  object.
   function Created (Object : Shader_Program_Reference) return Boolean;

   --  Creates a renderer that renders the font face given by Face.
   procedure Create (Object : in out Renderer_Reference;
                     Program : Shader_Program_Reference;
                     Face : FT.Faces.Face_Reference);

   --  Creates a renderer that renders the font face at Face_Index in the
   --  font file specified by Font_Path with the pixel size given by Size.
   procedure Create (Object : in out Renderer_Reference;
                     Program : Shader_Program_Reference;
                     Font_Path  : String;
                     Face_Index : FT.Faces.Face_Index_Type;
                     Size : Pixel_Size);

   --  Returns True iff Create has been called successfully on the given
   --  object.
   function Created (Object : Renderer_Reference) return Boolean;

   procedure Calculate_Dimensions (Object : Renderer_Reference;
                                   Content : UTF_8_String;
                                   Width : out Pixel_Size;
                                   Y_Min : out Pixel_Difference;
                                   Y_Max : out Pixel_Size);

   function To_Texture (Object : Renderer_Reference; Content : UTF_8_String;
                        Text_Color : GL.Types.Colors.Color)
                         return GL.Objects.Textures.Texture;

   function To_Texture (Object : Renderer_Reference; Content : UTF_8_String;
                        Width, Y_Min, Y_Max : Pixel_Difference;
                        Text_Color : GL.Types.Colors.Color)
                        return GL.Objects.Textures.Texture;
private
   type Shader_Program_Reference is record
      Id : GL.Objects.Programs.Program;
      Info_Id, Texture_Id, Color_Id, Transform_Id : GL.Uniforms.Uniform;
      Square_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Square_Buffer : GL.Objects.Buffers.Buffer;
   end record;

   type Loaded_Character is record
      Width, Y_Min, Y_Max, Advance, Left : Pixel_Difference;
      Image : GL.Objects.Textures.Texture;
   end record;

   function Hash (Value : FT.ULong) return Ada.Containers.Hash_Type;

   package Loaded_Characters is new Ada.Containers.Hashed_Maps
     (FT.ULong, Loaded_Character, Hash, Interfaces.C."=");

   type Renderer_Data is record
      Face : FT.Faces.Face_Reference;
      Refcount : Natural := 1;
      Characters : Loaded_Characters.Map;
      Program : Shader_Program_Reference;
   end record;

   type Pointer is access Renderer_Data;

   type Renderer_Reference is new Ada.Finalization.Controlled with record
      Data : Pointer;
   end record;

   overriding procedure Adjust (Object : in out Renderer_Reference);
   overriding procedure Finalize (Object : in out Renderer_Reference);
end GL.Text;
