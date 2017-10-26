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

   type Font_Rendering_Program is private;

   type Renderer_Reference is tagged private;

   subtype UTF_8_String is String;
   type Pixel_Difference is new Interfaces.C.int;

   function Init_Program return Font_Rendering_Program;

   function Exists (Object : Renderer_Reference) return Boolean;

   procedure Create (Object : in out Renderer_Reference;
                     Program : Font_Rendering_Program;
                     Face : FT.Faces.Face_Reference);

   procedure Create (Object : in out Renderer_Reference;
                     Program : Font_Rendering_Program;
                     Font_Path  : String;
                     Face_Index : FT.Faces.Face_Index_Type);

   procedure Calculate_Dimensions (Object : Renderer_Reference;
                                   Content : UTF_8_String;
                                   Width, Y_Min, Y_Max : out Pixel_Difference);

   function To_Texture (Object : Renderer_Reference; Content : UTF_8_String;
                        Text_Color : GL.Types.Colors.Color)
                         return GL.Objects.Textures.Texture;

   function To_Texture (Object : Renderer_Reference; Content : UTF_8_String;
                        Width, Y_Min, Y_Max : Pixel_Difference;
                        Text_Color : GL.Types.Colors.Color)
                        return GL.Objects.Textures.Texture;
private
   type Font_Rendering_Program is record
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
      Program : Font_Rendering_Program;
   end record;

   type Pointer is access Renderer_Data;

   type Renderer_Reference is new Ada.Finalization.Controlled with record
      Data : Pointer;
   end record;

   overriding procedure Adjust (Object : in out Renderer_Reference);
   overriding procedure Finalize (Object : in out Renderer_Reference);
end GL.Text;
