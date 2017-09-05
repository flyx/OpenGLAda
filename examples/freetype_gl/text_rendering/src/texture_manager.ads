
with Ada.Containers.Vectors;

with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Textures;
with GL.Types;

with FT.API;

package Texture_Manager is

   type Character_Record is private;
   subtype V_Buffer is GL.Objects.Buffers.Buffer;
   --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
   subtype Vertex_Array is GL.Types.Singles.Vector4_Array (1 .. 6);

   function Advance_X (Data : Character_Record) return GL.Types.Int;
   function Data (Index : GL.Types.Long) return Character_Record;
   function Left (Data : Character_Record) return GL.Types.Single;
   function Rows (Data : Character_Record) return GL.Types.Single;
   procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer);
   function Char_Texture (Data : Character_Record)
                          return GL.Objects.Textures.Texture;
   function Top (Data : Character_Record) return GL.Types.Single;
   function Width (Data : Character_Record) return GL.Types.Single;
private

   type Character_Size is record
      Width     : GL.Types.Single;
      Rows      : GL.Types.Single;
   end record;

   type Character_Bearing is record
      Left      : GL.Types.Single;
      Top       : GL.Types.Single;
   end record;

   type Character_Record is record
      Texture   : GL.Objects.Textures.Texture;
      Size      : Character_Size;
      Bearing   : Character_Bearing;
      Advance_X : GL.Types.Int;
   end record;

   package Data_Vector_Package is new Ada.Containers.Vectors (Natural, Character_Record);
   type Character_Data_Vector is new Data_Vector_Package.Vector with null record;

end Texture_Manager;
