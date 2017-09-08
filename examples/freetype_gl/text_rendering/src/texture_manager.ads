
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
   function Data (Index : GL.Types.Int) return Character_Record;
   function Left (Data : Character_Record) return GL.Types.Int;
   function Rows (Data : Character_Record) return GL.Types.Int;
   procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer);
   function Char_Texture (Data : Character_Record)
                          return GL.Objects.Textures.Texture;
   function Top (Data : Character_Record) return GL.Types.Int;
   function Width (Data : Character_Record) return GL.Types.Int;
private

   type Character_Size is record
      Width     : GL.Types.Int;
      Rows      : GL.Types.Int;
   end record;

   type Character_Bearing is record
      Left      : GL.Types.Int;
      Top       : GL.Types.Int;
   end record;

   type Character_Record is record
      Texture   : GL.Objects.Textures.Texture;
      Size      : Character_Size;
      Bearing   : Character_Bearing;
      Advance_X : GL.Types.Int;
   end record;

end Texture_Manager;
