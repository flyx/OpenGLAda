
with Ada.Containers.Doubly_Linked_Lists;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

package Load_VB_Object is

   type VB_Object is private;
   type VBM_Flags is (VBM_Has_Vertices, VBM_Has_Indices, VBM_Has_Frames,
                      VBM_Has_Materials);
   for VBM_Flags use
     (VBM_Has_Vertices => 1, VBM_Has_Indices => 2,
      VBM_Has_Frames => 4, VBM_Has_Materials => 8);

   type Material_Texture is private;

   procedure Load_From_VBM (File_Name : String;
                               Vertex_Index, Normal_Index,
                               Tex_Coord0_Index : out Int; Result : out Boolean);

private
   New_Header_Magic : UInt := 16#314d4253#;
   type VBM_Header (Magic : UInt := New_Header_Magic) is record
      Name           : String (1 .. 64);
      Size           : UInt;
      Num_Attributes : UInt;
      Num_Frames     : UInt;
      Num_Vertices   : UInt;
      Num_Indices    : UInt;
      Num_Materials  : UInt;
      Index_Type     : UInt;
      Flags          : UInt;
      case Magic is
         when others =>
            Num_Chunks  : UInt;
      end case;
   end record;

   type VBM_Old_Header is record
      Name           : String (1 .. 64);
      Magic          : UInt;
      Size           : UInt;
      Num_Attributes : UInt;
      Num_Frames     : UInt;
      Num_Vertices   : UInt;
      Num_Indices    : UInt;
      Num_Materials  : UInt;
      Index_Type     : UInt;
      Flags          : UInt;
   end record;

   type VBM_Attributes_Header is record
      Name           : String (1 .. 64);
      Attribute_Type : UInt;
      Components     : UInt;
      Flags          : VBM_Flags;
   end record;

   type VBM_Frame_Header is record
      First  : UInt;
      Count  : UInt;
      Flags  : VBM_Flags;
   end record;

   type VBM_Render_Chunk is record
      Material_Index  : UInt;
      First           : UInt;
      Count           : UInt;
   end record;

   type VBM_Vec2F is record
      X  : Float;
      Y  : Float;
   end record;

   type VBM_Vec3F is record
      X  : Float;
      Y  : Float;
      Z  : Float;
   end record;

   type VBM_Vec4F is record
      X  : Float;
      Y  : Float;
      Z  : Float;
      W  : Float;
   end record;

   type VBM_Material is record
      Name               : String (1 .. 32);
      Ambient            : VBM_Vec3F;
      Diffuse            : VBM_Vec3F;
      Specular           : VBM_Vec3F;
      Specular_Exponent  : VBM_Vec3F;
      Transmission       : VBM_Vec3F;
      Shininess          : Float;
      Alpha              : Float;
      Ambient_Map        : String (1 .. 64);
      Diffuse_Map        : String (1 .. 64);
      Specular_Map       : String (1 .. 64);
      Normal_Map         : String (1 .. 64);
   end record;

   type Material_Texture is record
      Diffuse  : UInt;
      Specular : UInt;
      Normal   : UInt;
   end record;
   package Material_Textures_Package is new
     Ada.Containers.Doubly_Linked_Lists (Material_Texture);
   type Material_Textures is new Material_Textures_Package.List with null record;

   type VB_Object is record
      Header             : VBM_Header;
      Attribute_Header   : VBM_Attributes_Header;
      Frame_Header       : VBM_Frame_Header;
      Vertex_Array       : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Attribute_Buffer   : GL.Objects.Buffers.Buffer;
      Indices            : GL.Objects.Buffers.Buffer;
      Material           : VBM_Material;
      Render_Chunk       : VBM_Render_Chunk;
      Texture_List       : Material_Textures;
   end record;

end Load_VB_Object;
