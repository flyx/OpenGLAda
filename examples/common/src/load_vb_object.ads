
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;

package Load_VB_Object is

   type VBM_Flags is (VBM_Has_Vertices, VBM_Has_Indices, VBM_Has_Frames,
                      VBM_Has_Materials);
   for VBM_Flags use
     (VBM_Has_Vertices => 1, VBM_Has_Indices => 2,
      VBM_Has_Frames => 4, VBM_Has_Materials => 8);

   type Material_Texture is private;
   type VB_Object is private;

   function Get_Vertex_Count (Object : VB_Object; Frame_Index : UInt := 1) return Int;
   procedure Load_From_VBM (File_Name : String; VBM_Object : in out VB_Object;
                            Vertex_Index, Normal_Index, Tex_Coord0_Index : Int;
                            Result : out Boolean);
   procedure Print_Attributes_Header (Message : String; Object : VB_Object;
                                      Attributes_Index : UInt);
   procedure Print_VBM_Object_Data (Message : String; Object : VB_Object);
   procedure Print_VBM_Frame_Data (Message : String; Object : VB_Object;
                                   Frame_Index : UInt);
   procedure Render (VBM_Object : VB_Object;
                     Frame_Index : UInt := 1; Instances : UInt := 0);

private
   New_Header_Magic : UInt := 16#314d4253#;  -- 1MBS

   type VBM_Header (Magic : UInt := New_Header_Magic) is record
      Size           : UInt := 0;
      Name           : String (1 .. 64);
      Num_Attributes : UInt := 0;
      Num_Frames     : UInt := 0;
      Num_Vertices   : UInt := 0;
      Num_Indices    : UInt := 0;
      Index_Type     : Numeric_Type := UByte_Type;
      Num_Materials  : UInt := 0;
      Flags          : UInt := 0;
      case Magic is
         when others =>
            Num_Chunks  : UInt;
      end case;
   end record;

   type VBM_Attributes_Header is record
      Name           : String (1 .. 64);
      Attribute_Type : Numeric_Type := UByte_Type;
      Components     : UInt := 0;
      Flags          : UInt := 0;
   end record;

   package Attribute_Package is new Ada.Containers.Vectors
     (Natural, VBM_Attributes_Header);
   type Attribute_Headers_List is new Attribute_Package.Vector with null record;

   type VBM_Frame_Header is record
      First        : UInt := 0;
      Num_Vertices : UInt := 0;
      Flags        : UInt := 0;
   end record;

   type VBM_Render_Chunk is record
      Material_Index  : UInt := 0;
      First           : UInt := 0;
      Count           : UInt := 0;
   end record;

   package Chunk_Package is new
     Ada.Containers.Vectors (Positive, VBM_Render_Chunk);
   type Render_Chunk_List is new Chunk_Package.Vector with null record;

   type VBM_Vec2F is record
      X  : Float := 0.0;
      Y  : Float := 0.0;
   end record;

   type VBM_Vec3F is record
      X  : Float := 0.0;
      Y  : Float := 0.0;
      Z  : Float := 0.0;
   end record;

   type VBM_Vec4F is record
      X  : Float := 0.0;
      Y  : Float := 0.0;
      Z  : Float := 0.0;
      W  : Float := 0.0;
   end record;

   type VBM_Material is record
      Name               : String (1 .. 32);
      Ambient            : VBM_Vec3F;
      Diffuse            : VBM_Vec3F;
      Specular           : VBM_Vec3F;
      Specular_Exponent  : VBM_Vec3F;
      Transmission       : VBM_Vec3F;
      Shininess          : Float := 0.0;
      Alpha              : Float := 0.0;
      Ambient_Map        : String (1 .. 64);
      Diffuse_Map        : String (1 .. 64);
      Specular_Map       : String (1 .. 64);
      Normal_Map         : String (1 .. 64);
   end record;

   package Materials_Package is new
     Ada.Containers.Vectors (Positive, VBM_Material);
   type Materials_List is new Materials_Package.Vector with null record;

   type Material_Texture is record
      Diffuse  : UInt := 0;
      Specular : UInt := 0;
      Normal   : UInt := 0;
   end record;
   package Material_Textures_Package is new
     Ada.Containers.Doubly_Linked_Lists (Material_Texture);
   type Material_Texture_List is new Material_Textures_Package.List with null record;

   package Frame_Headers_Package is new
     Ada.Containers.Vectors (Positive, VBM_Frame_Header);
   type Frame_Headers_List is new Frame_Headers_Package.Vector with null record;

   Max_Frames : Positive := 10000;
   subtype Num_Frames_Range is Positive range 1 .. Max_Frames;
   type VAO_List is array (Num_Frames_Range range <>) of
     GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   type Buffer_List is array (Num_Frames_Range range <>) of
     GL.Objects.Buffers.Buffer;

   type VB_Object is record
      --  One Vertex_Array_Object
      Vertex_Array        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Attribute_Buffer    : GL.Objects.Buffers.Buffer;  --  One Attribute_Buffer
      Index_Buffer        : GL.Objects.Buffers.Buffer;  --  Index_Buffer
      Header              : VBM_Header;
      Num_Frames          : Positive := 1;
      Attribute_Headers   : Attribute_Headers_List;  --  Array of attribute records
      Frame_Headers       : Frame_Headers_List;      --  Array of frame records
      Materials           : Materials_List;          --  Array of material records
      Chunks              : Render_Chunk_List;       --  Array of chunk records
      Material_Textures   : Material_Texture_List;
   end record;

end Load_VB_Object;
