
with GL.Types;

package Load_Object_File is

   procedure Get_Array_Sizes (File_Name : String; Vertex_Count, UV_Count,
                              Normal_Count, Face_Count : out GL.Types.Int;
                              Mesh_Count, Usemtl_Count : out Integer);
    procedure Load_Object (File_Name : String;
                        Vertices : out GL.Types.Singles.Vector3_Array;
                        UVs      : out GL.Types.Singles.Vector2_Array;
                        Normals  : out GL.Types.Singles.Vector3_Array);
end Load_Object_File;
