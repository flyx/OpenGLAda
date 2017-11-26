
with GL.Types;

package Load_Object_File is

    procedure Load_Object (File_Name  : String;
                        Vertices : out GL.Types.Singles.Vector3_Array;
                        UVs      : out GL.Types.Singles.Vector2_Array;
                        Normals  : out GL.Types.Singles.Vector3_Array);
end Load_Object_File;
