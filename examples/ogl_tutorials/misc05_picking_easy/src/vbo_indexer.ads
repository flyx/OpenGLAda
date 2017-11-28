
with GL.Types;

package VBO_Indexer is

   procedure Index_VBO (Vertices_In : GL.Types.Singles.Vector3_Array;
                        UVs_In      : GL.Types.Singles.Vector2_Array;
                        Normals_In  : GL.Types.Singles.Vector3_Array;
                        Vertices    : in out GL.Types.Singles.Vector3_Array;
                        UVs         : in out GL.Types.Singles.Vector2_Array;
                        Normals     : in out GL.Types.Singles.Vector3_Array;
                        Indices     : out GL.Types.Ints.Vector3_Array;
                        Last_Index  : out GL.Types.Int;
                        Last_Vertex : out GL.Types.Int);

end VBO_Indexer;
