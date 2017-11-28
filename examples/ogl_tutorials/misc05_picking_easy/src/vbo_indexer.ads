
with GL.Types;

package VBO_Indexer is

   procedure Index_VBO (Vertices_In : GL.Types.Singles.Vector3_Array;
                        UVs_In      : GL.Types.Singles.Vector2_Array;
                        Normals_In  : GL.Types.Singles.Vector3_Array;
                        Vertices    : GL.Types.Singles.Vector3_Array;
                        UVs         : GL.Types.Singles.Vector2_Array;
                        Normals     : GL.Types.Singles.Vector3_Array);

end VBO_Indexer;
