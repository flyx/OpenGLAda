
with GL.Types;

package VBO_Indexer is

   type Indices_Array is array (GL.Types.Int range <>) of GL.Types.Int;

   procedure Index_VBO (Vertices_In    : GL.Types.Singles.Vector3_Array;
                        UVs_In         : GL.Types.Singles.Vector2_Array;
                        Normals_In     : GL.Types.Singles.Vector3_Array;
                        Vertices       : in out GL.Types.Singles.Vector3_Array;
                        UVs            : in out GL.Types.Singles.Vector2_Array;
                        Normals        : in out GL.Types.Singles.Vector3_Array;
                        VBO_Indices    : out GL.Types.Int_Array;
                        Last_VBO_Index : out GL.Types.Int;
                        Last_Vertex    : out GL.Types.Int;
                        Last_UV        : out GL.Types.Int;
                        Last_Normal    : out GL.Types.Int);

end VBO_Indexer;
