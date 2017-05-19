
with GL.Types;

package Vertex_Data is
    use GL.Types;

    Vertices  : Singles.Vector2_Array (1 .. 3) :=
                      ((0.0, 0.5),
                       (0.5, -0.5),
                       (-0.5, -0.5));
end Vertex_Data;
