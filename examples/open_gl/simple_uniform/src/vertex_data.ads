
with Interfaces.C.Pointers;

with GL.Objects.Buffers;
with GL.Types;

package Vertex_Data is
    use GL.Types;
    type tPoints_Array is array (GL.Types.Int range <>) of aliased GL.Types.Single;

    package pVertex_Pointers is new Interfaces.C.Pointers
      (GL.Types.Int, GL.Types.Single, tPoints_Array, 0.0);

    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (pVertex_Pointers);

    Vertices  : tPoints_Array (0 .. 5) :=
                      (0.0, 0.5,
                       0.5, -0.5,
                       -0.5, -0.5);
end Vertex_Data;
