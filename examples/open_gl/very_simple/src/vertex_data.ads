
with Interfaces.C.Pointers;

with GL.Objects.Buffers;
with GL.Types;

package Vertex_Data is
    use GL.Types;
    type tPoint_Index is new GL.Types.Int;
    type tPoint_Coord is new GL.Types.Single;
    type tPoints_Array is array (tPoint_Index range <>) of aliased tPoint_Coord;

    package pVertex_Pointers is new Interfaces.C.Pointers
      (tPoint_Index, tPoint_Coord, tPoints_Array, 0.0);

    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (pVertex_Pointers);

    Vertices  : tPoints_Array (0 .. 5) :=
                      (0.0, 0.5, 0.5, -0.5, -0.5, -0.5);
end Vertex_Data;
