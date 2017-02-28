
with Interfaces.C.Pointers;

with GL.Objects.Buffers;
with GL.Types;

package Vertex_Data is
    use GL.Types;
    type tPoint_Index is new GL.Types.Int;
    type tPoint_Coord is new GL.Types.Single;
    type tElement is new GL.Types.Int;
    type tPoints_Array is array (tPoint_Index range <>) of aliased tPoint_Coord;
    type tElements_Array is array (tPoint_Index range <>) of aliased tElement;

    package pVertex_Pointers is new Interfaces.C.Pointers
      (tPoint_Index, tPoint_Coord, tPoints_Array, 0.0);
    package pElement_Pointers is new Interfaces.C.Pointers
      (tPoint_Index, tElement, tElements_Array, 0);

    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (pVertex_Pointers);
    procedure Load_Element_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (pElement_Pointers);

    Vertex_Array_Size : tPoint_Index := 4 * 2;
    Vertices : tPoints_Array (0 .. Vertex_Array_Size - 1) :=
                 (-1.0, -1.0,
                   1.0, -1.0,
                  -1.0,  1.0,
                   1.0,  1.0);

    Elements : tElements_Array (0 .. 3) := (0, 1, 2, 3);
end Vertex_Data;
