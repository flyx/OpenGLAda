
with Interfaces.C.Pointers;

with GL.Objects.Buffers;
with GL.Types;

package Vertex_Data is
    use GL.Types;
    type tPoints_Array is array (GL.Types.Int range <>) of aliased GL.Types.Single;
    type tElements_Array is array (GL.Types.Int range <>) of aliased GL.Types.Int;

    package pVertex_Pointers is new Interfaces.C.Pointers
      (GL.Types.Int, GL.Types.Single, tPoints_Array, 0.0);
    package pElement_Pointers is new Interfaces.C.Pointers
      (GL.Types.Int, GL.Types.Int, tElements_Array, 0);

    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (pVertex_Pointers);
    procedure Load_Element_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (pElement_Pointers);

    Vertices_Coloured  : tPoints_Array (0 .. 3 * 5 - 1) :=
                      (0.0, 0.5, 1.0, 0.0, 0.0,
                       0.5, -0.5, 0.0, 1.0, 0.0,
                       -0.5, -0.5, 0.0, 0.0, 1.0);
    Vertices_No_Colour  : tPoints_Array (0 .. 3 * 2 - 1) :=
                      (0.0, 0.5,
                       0.5, -0.5,
                       -0.5, -0.5);
    Elements  : tElements_Array (0 .. 2) := (0, 1, 2);
end Vertex_Data;
