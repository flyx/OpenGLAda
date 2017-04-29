
with Interfaces.C.Pointers;

with GL.Objects.Buffers;
with GL.Types;

package Vertex_Data is
    use GL.Types;

    type Elements_Array is array (GL.Types.Int range <>) of aliased GL.Types.Int;

    package Element_Pointers is new Interfaces.C.Pointers
      (GL.Types.Int, GL.Types.Int, Elements_Array, 0);

    procedure Load_Element_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (Element_Pointers);

    Vertex_Array_Size : GL.Types.Int := 4 * 2;
    Vertices : Singles.Vector2_Array (1 .. 4) :=
                 ((-1.0, -1.0),
                   (1.0, -1.0),
                  (-1.0,  1.0),
                   (1.0,  1.0));

    Elements : Elements_Array (1 .. 4) := (0, 1, 2, 3);
end Vertex_Data;
