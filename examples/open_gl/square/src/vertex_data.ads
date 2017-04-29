
with Interfaces.C.Pointers;

with GL.Objects.Buffers;
with GL.Types;

with Maths;

package Vertex_Data is
    use GL.Types;

    type Elements_Array is array (GL.Types.Int range <>) of aliased GL.Types.Int;

    package Element_Pointers is new Interfaces.C.Pointers
      (GL.Types.Int, GL.Types.Int, Elements_Array, 0);

    procedure Load_Element_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (Element_Pointers);

    Vertices  : Maths.Vector5_Array (1 .. 4) :=
                      ((-0.5,  0.5, 1.0, 0.0, 0.0),      --  Top-left
                        (0.5,  0.5, 0.0, 1.0, 0.0),       --  Top-right
                        (0.5, -0.5, 0.0, 0.0, 1.0),       --  Bottom-right
                       (-0.5, -0.5, 1.0, 1.0, 1.0));      --  Bottom-left

    Elements  : Elements_Array (1 .. 6) := (0, 1, 2,
                                            2, 3, 0);
end Vertex_Data;
