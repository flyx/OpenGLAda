
with GL.Objects.Buffers;
with GL.Types;

with Maths;

package Vertex_Data is
    use GL.Types;

    procedure Load_Element_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (GL.Types.Int_Pointers);

    Vertices_Coloured  : Maths.Vector5_Array (1 .. 3) :=
                      ((0.0, 0.5, 1.0, 0.0, 0.0),
                       (0.5, -0.5, 0.0, 1.0, 0.0),
                       (-0.5, -0.5, 0.0, 0.0, 1.0));

    Elements  : Int_Array (1 .. 3) := (0, 1, 2);
end Vertex_Data;
