
with GL.Objects.Buffers;
with GL.Types;

package Vertex_Data is
    use GL.Types;

    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (GL.Types.Single_Pointers);
    procedure Load_Element_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (GL.Types.Int_Pointers);

    Vertices_Coloured  : Single_Array (0 .. 3 * 5 - 1) :=
                      (0.0, 0.5, 1.0, 0.0, 0.0,
                       0.5, -0.5, 0.0, 1.0, 0.0,
                       -0.5, -0.5, 0.0, 0.0, 1.0);

    Elements  : Int_Array (0 .. 2) := (0, 1, 2);
end Vertex_Data;
