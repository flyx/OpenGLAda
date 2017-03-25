
with GL.Objects.Buffers;
with GL.Types;

package Vertex_Data is
    use GL.Types;

    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (Single_Pointers);

    Vertices  : Single_Array (0 .. 5) :=
                      (0.0, 0.5,
                       0.5, -0.5,
                       -0.5, -0.5);
end Vertex_Data;
