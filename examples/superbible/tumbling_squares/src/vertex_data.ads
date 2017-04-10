
with GL.Types; use GL.Types;

package Vertex_Data is

   Z_Pos : constant Single := 0.0;

    --  Vertex positions
    Vert_Plane : Singles.Vector3_Array (1 .. 6) :=
           ((-0.2, -0.2, Z_Pos),
            (-0.2, 0.2, Z_Pos),
             (0.2, 0.2, Z_Pos),

             (0.2, 0.2, Z_Pos),
             (0.2, -0.2, Z_Pos),
            (-0.2, -0.2, Z_Pos));

end Vertex_Data;
