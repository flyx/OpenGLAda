
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   Vertex_Positions : Singles.Vector4_Array (1 .. 4)
     :=  ((-1.0, -1.0, 0.0, 1.0),
          (1.0, -1.0, 0.0, 1.0),
          (-1.0, 1.0, 0.0, 1.0),
          (-1.0, -1.0, 0.0, 1.0));

   Vertex_Colours : Singles.Vector4_Array (1 .. 4)
     :=  ((1.0, 1.0, 1.0, 1.0),
          (1.0, 1.0, 0.0, 1.0),
          (1.0, 0.0, 1.0, 1.0),
          (0.0, 1.0, 1.0, 1.0));

   Vertex_Indices : UInt_Array (1 .. 3) := (0, 1, 2);

end Vertex_Data;
