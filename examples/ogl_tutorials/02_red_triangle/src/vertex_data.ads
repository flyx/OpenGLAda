
with  Interfaces.C.Pointers;
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   Vertex_Buffer_Data : Singles.Vector3_Array (1 .. 3)
                             :=  ((-1.0, -1.0, 0.0),
                                   (1.0, -1.0, 0.0),
                                   (0.0, 1.0, 0.0));
end Vertex_Data;
