
with GL.Types;

package Vertex_Data is
   use  GL.Types;
   Element_Buffer_Data : array (1 .. 3) of UShort := (0, 1, 2);

   Vertex_Buffer_Data : Single_Array (Int range 1 .. 9)
                             :=  (-1.0, -1.0, 0.0,
                                   1.0, -1.0, 0.0,
                                   0.0, 1.0, 0.0);
end Vertex_Data;
