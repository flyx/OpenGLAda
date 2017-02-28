
with  Interfaces.C.Pointers;
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   type tElement_Array is array (GL.Types.int range <>) of aliased GL.Types.Single;

   Default_Terminator : GL.Types.Single := 0.0;

   package pVertex_Pointers is new Interfaces.C.Pointers
     (GL.Types.Int, GL.Types.Single, tElement_Array, Default_Terminator);

   Vertex_Buffer_Data : tElement_Array (0 .. 8)
                             :=  (-1.0, -1.0, 0.0,
                                   1.0, -1.0, 0.0,
                                   0.0, 1.0, 0.0);
end Vertex_Data;
