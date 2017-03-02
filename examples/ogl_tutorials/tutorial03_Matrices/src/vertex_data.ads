

with  Interfaces.C.Pointers;
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   type tElement_Array is array (GL.Types.int range <>) of aliased GL.Types.Single;

   Default_Terminator : GL.Types.Single := 0.0;

 --  package Interfaces.C.Pointers generic interface:
 --  type Index is (<>);          : GL.Types.int
 --  type Element is private;     : GL.Types.Single
 --  type Element_Array is array (Index range <>) of aliased Element;
 --  Default_Terminator : Element;

   package pVertex_Pointers is new Interfaces.C.Pointers
     (GL.Types.Int, GL.Types.Single, tElement_Array, Default_Terminator);

   Element_Buffer_Data : Array (0 .. 2) of UShort := (0, 1, 2);

   Vertex_Buffer_Data : tElement_Array (0 .. 8)
                             :=  (-1.0, -1.0, 0.0,
                                   1.0, -1.0, 0.0,
                                   0.0, 1.0, 0.0);

end Vertex_Data;
