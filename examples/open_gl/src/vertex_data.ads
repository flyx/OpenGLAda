
with Interfaces.C.Pointers;

with GL.Objects.Buffers;
with GL.Types;

package Vertex_Data is
    use GL.Types;
    type tPoint_Index is new GL.Types.Int;
    type tPoint_Coord is new GL.Types.Single;
    type tElement is new GL.Types.Int;
    type tPoints_Array is array (tPoint_Index range <>) of aliased tPoint_Coord;
    type tElements_Array is array (tPoint_Index range <>) of aliased tElement;

    package pVertex_Pointers is new Interfaces.C.Pointers
      (tPoint_Index, tPoint_Coord, tPoints_Array, 0.0);
    package pElement_Pointers is new Interfaces.C.Pointers
      (tPoint_Index, tElement, tElements_Array, 0);

    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (pVertex_Pointers);
    procedure Load_Element_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (pElement_Pointers);

    Vertices  : tPoints_Array (0 .. 4 * 7 - 1) :=
                  --    Position      Colour      Texture Coordinates
                      (-0.5,  0.5, 1.0, 0.0, 0.0, 0.0, 0.0,  --  Top-left
                        0.5,  0.5, 0.4, 1.0, 0.0, 1.0, 0.0,  --  Top-right
                        0.5, -0.5, 0.4, 0.0, 1.0, 1.0, 1.0,  --  Bottom-right
                       -0.5, -0.5, 1.0, 1.0, 1.0, 0.0, 1.0); --  Bottom-left
    --  Use elements to draw a square using two triangles
    Elements  : tElements_Array (0 .. 5) := (0, 1, 2,   --  First triangle
                                             2, 3, 0);  --  Second triangle

end Vertex_Data;
