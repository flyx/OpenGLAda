
with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types;
with GL.Objects.Buffers;

with Maths;

package Vertex_Data is
    use  GL.Types;

    package Single_Functions is new Ada.Numerics.Generic_Elementary_Functions (Single);
    procedure Load_Element_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (GL.Types.Int_Pointers);

    Grid_Height  : constant Int := 50;
    Grid_Width   : constant Int := 50;
    Num_Vertices : constant Int := Grid_Height * Grid_Width;

    Quad_Height  : constant Int := Grid_Height - 1;
    Quad_Width   : constant Int := Grid_Width - 1;
    Num_Quads    : constant Int := Quad_Width * Quad_Height;

   type Grid_Array is array (1 .. Grid_Width, 1 .. Grid_Height) of single;

    --   The grid will look like this:
    --
    --         3   4   5
    --         *---*---*
    --         |   |   |
    --         | 0 | 1 |
    --         |   |   |
    --         *---*---*
    --         0   1   2

    type Vertex is record
        X     : single;
        Y     : single;
        Z     : single;
        Red   : single;
        Green : single;
        Blue  : single;
    end record;

   subtype Vertices_Array is  Maths.Vector6_Array (1 .. Num_Vertices);
   subtype Elements_Array is Int_Array (1 .. 4 * Num_Quads);

    Vertex_Buffer_Data : Vertices_Array;
    Quad_Element_Array : Elements_Array;

    procedure Adjust_Grid (Pressure    : in out Grid_Array);
    procedure Propogate_Wave (Pressure : in out Grid_Array;
                              Vel_X    : in out Grid_Array;
                              Vel_Y    : in out Grid_Array;
                              dt       : single);
    procedure Initialize_Grid (Pressure : in out Grid_Array;
                              Vel_X    : in out Grid_Array;
                              Vel_Y    : in out Grid_Array);
    procedure Initialize_Vertices (Vertices      : in out Vertices_Array;
                                   Quad_Elements : in out Elements_Array);
end Vertex_Data;
