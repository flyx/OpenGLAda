
with Ada.Containers.Vectors;

with E2GA;

package Geosphere is

    type Geosphere_Vertex is record
        X   : float;
        Y   : float;
        Z   : float;
    end record;

    package Vertices_Package is new Ada.Containers.Vectors (positive, Geosphere_Vertex);
    type Vertices_Vector is new Vertices_Package.Vector with null record;

    type Geosphere_S is private;
    type Geosphere_Face_S is private;

    procedure GS_Compute (Sphere : Geosphere_S; Depth : integer);

private
    type V_Array is array (1 .. 3) of integer;
    type Child_Array is array (1 .. 3) of integer;
    type Neighbour_Array is array (1 .. 3) of integer;
    type Contour_Intersect_Array is array (1 .. 3) of integer;
    type Contour_Visited_Array is array (1 .. 3) of integer;

    type Geosphere_Face_S is record
        V                 : V_Array;
        Child             : Child_Array;
        Plane             : E2GA.Bivector;
        D                 : float;
        Depth             : integer;
        Neighbour         : Neighbour_Array;
        Contour_Intersect : Contour_Intersect_Array;
        Contour_Visited   : Contour_Visited_Array;
    end record;

    type Geosphere_S is record
        Num_Vertices      : Natural;
        Max_Vertices      : Natural;
        Num_Faces         : Natural;
        Max_Faces         : Natural;
        Num_Primitives         : Natural;
        Depth             : integer;
    end record;

end Geosphere;
