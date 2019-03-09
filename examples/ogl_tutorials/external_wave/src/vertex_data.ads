
with Ada.Numerics.Generic_Elementary_Functions;

with GL.Types;
with GL.Objects.Buffers;

with Maths;

package Vertex_Data is
   use  GL.Types;

   package Single_Functions is new Ada.Numerics.Generic_Elementary_Functions (Single);
   procedure Load_Element_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (GL.Types.Int_Pointers);

   Grid_Height       : constant Int := 50;
   Grid_Width        : constant Int := 50;
   Num_Vertices      : constant Int := Grid_Height * Grid_Width;

   Quad_Height       : constant Int := Grid_Height - 1;
   Quad_Width        : constant Int := Grid_Width - 1;
   Num_Quads         : constant Int := Quad_Width * Quad_Height;
   --  Each quad comprises two triangles,
   --  thus six points of which two are identical pairs.
   Num_Quad_Vertices : constant Int := 6 * Num_Quads;

   Animation_Speed : constant Single := 2.2;
   Max_dt          : constant Single := 0.1;

   type Grid_Array is array (1 .. Grid_Width, 1 .. Grid_Height) of Single;

   --   The grid will look like this:
   --
   --         4   5   6
   --         *---*---*
   --         |   |   |
   --         | 1 | 2 |
   --         |   |   |
   --         *---*---*
   --         1  2   3

   type Vertex is record
      X     : Single;
      Y     : Single;
      Z     : Single;
      Red   : Single;
      Green : Single;
      Blue  : Single;
   end record;

   Stride        : constant Int := 6 * 4;
   Vertex_Offset : constant Int := 0 * 4;
   Colour_Offset : constant Int := 3 * 4;
   Num_Elements       : constant Int := Num_Quad_Vertices + 48 * Stride; --  192 = 48 * 4
   Vertex_Buffer_Data : Maths.Vector6_Array (1 .. Num_Vertices);
   Quad_Element_Array : Int_Array (1 .. Num_Elements);

   procedure Adjust_Grid;
   procedure Calculate_Grid (dt : Single);
   procedure Get_Data (Press, VX, VY : out Grid_Array);
   procedure Propogate_Wave (dt : Single);
   procedure Initialize_Grid;
   procedure Initialize_Simulation;

end Vertex_Data;
