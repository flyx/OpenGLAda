
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

package body Vertex_Data is

    Pressure        : Vertex_Data.Grid_Array;
    Vel_X           : Vertex_Data.Grid_Array;
    Vel_Y           : Vertex_Data.Grid_Array;

   --  -------------------------------------------------------------------------

   procedure Adjust_Grid is
      use Maths;
      Position : Int;
   begin
        for y_index in 1 .. Grid_Height loop
            for x_index in 1 .. Grid_Width loop
                Position := (y_index - 1) * Grid_Width + x_index;
                Vertex_Buffer_Data (Position) (Z) :=
                  Single (Pressure (x_index, y_index)) / 50.0;
            end loop;
        end loop;

    exception
        when others =>
            Put_Line ("An exception occurred in Adjust_Grid.");
            raise;
   end Adjust_Grid;

   --  -------------------------------------------------------------------------

    procedure Calculate_Grid (dt : single) is
        Acc_X     : Grid_Array;
        Acc_Y     : Grid_Array;
        X2        : int;
        Y2        : int;
        Time_Step : single := dt * Animation_Speed;
    begin
        for X in 1 .. Grid_Width - 1 loop
            X2 := X mod Grid_Width;
            for Y in 1 .. Grid_Height loop
                Acc_X (X, Y) := Pressure (X, Y) - Pressure (X2, Y);
            end loop;
        end loop;
        for Y in 1 .. Grid_Height - 1 loop
            Y2 := Y mod Grid_Height;
            for X in 1 .. Grid_Width loop
                Acc_Y (X, Y) := Pressure (X, Y) - Pressure (X, Y2);
            end loop;
        end loop;
        for X in 1 .. Grid_Width loop
            for Y in 1 .. Grid_Height loop
                Vel_X (X, Y) := Vel_X (X, Y) + Acc_X (X, Y) * Time_Step;
                Vel_Y (X, Y) := Vel_Y (X, Y) + Acc_Y (X, Y) * Time_Step;
            end loop;
        end loop;
        for X in 2 .. Grid_Width loop
            X2 := X - 1;
            for Y in 2 .. Grid_Height loop
                Y2 := Y - 1;
                Pressure (X, Y) := Pressure (X, Y) + (Vel_X (X2, Y) - Vel_X (X, Y)
                                   + Vel_Y (X, Y2) - Vel_Y (X, Y)) * Time_Step;
            end loop;
        end loop;
    exception
        when others =>
            Put_Line ("An exception occurred in Calculate_Grid.");
            raise;
    end Calculate_Grid;

    --  ------------------------------------------------------------------------

    procedure Get_Data (Press, VX, VY : out Grid_Array) is
    begin
        Press := Pressure;
        VX := Vel_X;
        VY := Vel_Y;
    end Get_Data;

    --  ------------------------------------------------------------------------

    procedure Initialize_Grid is
        Half_Height  : constant single := single (Grid_Height) / 2.0;
        Half_Width   : constant single := single (Grid_Width) / 2.0;
        dx           : single;
        dy           : single;
        d            : single;
    begin
        for y_index in 1 .. Grid_Height loop
            for x_index in 1 .. Grid_Width loop
                dx := single (x_index) - Half_Width;
                dy := single (y_index) - Half_Height;
                d := Single_Functions.Sqrt (dx * dx + dy * dy);
                if d < 0.1 * Half_Width then
                    d := 10.0 * d;
                    Pressure (x_index, y_index) := -100.0 *
                      Single_Functions.Cos (d * Ada.Numerics.Pi / single (4 * Grid_Width));
                else
                    Pressure (x_index, y_index) := 0.0;
                end if;
                Vel_X (x_index, y_index) := 0.0;
                Vel_Y (x_index, y_index) := 0.0;
            end loop;
        end loop;

    exception
        when others =>
            Put_Line ("An exception occurred in Initialize_Grid.");
            raise;
    end Initialize_Grid;

    --  ----------------------------------------------------------------------------
    --  Iniialize_Vertices places the vertices in a grid
    procedure Initialize_Vertices is
      use Maths;
        Half_Height  : constant single := single (Grid_Height) / 2.0;
        Half_Width   : constant single := single (Grid_Width) / 2.0;
        V_Point      : Int;
        Q_Point      : Int := 0;
        Vym1_GW      : Int;
        Qym1_GW      : Int;
        Qy_GW        : Int;
    begin
        for y_index in Int range 1 .. Grid_Height loop
            Vym1_GW := (y_index - 1) * Grid_Width;
            for x_index in Int range 1 .. Grid_Width loop
                V_Point := Vym1_GW + x_index;
                Vertex_Buffer_Data (V_Point) (X) := single (x_index) - Half_Width / single (Half_Width);
                Vertex_Buffer_Data (V_Point) (Y) := single (y_index) - Half_Width / single (Half_Height);
                Vertex_Buffer_Data (V_Point) (Z) := 0.0;

                if (x_index mod 4 < 2) and then (y_index mod 4 < 2) then
                    Vertex_Buffer_Data (V_Point) (R) := 0.0;
                else
                    Vertex_Buffer_Data (V_Point) (R) := 0.0;
                end if;
                Vertex_Buffer_Data (V_Point) (G) := single (y_index) / single (Grid_Height);
                Vertex_Buffer_Data (V_Point) (B) := 1.0 - single (x_index) / single (Grid_Width) +
                  single (y_index) / single (Grid_Height) / 2.0;
--                  Put_Line ("Initialize_Vertices, x, y, V_Point: " & Int'Image (x_index)
--                   & ",  " & Int'Image (y_index) & ",  " & Int'Image (V_Point));
--                  Put_Line ("Initialize_Vertices,V_Point X, Y: " & Int'image (V_Point)
--                            & ",  " &  single'Image (Vertex_Buffer_Data (V_Point) (X))
--                             & ",  " &  single'Image (Vertex_Buffer_Data (V_Point) (Y)));
            end loop;
        end loop;

        for y_index in Int range  1 .. Int (Quad_Height) loop
            Qym1_GW := (y_index - 1) * Int (Grid_Width);
            Qy_GW := y_index * Int (Grid_Width);
            for x_index in Int range 1 .. Int (Quad_Width) loop
                Q_Point := 6 * (Qym1_GW + x_index) - 5;
                --  Four vertices of a quadralateral:
                --  First triangle
                Quad_Element_Array (Q_Point) := Qym1_GW + x_index;          --  a point
                Quad_Element_Array (Q_Point + 1) := Qym1_GW + x_index + 1;  --  right side neighbour
                Quad_Element_Array (Q_Point + 2) := Qy_GW + x_index;    --  upper right neighbour
                --  Second Triangle
                Quad_Element_Array (Q_Point + 3) := Quad_Element_Array (Q_Point + 1);
                Quad_Element_Array (Q_Point + 4) := Qy_GW + x_index + 1;
                Quad_Element_Array (Q_Point + 5) := Qy_GW + x_index;        --  upper neighbour
            end loop;
        end loop;

    exception
        when others =>
            Put_Line ("An exception occurred in Initialize_Vertices.");
            raise;
    end Initialize_Vertices;

    --  ----------------------------------------------------------------------------

    procedure Propogate_Wave (dt : single) is
        Animation_Speed : constant single := 10.0;
        Time_Step       : single := dt * Animation_Speed;
        Acc_X           : Grid_Array;
        Acc_Y           : Grid_Array;
        x2              : Int;
        y2              : Int;
    begin
        for x_index in 0 .. Grid_Width - 1 loop
            x2 := (x_index + 1) mod Grid_Width;
            for y_index in 0 .. Grid_Height - 1 loop
                Acc_X (x_index, y_index) := Pressure (x_index, y_index) - Pressure (x2, y_index);
            end loop;
        end loop;

        for y_index in 0 .. Grid_Height - 1 loop
            y2 := (y_index + 1) mod Grid_Height;
            for x_index in 0 .. Grid_Width - 1 loop
                Acc_Y (x_index, y_index) := Pressure (x_index, y_index) - Pressure (x_index, y2);
            end loop;
        end loop;

        for x_index in 0 .. Grid_Width - 1 loop
            x2 := (x_index + 1) mod Grid_Width;
            for y_index in 0 .. Grid_Height - 1 loop
                Vel_X (x_index, y_index) := Vel_X (x_index, y_index) + Acc_X (x_index, y_index) * Time_Step;
                Vel_Y (x_index, y_index) := Vel_Y (x_index, y_index) + Acc_Y (x_index, y_index) * Time_Step;
            end loop;
        end loop;

        for x_index in 0 .. Grid_Width - 1 loop
            x2 := x_index - 1;
            for y_index in 0 .. Grid_Height - 1 loop
                y2 := y_index - 1;
                Pressure (x_index, y_index) := Pressure (x_index, y_index) +
                  (Vel_X (x2, y_index) - Vel_X (x_index, y_index) -
                   Vel_Y (x_index, y2) - Vel_Y (x_index, y_index)) * Time_Step;
            end loop;
        end loop;

    exception
        when others =>
            Put_Line ("An exception occurred in Propogate_Wave.");
            raise;
    end Propogate_Wave;

    --  ----------------------------------------------------------------------------

end Vertex_Data;
