
with GL.Types;

with Glfw.Windows;

with Maths;
with Vertex_Data;

package Control_Wave is

    type Settings is private;
    type Data is private;

    procedure Check_Input (Window : in out Glfw.Windows.Window);
    procedure Get_Settings (A, B : out Maths.Degree; Z : out GL.Types.Single);

private
    use Maths;
    type Settings is record
        Alpha    : Degree := -Degrees (210.0);
        Beta     : Degree := -Degrees (70.0);
        Zoom     : GL.Types.Single := 2.0;
    end record;

    type Data is record
        Pressure : Vertex_Data.Grid_Array;
        Vel_X    : Vertex_Data.Grid_Array;
        Vel_Y    : Vertex_Data.Grid_Array;
    end record;

end Control_Wave;
