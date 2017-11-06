
with GL.Types;

with Glfw.Windows;

package Controls is
    Procedure Compute_Matrices_From_Inputs (Window : in out Glfw.Windows.Window;
           Projection_Matrix, View_Matrix : in out GL.Types.Singles.Matrix4);
end Controls;
