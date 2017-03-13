
with GL.Types;

with Glfw.Windows;

package Controls is
    Procedure Compute_Matrices_From_Inputs (Window : in out Glfw.Windows.Window);
    function Get_Projection_Matrix return GL.Types.Singles.Matrix4;
    function Get_View_Matrix return GL.Types.Singles.Matrix4;
end Controls;
