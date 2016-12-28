
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;

with Window_Types;

package Initialization is

    function Initialize (Main_Window               : in out Window_Types.tWindow;
                         Rendering_Program         : out GL.Objects.Programs.Program;
                         Vertex_Array              : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                         return Boolean;

end Initialization;
