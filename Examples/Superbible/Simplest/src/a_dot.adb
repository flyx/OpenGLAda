-- Program A_Dot
-- Author Roger Mc Murtrie
-- Created 16 December 2016

with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;

with glfw;

with Initialization;
with Run;
with Utilities;
with Window_Types;

procedure A_Dot is
    Main_Window               : Window_Types.tWindow;
    Rendering_Program         : GL.Objects.Programs.Program;
    Vertex_Array              : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
begin
    glfw.Init;
    if Initialization.Initialize (Main_Window, Rendering_Program, Vertex_Array) then
        Run.Main_Loop (Main_Window, Rendering_Program);
    end if;

    Vertex_Array.Delete_Id;
    Rendering_Program.Delete_Id;
    glfw.Shutdown;

 exception
      when anError : Constraint_Error =>
         Put ("Application returned constraint error: ");
         Put_Line (Exception_Information (anError));

      when anError :  others =>
         Put_Line ("An exceptiom occurred in Application.");
         Put_Line (Exception_Information (anError));
end A_Dot;
