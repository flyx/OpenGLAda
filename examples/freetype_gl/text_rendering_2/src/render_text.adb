--  Program Render Text Example
--  Author: Roger Mc Murtrie
--  28 October 2017
--  This example demonstrates use of the OpenGL.Text API

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Render_Text is
   Main_Window  : Glfw.Windows.Window;
   Window_Title : constant String := "Render Text Example";
begin
   Glfw.Init;
   Initialize (Main_Window, Window_Title);
   Main_Loop (Main_Window);
   Glfw.Shutdown;

exception
   when anError : Constraint_Error =>
      Put_Line ("Render_Text returned a constraint error: ");
      Put_Line (Exception_Information (anError));

   when anError :  others =>
      Put_Line ("An exception occurred in Render_Text.");
      Put_Line (Exception_Information (anError));

end Render_Text;
