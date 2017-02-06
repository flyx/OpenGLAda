--  Program  Colour Fragments
--  Author Roger Mc Murtrie
--  Created 17 December 2016

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Colour_Fragments is
    Main_Window : Glfw.Windows.Window;
begin
    Glfw.Init;
    Initialize (Main_Window,"OpenGL SupeBible - Fragment Colouring Example");
    Main_Loop (Main_Window);
    Glfw.Shutdown;

 exception
      when anError : Constraint_Error =>
         Put("Colour_Fragments returned constraint error: ");
         Put_Line(Exception_Information(anError));

      when anError :  others =>
         Put_Line("An exceptiom occurred in Colour_Fragments.");
         Put_Line(Exception_Information(anError));
  end Colour_Fragments;
