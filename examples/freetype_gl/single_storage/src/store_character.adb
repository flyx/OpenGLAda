--  Program Store_Character
--  Author Roger Mc Murtrie
--  Created 22 September 2017

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Store_Character is
   Main_Window : Glfw.Windows.Window;
   Window_Title : constant String := "Single Freetype Character Using Texture Storage";
begin
   Glfw.Init;
   Initialize (Main_Window, Window_Title);
   Main_Loop (Main_Window);
   Glfw.Shutdown;

exception
   when anError : Constraint_Error =>
      Put ("Store_Character returned a constraint error: ");
      Put_Line (Exception_Information (anError));

   when anError :  others =>
      Put_Line ("An exception occurred in Store_Character.");
      Put_Line (Exception_Information (anError));
end Store_Character;
