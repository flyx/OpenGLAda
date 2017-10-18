--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.Display;
with Glfw.Display.Modes;
with Glfw.Events;
with Glfw.Events.Keys;

with Ada.Text_IO;

procedure Glfw_Test.Display is

   procedure Write_Mode (Subject : Glfw.Display.Modes.Mode) is
   begin
      Ada.Text_IO.Put (Subject.Width'Img & " x" & Subject.Height'Img);
      Ada.Text_IO.Put ("; RGB:" & Subject.Red_Bits'Img & " +"
                       & Subject.Green_Bits'Img & " +"
                       & Subject.Blue_Bits'Img);
      Ada.Text_IO.New_Line;
   end Write_Mode;

begin
   Glfw.Init;

   Glfw.Display.Open (Mode => Glfw.Display.Window);

   Glfw.Events.Keys.Set_Key_Callback (Glfw_Test.Key_To_Title'Access);

   declare
      Mode_List : Glfw.Display.Modes.Mode_List := Glfw.Display.Modes.Available_Modes;
      Desktop_Mode : Glfw.Display.Modes.Mode := Glfw.Display.Modes.Desktop_Mode;
   begin
     Ada.Text_IO.Put_Line ("---- Video Modes ----");
     for Index in Mode_List'Range loop
         Write_Mode (Mode_List (Index));
      end loop;

      Ada.Text_IO.Put ("Current: ");
      Write_Mode (Desktop_Mode);
   end;

   while Glfw.Display.Opened loop
      Glfw.Events.Wait_For_Events;
   end loop;

   Glfw.Terminate_Glfw;
end Glfw_Test.Display;
