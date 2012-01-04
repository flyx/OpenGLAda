--------------------------------------------------------------------------------
--  Copyright (c) 2011, Felix Krause
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED.
--  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
--  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY OUT OF THE USE OF
--  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------

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
