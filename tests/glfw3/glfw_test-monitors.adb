--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Glfw.Monitors;
with Ada.Text_IO;

procedure Glfw_Test.Monitors is

   procedure Print_Gamma_Value_Array (Values : Glfw.Monitors.Gamma_Value_Array) is
      use Ada.Text_IO;
      First : Boolean := True;
   begin
      Put ("[");
      for I in Values'Range loop
         if First then
            First := False;
         else
            Put (",");
         end if;
         Put (Values (I)'Img);
      end loop;
      Put_Line ("]");
   end Print_Gamma_Value_Array;

   procedure Print_Monitor_Info (M : Glfw.Monitors.Monitor) is
      use Ada.Text_IO;
      use type Glfw.Monitors.Video_Mode;
      Param1, Param2 : Integer;
      VM_List : Glfw.Monitors.Video_Mode_List := M.Video_Modes;
      Gamma_Ramp : Glfw.Monitors.Gamma_Ramp := M.Current_Gamma_Ramp;
   begin
      Put_Line ("Monitor """ & M.Name & """");
      M.Get_Position (Param1, Param2);
      Put_Line ("Position: (" & Param1'Img & "," & Param2'Img & ")");
      M.Get_Physical_Size (Param1, Param2);
      Put_Line ("Dimensions: " & Param1'Img & " x " & Param2'Img);
      Put_Line ("Video modes: ");
      for I in VM_List'Range loop
         Put ("  [");
         if VM_List (I) = M.Current_Video_Mode then
            Put ("x");
         else
            Put (" ");
         end if;
         Put ("] dim(" & VM_List (I).Width'Img & " x" & VM_List (I).Height'Img);
         Put ("), rgb(" & VM_List (I).Red_Bits'Img & "," &
                VM_List (I).Green_Bits'Img & "," & VM_List (I).Blue_Bits'Img);
         Put_Line ("), refresh(" & VM_List (I).Refresh_Rate'Img & ")");
      end loop;
      Put_Line ("Gamma ramp:");
      Put ("  red:   ");
      Print_Gamma_Value_Array (Gamma_Ramp.Red);
      Put ("  green: ");
      Print_Gamma_Value_Array (Gamma_Ramp.Green);
      Put ("  blue:  ");
      Print_Gamma_Value_Array (Gamma_Ramp.Blue);
   end Print_Monitor_Info;

begin
   Glfw.Init;
   Enable_Print_Errors;
   declare
      My_Monitors : Glfw.Monitors.Monitor_List := Glfw.Monitors.Monitors;
   begin
      for I in My_Monitors'Range loop
         Print_Monitor_Info (My_Monitors (I));
         Ada.Text_IO.Put_Line ("--------------------------------------------");
      end loop;
   end;
   Glfw.Shutdown;
end Glfw_Test.Monitors;
