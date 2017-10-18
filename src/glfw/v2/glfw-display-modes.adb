--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.API;


package body Glfw.Display.Modes is

   function Available_Modes return Mode_List is
      Max_Modes  : Integer := 32;
      Mode_Count : Integer;
   begin
      loop
         declare
            Raw_Modes : API.Video_Mode_List (1 .. Max_Modes);
         begin
            Mode_Count := Integer (API.Get_Video_Modes (Raw_Modes (1) (1)'Address,
              C.int (Max_Modes)));

            if (Mode_Count < Max_Modes) then
               declare
                  Return_List : Mode_List (1 .. Mode_Count);
               begin
                  for Index in Return_List'Range loop
                     Return_List (Index)
                       := Mode'(Width      => Natural (Raw_Modes (Index) (1)),
                                Height     => Natural (Raw_Modes (Index) (2)),
                                Red_Bits   => Natural (Raw_Modes (Index) (3)),
                                Green_Bits => Natural (Raw_Modes (Index) (4)),
                                Blue_Bits  => Natural (Raw_Modes (Index) (5)));
                  end loop;
                  return Return_List;
               end;
            end if;
         end;

         Max_Modes := Max_Modes * 2;
      end loop;
   end Available_Modes;


   function Desktop_Mode return Mode is
      Raw_Mode : aliased API.Raw_Video_Mode;
   begin
      API.Get_Desktop_Mode (Raw_Mode (1)'Access);
      return Mode'(Width      => Natural (Raw_Mode (1)),
                   Height     => Natural (Raw_Mode (2)),
                   Red_Bits   => Natural (Raw_Mode (3)),
                   Green_Bits => Natural (Raw_Mode (4)),
                   Blue_Bits  => Natural (Raw_Mode (5)));
   end Desktop_Mode;

end Glfw.Display.Modes;
