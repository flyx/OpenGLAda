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

with Glfw.Api;


package body Glfw.Display.Modes is

   function Available_Modes return Mode_List is
      Max_Modes  : Integer := 32;
      Mode_Count : Integer;
   begin
      loop
         declare
            Raw_Modes : Api.Video_Mode_List (1 .. Max_Modes);
         begin
            Mode_Count := Integer (Api.Get_Video_Modes (Raw_Modes (1) (1)'Address,
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
      Raw_Mode : aliased Api.Raw_Video_Mode;
   begin
      Api.Get_Desktop_Mode (Raw_Mode (1)'Access);
      return Mode'(Width      => Natural (Raw_Mode (1)),
                   Height     => Natural (Raw_Mode (2)),
                   Red_Bits   => Natural (Raw_Mode (3)),
                   Green_Bits => Natural (Raw_Mode (4)),
                   Blue_Bits  => Natural (Raw_Mode (5)));
   end Desktop_Mode;

end Glfw.Display.Modes;
