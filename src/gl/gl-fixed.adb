--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

with GL.Enums;
with GL.API;

-- Fixed function pipeline. Deprecated in OpenGL 3.0.
package body GL.Fixed is
   procedure Set_Vertex_Pointer (Length : Vertex_Length;
                                 Stride, Offset : Natural) is
      
   begin
      API.Vertex_Pointer (Low_Level.Int (Length), Common.Double_Type,
                          Low_Level.SizeI (Stride), Low_Level.Int (Offset));
      Check_OpenGL_Error;
   end Set_Vertex_Pointer;
   
   procedure Set_Index_Pointer  (Index_Type : Common.Numeric_Type;
                                 Stride, Offset : Natural) is
   begin
      API.Index_Pointer (Index_Type, Low_Level.SizeI (Stride),
                         Low_Level.Int (Offset));
      Check_OpenGL_Error;
   end Set_Index_Pointer;
   
   procedure Set_Color_Pointer  (Stride : Natural) is
   begin
      API.Color_Pointer (4, Common.Float_Type, Low_Level.SizeI (Stride), 0);
      Check_OpenGL_Error;
   end Set_Color_Pointer;
   
   procedure Draw_Elements (Mode : Connection_Mode; Count : Natural;
                            Index_Type : Common.Unsigned_Numeric_Type) is
   begin
      API.Draw_Elements (Mode, Low_Level.SizeI (Count), Index_Type, 0);
      Check_OpenGL_Error;
   end Draw_Elements;

   procedure Enable (Capability : Client_Side_Capability) is
   begin
      API.Enable_Client_State (Capability);
      Check_OpenGL_Error;
   end Enable;
   
   procedure Disable (Capability : Client_Side_Capability) is
   begin
      API.Disable_Client_State (Capability);
      Check_OpenGL_Error;
   end Disable;
   
   procedure Draw_Arrays (Mode : Connection_Mode; First, Count : Natural) is
   begin
      API.Draw_Arrays (Mode, Low_Level.Int (First), Low_Level.SizeI (Count));
      Check_OpenGL_Error;
   end Draw_Arrays;
end GL.Fixed;