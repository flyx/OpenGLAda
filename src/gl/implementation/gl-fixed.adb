--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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

with GL.API;

-- Fixed function pipeline. Deprecated in OpenGL 3.0.
package body GL.Fixed is
   procedure Set_Vertex_Pointer (Length : Vertex_Length;
                                 Stride, Offset : Size) is
      
   begin
      API.Vertex_Pointer (Int (Length), Double_Type,
                          Stride, Offset);
      Raise_Exception_On_OpenGL_Error;
   end Set_Vertex_Pointer;
   
   procedure Set_Color_Pointer  (Stride, Offset : Size) is
   begin
      API.Color_Pointer (4, Single_Type, Stride,
                         Int (Offset));
      Raise_Exception_On_OpenGL_Error;
   end Set_Color_Pointer;

   procedure Enable (Capability : Client_Side_Capability) is
   begin
      API.Enable_Client_State (Capability);
      Raise_Exception_On_OpenGL_Error;
   end Enable;
   
   procedure Disable (Capability : Client_Side_Capability) is
   begin
      API.Disable_Client_State (Capability);
      Raise_Exception_On_OpenGL_Error;
   end Disable;
end GL.Fixed;
