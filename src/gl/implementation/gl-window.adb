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

with GL.API;
with GL.Enums.Getter;

package body GL.Window is

   procedure Set_Viewport (X, Y : Int; Width, Height : Size) is
   begin
      GL.API.Viewport (X, Y, Width, Height);
      Raise_Exception_On_OpenGL_Error;
   end Set_Viewport;

   procedure Get_Viewport (X, Y : out Int; Width, Height : out Size) is
      Ret : Ints.Vector4;
   begin
      API.Get_Int_Vec4 (Enums.Getter.Viewport, Ret);
      Raise_Exception_On_OpenGL_Error;
      X := Ret (GL.X);
      Y := Ret (GL.Y);
      Width := Size (Ret (Z));
      Height := Size (Ret (W));
   end Get_Viewport;

   procedure Set_Depth_Range (Near, Far : Double) is
   begin
      API.Depth_Range (Near, Far);
      Raise_Exception_On_OpenGL_Error;
   end Set_Depth_Range;

   procedure Get_Depth_Range (Near, Far : out Double) is
      Ret : Doubles.Vector2;
   begin
      API.Get_Double_Vec2 (Enums.Getter.Depth_Range, Ret);
      Raise_Exception_On_OpenGL_Error;
      Near := Ret (X);
      Far := Ret (Y);
   end Get_Depth_Range;

end GL.Window;
