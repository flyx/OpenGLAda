--------------------------------------------------------------------------------
-- Copyright (c) 2014, Felix Krause <contact@flyx.org>
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
with GL.Culling;
with GL.Enums.Getter;

package body GL.Rasterization is

   procedure Set_Line_Width (Value : Single) is
   begin
      API.Line_Width (Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Line_Width;

   function Line_Width return Single is
      Ret : aliased Single;
   begin
      API.Get_Single (Enums.Getter.Line_Width, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Line_Width;

   function Aliased_Line_Width_Range return Line_Width_Range is
      Ret : Singles.Vector2 := (others => <>);
   begin
      API.Get_Single_Vec2 (Enums.Getter.Aliased_Line_Width_Range, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Aliased_Line_Width_Range;

   function Smooth_Line_Width_Range  return Line_Width_Range is
      Ret : Singles.Vector2 := (others => <>);
   begin
      API.Get_Single_Vec2 (Enums.Getter.Smooth_Line_Width_Range, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Smooth_Line_Width_Range;

   function Smooth_Line_Width_Granularity return Single is
      Ret : aliased Single;
   begin
      API.Get_Single (Enums.Getter.Smooth_Line_Width_Granularity, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Smooth_Line_Width_Granularity;

   procedure Set_Polygon_Mode (Value : Polygon_Mode_Type) is
   begin
      API.Polygon_Mode (Culling.Front_And_Back, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Polygon_Mode;

   function Polygon_Mode return Polygon_Mode_Type is
      Ret : aliased Polygon_Mode_Type;
   begin
      API.Get_Polygon_Mode (Enums.Getter.Polygon_Mode, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Polygon_Mode;

end GL.Rasterization;
