--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

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

   procedure Set_Point_Size (Value : Single) is
   begin
      API.Set_Point_Size (Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Point_Size;

   function Point_Size return Single is
      Ret : aliased Single := 0.0;
   begin
      API.Get_Single (Enums.Getter.Point_Size, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Point_Size;

   function Point_Size_Range return Singles.Vector2 is
      Ret : Singles.Vector2 := Singles.Vector2'(0.0, 0.0);
   begin
      API.Get_Single_Vec2 (Enums.Getter.Point_Size_Range, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Point_Size_Range;

   function Point_Size_Granularity return Single is
      Ret : aliased Single := 0.0;
   begin
      API.Get_Single (Enums.Getter.Point_Size_Granularity, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Point_Size_Granularity;

   procedure Set_Point_Fade_Threshold_Size (Value : Single) is
   begin
      API.Set_Point_Parameter_Single (Enums.Fade_Threshold_Size, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Point_Fade_Threshold_Size;

   function Point_Fade_Threshold_Size return Single is
      Ret : aliased Single := 0.0;
   begin
      API.Get_Single (Enums.Getter.Point_Fade_Threshold_Size, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Point_Fade_Threshold_Size;

end GL.Rasterization;
