--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types;

private with GL.Low_Level;

package GL.Rasterization is
   pragma Preelaborate;

   use GL.Types;

   subtype Line_Width_Range is Singles.Vector2;

   type Polygon_Mode_Type is (Point, Line, Fill);

   procedure Set_Line_Width (Value : Single);
   function Line_Width return Single;

   function Aliased_Line_Width_Range return Line_Width_Range;
   function Smooth_Line_Width_Range  return Line_Width_Range;
   function Smooth_Line_Width_Granularity return Single;

   procedure Set_Polygon_Mode (Value : Polygon_Mode_Type);
   function Polygon_Mode return Polygon_Mode_Type;

   procedure Set_Point_Size (Value : Single);
   function Point_Size return Single;
   function Point_Size_Range return Singles.Vector2;
   function Point_Size_Granularity return Single;

   procedure Set_Point_Fade_Threshold_Size (Value : Single);
   function Point_Fade_Threshold_Size return Single;
private
   for Polygon_Mode_Type use (Point => 16#1B00#,
                              Line => 16#1B01#,
                              Fill => 16#1B02#);
   for Polygon_Mode_Type'Size use Low_Level.Enum'Size;
end GL.Rasterization;
