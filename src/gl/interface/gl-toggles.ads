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

private with GL.Low_Level;

package GL.Toggles is
   pragma Preelaborate;
   
   type Toggle_State is (Disabled, Enabled);
   
   type Toggle is (Point_Smooth, Line_Smooth, Line_Stipple, Polygon_Smooth, Polygon_Stipple,
                   Cull_Face, Lighting, Color_Material,
                   Fog, Depth_Test, Stencil_Test, Normalize, Alpha_Test, Dither,
                   Blend, Index_Logic_Op, Color_Logic_Op, Scissor_Test, Texture_Gen_S,
                   Texture_Gen_T, Texture_Gen_R, Texture_Gen_Q,
                   Auto_Normal,
                   Map1_Color_4, Map1_Index,
                   Map1_Normal, Map1_Texture_Coord_1, Map1_Texture_Coord_2,
                   Map1_Texture_Coord_3, Map1_Texture_Coord_4,
                   Map1_Vertex_3, Map1_Vertex_4, Map2_Color_4, Map2_Index, 
                   Map2_Normal, Map2_Texture_Coord_1, Map2_Texture_Coord_2,
                   Map2_Texture_Coord_3, Map2_Texture_Coord_4, Map2_Vertex_3,
                   Map2_Vertex_4, Texture_1D, Texture_2D, Polygon_Offset_Point, Polygon_Offset_Line,
                   Clip_Plane_0, Clip_Plane_1,
                   Clip_Plane_2, Clip_Plane_3, Clip_Plane_4, Clip_Plane_5,
                   Light0, Light1, Light2, Light3, Light4, Light5, Light6, Light7,
                   Convolution_1D, Convolution_2D, Separable_2D, Histogram,
                   Minmax, Polygon_Offset_Fill, Rescale_Normal, Texture_3D, Multisample,
                   Sample_Alpha_To_Coverage, Sample_Alpha_To_One,
                   Sample_Coverage, Color_Table, Post_Convolution_Color_Table,
                   Post_Color_Matrix_Color_Table, Color_Sum, Texture_Cube_Map,
                   Vertex_Program_Point_Size, Vertex_Program_Two_Side, Point_Sprite);
   
   procedure Enable (Subject : Toggle);
   procedure Disable (Subject : Toggle);
   procedure Set (Subject : Toggle; Value : Toggle_State);
   function State (Subject : Toggle) return Toggle_State;
private
   for Toggle use (Point_Smooth             => 16#0B10#,
                   Line_Smooth              => 16#0B20#,
                   Line_Stipple             => 16#0B24#,
                   Polygon_Smooth           => 16#0B41#,
                   Polygon_Stipple          => 16#0B42#,
                   Cull_Face                => 16#0B44#,
                   Lighting                 => 16#0B50#,
                   Color_Material           => 16#0B57#,
                   Fog                      => 16#0B60#,
                   Depth_Test               => 16#0B71#,
                   Stencil_Test             => 16#0B90#,
                   Normalize                => 16#0BA1#,
                   Alpha_Test               => 16#0BC0#,
                   Dither                   => 16#0BD0#,
                   Blend                    => 16#0BE2#,
                   Index_Logic_Op           => 16#0BF1#,
                   Color_Logic_Op           => 16#0BF2#,
                   Scissor_Test             => 16#0C11#,
                   Texture_Gen_S            => 16#0C60#,
                   Texture_Gen_T            => 16#0C61#,
                   Texture_Gen_R            => 16#0C62#,
                   Texture_Gen_Q            => 16#0C63#,
                   Auto_Normal              => 16#0D80#,
                   Map1_Color_4             => 16#0D90#,
                   Map1_Index               => 16#0D91#,
                   Map1_Normal              => 16#0D92#,
                   Map1_Texture_Coord_1     => 16#0D93#,
                   Map1_Texture_Coord_2     => 16#0D94#,
                   Map1_Texture_Coord_3     => 16#0D95#,
                   Map1_Texture_Coord_4     => 16#0D96#,
                   Map1_Vertex_3            => 16#0D97#,
                   Map1_Vertex_4            => 16#0D98#,
                   Map2_Color_4             => 16#0DB0#,
                   Map2_Index               => 16#0DB1#,
                   Map2_Normal              => 16#0DB2#,
                   Map2_Texture_Coord_1     => 16#0DB3#,
                   Map2_Texture_Coord_2     => 16#0DB4#,
                   Map2_Texture_Coord_3     => 16#0DB5#,
                   Map2_Texture_Coord_4     => 16#0DB6#,
                   Map2_Vertex_3            => 16#0DB7#,
                   Map2_Vertex_4            => 16#0DB8#,
                   Texture_1D               => 16#0DE0#,
                   Texture_2D               => 16#0DE1#,
                   Polygon_Offset_Point     => 16#2A01#,
                   Polygon_Offset_Line      => 16#2A02#,
                   Clip_Plane_0             => 16#3000#,
                   Clip_Plane_1             => 16#3001#,
                   Clip_Plane_2             => 16#3002#,
                   Clip_Plane_3             => 16#3003#,
                   Clip_Plane_4             => 16#3004#,
                   Clip_Plane_5             => 16#3005#,
                   Light0                   => 16#4000#,
                   Light1                   => 16#4001#,
                   Light2                   => 16#4002#,
                   Light3                   => 16#4003#,
                   Light4                   => 16#4004#,
                   Light5                   => 16#4005#,
                   Light6                   => 16#4006#,
                   Light7                   => 16#4007#,
                   Convolution_1D           => 16#8010#,
                   Convolution_2D           => 16#8011#,
                   Separable_2D             => 16#8012#,
                   Histogram                => 16#8024#,
                   Minmax                   => 16#802E#,
                   Polygon_Offset_Fill      => 16#8037#,
                   Rescale_Normal           => 16#803A#,
                   Texture_3D               => 16#806F#,
                   Multisample              => 16#809D#,
                   Sample_Alpha_To_Coverage => 16#809E#,
                   Sample_Alpha_To_One      => 16#809F#,
                   Sample_Coverage          => 16#80A0#,
                   Color_Table              => 16#80D0#,
                   Post_Convolution_Color_Table  => 16#80D1#,
                   Post_Color_Matrix_Color_Table => 16#80D2#,
                   Color_Sum                => 16#8458#,
                   Texture_Cube_Map         => 16#8513#,
                   Vertex_Program_Point_Size=> 16#8642#,
                   Vertex_Program_Two_Side  => 16#8643#,
                   Point_Sprite             => 16#8861#);
   for Toggle'Size use Low_Level.Enum'Size;
   
end GL.Toggles;