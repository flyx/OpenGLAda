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

with GL.Toggles;
with GL.Low_Level;

private package GL.Enums is
   pragma Preelaborate;

   type Attribute_Mask is record
      Depth_Buffer   : Boolean;
      Stencil_Buffer : Boolean;
      Color_Buffer   : Boolean;
   end record;

   type Blending_Factor_Destination is (Zero, One, Src_Color, One_Minus_Src_Color,
                                        Src_Alpha, One_Minus_Src_Alpha, Dst_Alpha,
                                        One_Minus_Dst_Alpha);

   type Blending_Factor_Source is (Dst_Color, One_Minus_Dst_Color,
                                   Src_Alpha_Saturate);

   type Front_Face_Direction is (CW, CCW);

   type Matrix_Mode is (Modelview, Projection, Texture, Color);
   
   type Light_Model_Parameter is (Local_Viewer, Two_Side,
                                  Ambient, Color_Control);
   
   for Light_Model_Parameter use (Local_Viewer  => 16#0B51#,
                                  Two_Side      => 16#0B52#,
                                  Ambient       => 16#0B53#,
                                  Color_Control => 16#81F8#);
   for Light_Model_Parameter'Size use Low_Level.Enum'Size;                               
   
   subtype Light_Model_Ambient_Parameter is Light_Model_Parameter range
     Ambient .. Ambient;
   
   subtype Light_Model_CC_Parameter is Light_Model_Parameter range
     Color_Control .. Color_Control;
   
   subtype Light_Model_Toggle_Parameter is Light_Model_Parameter range
     Local_Viewer .. Two_Side;
   
   type Light_Param is (Ambient, Diffuse, Specular, Position,
                        Spot_Direction, Spot_Exponent,Spot_Cutoff,
                        Constant_Attenduation, Linear_Attenduation,
                        Quadratic_Attenduation);
   
   subtype Light_Name is Toggles.Toggle range Toggles.Light0 .. Toggles.Light7;
   
   type Shader_Param is (Shader_Type, Delete_Status, Compile_Status,
                         Info_Log_Length, Shader_Source_Length);
   
   type Program_Param is (Program_Binary_Length, Geometry_Vertices_Out,
                          Geometry_Input_Type, Geometry_Output_Type,
                          Active_Uniform_Block_Max_Name_Length,
                          Active_Uniform_Blocks, Delete_Status,
                          Link_Status, Validate_Status, Info_Log_Length,
                          Attached_Shaders, Active_Uniforms,
                          Active_Uniform_Max_Length, Active_Attributes, 
                          Active_Attribute_Max_Length,
                          Transform_Feedback_Varying_Max_Length,
                          Transform_Feedback_Buffer_Mode,
                          Transform_Feedback_Varyings);
   
   type Program_Stage_Param is (Active_Subroutines, Active_Subroutine_Uniforms,
                                Active_Subroutine_Uniform_Locations,
                                Active_Subroutine_Max_Length,
                                Active_Subroutine_Uniform_Max_Length);

   type Clamp_Color_Param is (Clamp_Read_Color);
   
   type Pixel_Store_Param is (Unpack_Swap_Bytes, Unpack_LSB_First,
                              Unpack_Row_Length, Unpack_Skip_Rows,
                              Unpack_Skip_Pixels, Unpack_Alignment,
                              Pack_Swap_Bytes, Pack_LSB_First, Pack_Row_Length,
                              Pack_Skip_Rows, Pack_Skip_Pixels,
                              Pack_Alignment, Pack_Skip_Images, Pack_Image_Height,
                              Unpack_Skip_Images, Unpack_Image_Height);
   
   type Buffer_Param is (Buffer_Access, Buffer_Mapped,
                         Buffer_Size, Buffer_Usage);
   
   type Buffer_Pointer_Param is (Buffer_Map_Pointer);
   
   type Framebuffer_Param is (Default_Width, Default_Height, Default_Layers,
                              Default_Samples, Default_Fixed_Sample_Locations);
private

   for Attribute_Mask use record
      Depth_Buffer   at 1 range 0 .. 0;
      Stencil_Buffer at 1 range 2 .. 2;
      Color_Buffer   at 1 range 6 .. 6;
   end record;
   for Attribute_Mask'Size use Low_Level.Bitfield'Size;

   for Blending_Factor_Destination use (Zero                => 0,
                                        One                 => 1,
                                        Src_Color           => 16#0300#,
                                        One_Minus_Src_Color => 16#0301#,
                                        Src_Alpha           => 16#0302#,
                                        One_Minus_Src_Alpha => 16#0303#,
                                        Dst_Alpha           => 16#0304#,
                                        One_Minus_Dst_Alpha => 16#0305#);
   for Blending_Factor_Destination'Size use Low_Level.Enum'Size;

   for Blending_Factor_Source use (Dst_Color           => 16#0306#,
                                   One_Minus_Dst_Color => 16#0307#,
                                   Src_Alpha_Saturate  => 16#0308#);
   for Blending_Factor_Source'Size use Low_Level.Enum'Size;

   for Front_Face_Direction use (CW  => 16#0900#,
                                 CCW => 16#0901#);
   for Front_Face_Direction'Size use Low_Level.Enum'Size;

   for Matrix_Mode use (Modelview  => 16#1700#,
                        Projection => 16#1701#,
                        Texture    => 16#1702#,
                        Color      => 16#1800#);
   for Matrix_Mode'Size use Low_Level.Enum'Size;
   
   for Light_Param use (Ambient        => 16#1200#,
                        Diffuse        => 16#1201#,
                        Specular       => 16#1202#,
                        Position       => 16#1203#,
                        Spot_Direction => 16#1204#,
                        Spot_Exponent  => 16#1205#,
                        Spot_Cutoff    => 16#1206#,
                        Constant_Attenduation  => 16#1207#,
                        Linear_Attenduation    => 16#1208#,
                        Quadratic_Attenduation => 16#1209#);
   for Light_Param'Size use Low_Level.Enum'Size;
   
   for Shader_Param use (Shader_Type          => 16#8B4F#,
                         Delete_Status        => 16#8B80#,
                         Compile_Status       => 16#8B81#,
                         Info_Log_Length      => 16#8B84#,
                         Shader_Source_Length => 16#8B88#);
   for Shader_Param'Size use Low_Level.Enum'Size;
   
   for Program_Param use (Program_Binary_Length       => 16#8741#,
                          Geometry_Vertices_Out       => 16#8916#,
                          Geometry_Input_Type         => 16#8917#,
                          Geometry_Output_Type        => 16#8918#,
                          Active_Uniform_Block_Max_Name_Length  => 16#8A35#,
                          Active_Uniform_Blocks                 => 16#8A36#,
                          Delete_Status               => 16#8B4F#,
                          Link_Status                 => 16#8B82#,
                          Validate_Status             => 16#8B83#,
                          Info_Log_Length             => 16#8B84#,
                          Attached_Shaders            => 16#8B85#,
                          Active_Uniforms             => 16#8B86#,
                          Active_Uniform_Max_Length   => 16#8B87#,
                          Active_Attributes           => 16#8B89#,
                          Active_Attribute_Max_Length => 16#8B8A#,
                          Transform_Feedback_Varying_Max_Length => 16#8C76#,
                          Transform_Feedback_Buffer_Mode        => 16#8C7F#,
                          Transform_Feedback_Varyings           => 16#8C83#);
   for Program_Param'Size use Low_Level.Enum'Size;
                                
   for Program_Stage_Param use (Active_Subroutines => 16#8DE5#,
                                Active_Subroutine_Uniforms => 16#8DE6#,
                                Active_Subroutine_Uniform_Locations => 16#8E47#,
                                Active_Subroutine_Max_Length => 16#8E48#,
                                Active_Subroutine_Uniform_Max_Length => 16#8E49#);
   for Program_Stage_Param'Size use Low_Level.Enum'Size;
   
   for Clamp_Color_Param use (Clamp_Read_Color => 16#891C#);
   for Clamp_Color_Param'Size use Low_Level.Enum'Size;
   
   for Pixel_Store_Param use (Unpack_Swap_Bytes   => 16#0CF0#,
                              Unpack_LSB_First    => 16#0CF1#,
                              Unpack_Row_Length   => 16#0CF2#,
                              Unpack_Skip_Rows    => 16#0CF3#,
                              Unpack_Skip_Pixels  => 16#0CF4#,
                              Unpack_Alignment    => 16#0CF5#,
                              Pack_Swap_Bytes     => 16#0D00#,
                              Pack_LSB_First      => 16#0D01#,
                              Pack_Row_Length     => 16#0D02#,
                              Pack_Skip_Rows      => 16#0D03#,
                              Pack_Skip_Pixels    => 16#0D04#,
                              Pack_Alignment      => 16#0D05#,
                              Pack_Skip_Images    => 16#806B#,
                              Pack_Image_Height   => 16#806C#,
                              Unpack_Skip_Images  => 16#806D#,
                              Unpack_Image_Height => 16#806E#);
   for Pixel_Store_Param'Size use Low_Level.Enum'Size;
   
   for Buffer_Param use (Buffer_Access => 16#88BB#,
                         Buffer_Mapped => 16#88BC#,
                         Buffer_Size   => 16#88BD#,
                         Buffer_Usage  => 16#88BE#);
   for Buffer_Param'Size use Low_Level.Enum'Size;
   
   for Buffer_Pointer_Param use (Buffer_Map_Pointer => 16#88BD#);
   for Buffer_Pointer_Param'Size use Low_Level.Enum'Size;
   
   for Framebuffer_Param use (Default_Width                  => 16#9310#,
                              Default_Height                 => 16#9311#,
                              Default_Layers                 => 16#9312#,
                              Default_Samples                => 16#9313#,
                              Default_Fixed_Sample_Locations => 16#9314#);
   for Framebuffer_Param'Size use Low_Level.Enum'Size;
end GL.Enums;
