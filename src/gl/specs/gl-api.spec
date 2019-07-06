with GL.Attributes;
with GL.Blending;
with GL.Buffers;
with GL.Culling;
with GL.Enums.Getter;
with GL.Enums.Textures;
with GL.Errors;
with GL.Fixed.Textures;
with GL.Fixed.Lighting;
with GL.Framebuffer;
with GL.Low_Level.Enums;
with GL.Objects.Textures;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Pixels;
with GL.Rasterization;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;

with Interfaces.C.Strings;

with System;

spec GL.API is
   use GL.Types;

   function Get_Error return Errors.Error_Code is Static ("glGetError");
   procedure Flush with Static => "glFlush", Wrapper => "GL.Flush";
   procedure Finish with Static => "glFinish", Wrapper => "GL.Finish";

   -----------------------------------------------------------------------------
   --                           Parameter Getters                             --
   -----------------------------------------------------------------------------

   procedure Get_Boolean (Name   : Enums.Getter.Parameter;
                          Target : access Low_Level.Bool) with
     Static => "glGetBooleanv";
   procedure Get_Double (Name   : Enums.Getter.Parameter;
                         Target : access Double) with
     Static => "glGetDoublev";
   procedure Get_Double_Vec2 (Name   : Enums.Getter.Parameter;
                              Target : in out Doubles.Vector2) with
     Static => "glGetDoublev";
   procedure Get_Single (Name   : Enums.Getter.Parameter;
                         Target : access Single) with
     Static => "glGetFloatv";
   procedure Get_Single_Vec2 (Name   : Enums.Getter.Parameter;
                              Target : in out Singles.Vector2) with
     Static => "glGetFloatv";
   procedure Get_Color (Name   : Enums.Getter.Parameter;
                        Target : in out Colors.Color) with
     Static => "glGetFloatv";
   procedure Get_Integer (Name   : Enums.Getter.Parameter;
                          Target : access Int) with Static => "glGetIntegerv";
   procedure Get_Int_Vec4 (Name   : Enums.Getter.Parameter;
                           Target : in out Ints.Vector4) with
     Static => "glGetIntegerv";
   procedure Get_Unsigned_Integer (Name   : Enums.Getter.Parameter;
                                   Target : access UInt) with
     Static => "glGetIntegerv";
   procedure Get_Size (Name   : Enums.Getter.Parameter;
                       Target : access Size) with
     Static => "glGetIntegerv";
   procedure Get_Color_Control (Name   : Enums.Getter.Parameter;
                                Target : access Fixed.Lighting.Color_Control)
     with Static => "glGetIntegerv";
   procedure Get_Shade_Model (Name   : Enums.Getter.Parameter;
                              Target : access Fixed.Lighting.Shade_Model) with
     Static => "glGetIntegerv";
   procedure Get_Blend_Factor (Name : Enums.Getter.Parameter;
                               Target : access Blending.Blend_Factor) with
     Static => "glGetIntegerv";
   procedure Get_Alignment (Name : Enums.Getter.Parameter;
                            Target : access Pixels.Alignment) with
     Static => "glGetIntegerv";
   procedure Get_Blend_Equation (Name : Enums.Getter.Parameter;
                                 Target : access Blending.Equation) with
     Static => "glGetIntegerv";
   procedure Get_Compare_Function (Name : Enums.Getter.Parameter;
                                   Target : access Compare_Function) with
     Static => "glGetIntegerv";
   procedure Get_Orientation (Name : Enums.Getter.Parameter;
                              Target : access Orientation) with
     Static => "glGetIntegerv";
   procedure Get_Face_Selector (Name : Enums.Getter.Parameter;
                                Target : access Culling.Face_Selector) with
     Static => "glGetIntegerv";
   procedure Get_Polygon_Mode (Name   : Enums.Getter.Parameter;
                               Target : access Rasterization.Polygon_Mode_Type)
     with Static => "glGetIntegerv";
   procedure Get_Logic_Op (Name : Enums.Getter.Parameter;
                           Target : access Framebuffer.Logic_Op) with
     Static => "glGetIntegerv";
   procedure Get_Stencil_Action (Name : Enums.Getter.Parameter;
                                 Target : access Buffers.Stencil_Action) with
     Static => "glGetIntegerv";
   procedure Get_Read_Buffer_Selector
     (Name   : Enums.Getter.Parameter;
      Target : access Framebuffer.Read_Buffer_Selector) with
     Static => "glGetIntegerv";
   procedure Get_Light_Color (Name   : Enums.Light_Name;
                              Pname  : Enums.Light_Param;
                              Target : in out Colors.Color) with
     Static => "glGetLightfv",
     Wrapper => "GL.Fixed.Lighting.Ambient",
     Wrapper => "GL.Fixed.Lighting.Diffuse",
     Wrapper => "GL.Fixed.Lighting.Specular";
   function Get_String (Name : Enums.Getter.String_Parameter)
                        return C.Strings.chars_ptr is Static ("glGetString");
   function Get_String_I (Name  : Enums.Getter.String_Parameter;
                          Index : UInt) return C.Strings.chars_ptr is
     Dynamic ("glGetStringi");

   -----------------------------------------------------------------------------
   --                                 Toggles                                 --
   -----------------------------------------------------------------------------

   procedure Enable (Subject : Toggles.Toggle) with
     Static => "glEnable", Wrapper => "GL.Toggles.Enable";
   procedure Disable (Subject : Toggles.Toggle) with
     Static => "glDisable", Wrapper => "GL.Toggles.Disable";
   function Is_Enabled (Subject : Toggles.Toggle) return Low_Level.Bool with
     Static => "glIsEnabled", Wrapper => "GL.Toggles.State";

   -----------------------------------------------------------------------------
   --                                 Culling                                 --
   -----------------------------------------------------------------------------

   procedure Cull_Face (Selector : Culling.Face_Selector) with
     Static => "glCullFace", Wrapper => "GL.Culling.Set_Cull_Face";
   procedure Front_Face (Face : Orientation) with
     Static => "glFrontFace", Wrapper => "GL.Culling.Set_Front_Face";

   -----------------------------------------------------------------------------
   --                               Pixel Stuff                               --
   -----------------------------------------------------------------------------

   procedure Pixel_Store (Param : Enums.Pixel_Store_Param;
                          Value : Low_Level.Bool) with
     Static  => "glPixelStorei",
     Wrapper => "GL.Pixels.Set_Pack_Swap_Bytes",
     Wrapper => "GL.Pixels.Set_Pack_LSB_First";
   procedure Pixel_Store (Param : Enums.Pixel_Store_Param;
                          Value : Size) with
     Static  => "glPixelStorei",
     Wrapper => "GL.Pixels.Set_Pack_Row_Length",
     Wrapper => "GL.Pixels.Set_Pack_Image_Height",
     Wrapper => "GL.Pixels.Set_Pack_Skip_Pixels",
     Wrapper => "GL.Pixels.Set_Pack_Skip_Rows",
     Wrapper => "GL.Pixels.Set_Pack_Skip_Images";
   procedure Pixel_Store (Param : Enums.Pixel_Store_Param;
                          Value : Pixels.Alignment) with
     Static => "glPixelStorei", Wrapper => "GL.Pixels.Set_Pack_Alignment";

   -----------------------------------------------------------------------------
   --            Matrix stack API (deprecated as of OpenGL 3.0)               --
   -----------------------------------------------------------------------------

   procedure Matrix_Mode (Mode : Enums.Matrix_Mode) with
     Static => "glMatrixMode";
   procedure Frustum (Left, Right, Bottom, Top, zNear, zFar : Double) with
     Static => "glFrustum", Wrapper => "GL.Fixed.Matrix.Apply_Frustum";
   procedure Ortho (Left, Right, Bottom, Top, zNear, zFar : Double) with
     Static => "glOrtho", Wrapper => "GL.Fixed.Matrix.Apply_Orthogonal";
   procedure Load_Identity with
     Static => "glLoadIdentity", Wrapper => "GL.Fixed.Matrix.Load_Identity";
   procedure Push_Matrix with
     Static => "glPushMatrix", Wrapper => "GL.Fixed.Matrix.Push";
   procedure Pop_Matrix with
     Static => "glPopMatrix", Wrapper => "GL.Fixed.Matrix.Pop";
   procedure Rotate (Angle, X, Y, Z : Double) with
     Static => "glRotated", Wrapper => "GL.Fixed.Matrix.Apply_Rotation";
   procedure Scale (X, Y, Z : Double) with
     Static => "glScaled", Wrapper => "GL.Fixed.Matrix.Apply_Scaling";
   procedure Translate (X, Y, Z : Double) with
     Static => "glTranslated", Wrapper => "GL.Fixed.Matrix.Apply_Translation";

   -----------------------------------------------------------------------------
   --              Immediate API (deprecated as of OpenGL 3.0)                --
   -----------------------------------------------------------------------------

   procedure GL_Begin (Mode : Connection_Mode) with
     Static => "glBegin", Wrapper => "GL.Immediate.Start";
   procedure GL_End with Static => "glEnd";
   procedure Color (Value : Colors.Color) with
     Static => "glColor4fv", Wrapper => "GL.Immediate.Set_Color";
   procedure Secondary_Color (Value : Colors.Color) with
     Dynamic => "glSecondaryColor3dv",
     Wrapper => "GL.Immediate.Set_Secondary_Color";
   procedure Fog_Coord (Value : Double) with
     Dynamic => "glFogCoordd", Wrapper => "GL.Immediate.Set_Fog_Distance";

   -----------------------------------------------------------------------------
   --        Fixed Function Pipeline (deprecated as of OpenGL 3.0)            --
   -----------------------------------------------------------------------------

   procedure Vertex_Pointer (Count     : Int;
                             Data_Type : Signed_Numeric_Type;
                             Stride    : Size;
                             Pointer   : Int) with
     Static => "glVertexPointer", Wrapper => "GL.Fixed.Set_Vertex_Pointer";
   procedure Index_Pointer (Data_Type : Signed_Numeric_Type;
                            Stride    : Size;
                            Pointer   : Int) with Static => "glIndexPointer";
   procedure Color_Pointer (Count     : Int;
                            Data_Type : Signed_Numeric_Type;
                            Stride    : Size;
                            Pointer   : Int) with
     Static => "glColorPointer", Wrapper => "GL.Fixed.Set_Color_Pointer";
   procedure Enable_Client_State (Cap : Fixed.Client_Side_Capability) with
     Static => "glEnableClientState", Wrapper => "GL.Fixed.Enable";
   procedure Disable_Client_State (Cap : Fixed.Client_Side_Capability) with
     Static => "glDisableClientState", Wrapper => "GL.Fixed.Disable";
   procedure Draw_Arrays (Mode  : Connection_Mode;
                          First : Int; Count : Size) with
     Static  => "glDrawArrays",
     Wrapper => "GL.Objects.Vertex_Arrays.Draw_Arrays";
   procedure Draw_Arrays_Instanced (Mode  : Connection_Mode;
     First : Size; Count : Size; Instances : Size) with
     Dynamic => "glDrawArraysInstanced",
     Wrapper => "GL.Objects.Vertex_Arrays.Draw_Arrays_Instanced";
   procedure Draw_Elements (Mode       : Connection_Mode;
                            Count      : Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Indices    : Low_Level.IntPtr) with
     Static => "glDrawElements", Wrapper => "GL.Objects.Buffers.Draw_Elements";
   procedure Draw_Elements_Instanced (Mode       : Connection_Mode;
                                      Count      : Size;
                                      Index_Type : Unsigned_Numeric_Type;
                                      Indices    : Low_Level.IntPtr;
                                      Instances  : Size) with
     Dynamic => "glDrawElementsInstanced", Wrapper => "GL.Objects.Buffers.Draw_Elements_Instanced";
   procedure Draw_Elements_Base_Vertex (Mode           : Connection_Mode; 
                                        Count          : UInt;
                                        Index_Type     : Unsigned_Numeric_Type;
                                        Element_Offset : UInt;
                                        Base_Vertex    : Int) with
     Dynamic => "glDrawElementsBaseVertex", Wrapper => "GL.Objects.Buffers.Draw_Elements_Base_Vertex";
   procedure Primitive_Restart_Index (Index : UInt) with
     Dynamic => "glPrimitiveRestartIndex",
     Wrapper => "GL.Objects.Vertex_Arrays.Set_Primitive_Restart_Index";
   procedure Vertex_Attrib_Divisor (Index   : GL.Attributes.Attribute;
                                    Divisor : UInt) with
     Dynamic => "glVertexAttribDivisor", Wrapper => "GL.Attributes.Vertex_Attrib_Divisor";
   procedure Load_Matrix (Value : Types.Doubles.Matrix4) with
     Static => "glLoadMatrixd", Wrapper => "GL.Fixed.Matrix.Load_Matrix";
   procedure Mult_Matrix (Factor : Types.Doubles.Matrix4) with
     Static  => "glMultMatrixd",
     Wrapper => "GL.Fixed.Matrix.Apply_Multiplication";
   procedure Vertex4 (Value : Types.Doubles.Vector4) with
     Static => "glVertex4dv", Wrapper => "GL.Immediate.Add_Vertex";
   procedure Vertex3 (Value : Types.Doubles.Vector3) with
     Static => "glVertex3dv", Wrapper => "GL.Immediate.Add_Vertex";
   procedure Vertex2 (Value : Types.Doubles.Vector2) with
     Static => "glVertex2dv", Wrapper => "GL.Immediate.Add_Vertex";
   procedure Normal (Value : Types.Doubles.Vector3) with
     Static => "glNormal3dv", Wrapper => "GL.Immediate.Set_Normal";
   procedure Tex_Coord4 (Value : Types.Doubles.Vector4) with
     Static  => "glTexCoord4dv",
     Wrapper => "GL.Immediate.Set_Texture_Coordinates";
   procedure Tex_Coord3 (Value : Types.Doubles.Vector3) with
     Static  => "glTexCoord3dv",
     Wrapper => "GL.Immediate.Set_Texture_Coordinates";
   procedure Tex_Coord2 (Value : Types.Doubles.Vector2) with
     Static  => "glTexCoord2dv",
     Wrapper => "GL.Immediate.Set_Texture_Coordinates";

   -----------------------------------------------------------------------------
   --               Lighting API (deprecated as of OpenGL 3.0)                --
   -----------------------------------------------------------------------------

   procedure Light_Model_Color (Param : Enums.Light_Model_Ambient_Parameter;
                                Color : Colors.Color) with
     Static  => "glLightModelfv",
     Wrapper => "GL.Fixed.Lighting.Set_Global_Ambient_Light";
   procedure Light_Model_Color_Control (Param : Enums.Light_Model_CC_Parameter;
     Value : access constant Fixed.Lighting.Color_Control) with
     Static  => "glLightModeliv",
     Wrapper => "GL.Fixed.Lighting.Set_Color_Control";
   procedure Light_Model_Toggles (Param : Enums.Light_Model_Toggle_Parameter;
                                  Value : access constant Int) with
     Static  => "glLightModeliv",
     Wrapper => "GL.Fixed.Lighting.Enable_Local_Viewer",
     Wrapper => "GL.Fixed.Lighting.Disable_Local_Viewer",
     Wrapper => "GL.Fixed.Lighting.Enable_Two_Side",
     Wrapper => "GL.Fixed.Lighting.Disable_Two_Side";
   procedure Shade_Model (Mode : Fixed.Lighting.Shade_Model) with
     Static => "glShadeModel", Wrapper => "GL.Fixed.Lighting.Set_Shade_Model";
   procedure Light_Color (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
                          Param : Colors.Color) with
     Static  => "glLightfv",
     Wrapper => "GL.Fixed.Lighting.Set_Ambient",
     Wrapper => "GL.Fixed.Lighting.Set_Diffuse",
     Wrapper => "GL.Fixed.Lighting.Set_Specular";
   procedure Get_Light_Position
     (Name   : Enums.Light_Name; Pname : Enums.Light_Param;
      Target : in out Types.Singles.Vector4) with
     Static  => "glGetLightfv", Wrapper => "GL.Fixed.Lighting.Position";
   procedure Get_Light_Direction
     (Name   : Enums.Light_Name; Pname : Enums.Light_Param;
      Target : in out Types.Singles.Vector3) with
     Static  => "glGetLightfv", Wrapper => "GL.Fixed.Lighting.Spot_Direction";
   procedure Light_Position
     (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
      Param : Types.Singles.Vector4) with
     Static  => "glLightfv", Wrapper => "GL.Fixed.Lighting.Set_Position";
   procedure Light_Direction
     (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
      Param : Types.Singles.Vector3) with
     Static => "glLightfv", Wrapper => "GL.Fixed.Lighting.Set_Spot_Direction";

   -----------------------------------------------------------------------------
   --                               Blending                                  --
   -----------------------------------------------------------------------------

   procedure Blend_Func (Src_Factor, Dst_Factor : Blending.Blend_Factor) with
     Static => "glBlendFunc", Wrapper => "GL.Blending.Set_Blend_Func";
   procedure Blend_Func_I (Buf : Buffers.Draw_Buffer_Index;
     Src_Factor, Dst_Factor : Blending.Blend_Factor) with
     Dynamic => "glBlendFunci", Wrapper => "GL.Blending.Set_Blend_Func";
   procedure Blend_Func_Separate
     (Src_Rgb, Dst_Rgb, Src_Alpha, Dst_Alpha : Blending.Blend_Factor) with
     Dynamic => "glBlendFuncSeparate",
     Wrapper => "GL.Blending.Set_Blend_Func_Separate";
   procedure Blend_Func_Separate_I (Buf : Buffers.Draw_Buffer_Index;
     Src_Rgb, Dst_Rgb, Src_Alpha, Dst_Alpha : Blending.Blend_Factor) with
     Dynamic => "glBlendFuncSeparate",
     Wrapper => "GL.Blending.Set_Blend_Func_Separate";
   procedure Blend_Color (Red, Green, Blue, Alpha : Colors.Component) with
     Dynamic => "glBlendColor",
     Wrapper => "GL.Blending.Set_Blend_Color";
   procedure Blend_Equation (Mode : Blending.Equation) with
     Dynamic => "glBlendEquation",
     Wrapper => "GL.Blending.Set_Blend_Equation";
   procedure Blend_Equation_I (Buf : Buffers.Draw_Buffer_Index;
                               Mode : Blending.Equation) with
     Dynamic => "glBlendEquationi",
     Wrapper => "GL.Blending.Set_Blend_Equation";
   procedure Blend_Equation_Separate (Mode_Rgb, Mode_Alpha : Blending.Equation)
     with Dynamic => "glBlendEquationSeparate",
          Wrapper => "GL.Blending.Set_Blend_Equation_Separate";
   procedure Blend_Equation_Separate_I (Buf : Buffers.Draw_Buffer_Index;
     Mode_Rgb, Mode_Alpha : Blending.Equation) with
     Dynamic => "glBlendEquationi",
     Wrapper => "GL.Blending.Set_Blend_Equation_Separate";

   -----------------------------------------------------------------------------
   --                             Rasterization                               --
   -----------------------------------------------------------------------------

   procedure Line_Width (Value : Single) with
     Static => "glLineWidth", Wrapper => "GL.Rasterization.Set_Line_Width";
   procedure Polygon_Mode (Face : Culling.Face_Selector;
                           Value : Rasterization.Polygon_Mode_Type) with
     Static => "glPolygonMode", Wrapper => "GL.Rasterization.Set_Polygon_Mode";
   procedure Set_Point_Size (Value : Single) with
     Static => "glPointSize", Wrapper => "GL.Rasterization.Set_Point_Size";
   procedure Set_Point_Parameter_Single (Pname : Enums.Point_Param;
     Param : Single) with
     Dynamic => "glPointParameterf",
     Wrapper => "GL.Rasterization.Set_Point_Fade_Threshold_Size";
   procedure Raster_Pos4 (Value : Types.Doubles.Vector4) with
     Static => "glRasterPos4dv", Wrapper => "GL.Raster.Set_Pos";
   procedure Raster_Pos3 (Value : Types.Doubles.Vector3) with
     Static => "glRasterPos3dv", Wrapper => "GL.Raster.Set_Pos";
   procedure Raster_Pos2 (Value : Types.Doubles.Vector2) with
     Static => "glRasterPos2dv", Wrapper => "GL.Raster.Set_Pos";

   -----------------------------------------------------------------------------
   --                                Buffers                                  --
   -----------------------------------------------------------------------------

   procedure Clear (Bits : Low_Level.Bitfield) with
     Static => "glClear", Wrapper => "GL.Buffers.Clear";
   procedure Draw_Buffer (Mode : Buffers.Explicit_Color_Buffer_Selector) with
     Static => "glDrawBuffer", Wrapper => "GL.Buffers.Set_Active_Buffer";
   procedure Draw_Buffers (N : UInt; Bufs : Buffers.Explicit_Color_Buffer_List)
     with Dynamic => "glDrawBuffers",
          Wrapper => "GL.Buffers.Set_Active_Buffers";
   procedure Clear_Color (Red, Green, Blue, Alpha : Colors.Component) with
     Static => "glClearColor", Wrapper => "GL.Buffers.Set_Color_Clear_Value";
   procedure Clear_Depth (Depth : Buffers.Depth) with
     Static => "glClearDepth", Wrapper => "GL.Buffers.Set_Depth_Clear_Value";
   procedure Clear_Stencil (Index : Buffers.Stencil_Index) with
     Static  => "glClearStencil",
     Wrapper => "GL.Buffers.Set_Stencil_Clear_Value";

   -- dropped in OpenGL 3
   procedure Clear_Accum (Red, Green, Blue, Alpha : Colors.Component) with
     Dynamic => "glClearAccum", Wrapper => "GL.Buffers.Set_Accum_Clear_Value";

   procedure Clear_Buffer (Buffer : Buffers.Color_Buffer_Selector;
                           Drawbuffer : Low_Level.Zero; Value : Colors.Color)
     with Dynamic => "glClearBufferfv",
          Wrapper => "GL.Buffers.Clear_Color_Buffers";
   procedure Clear_Draw_Buffer
     (Buffer : Low_Level.Enums.Only_Color_Buffer;
      Drawbuffer : Buffers.Draw_Buffer_Index; Value : Colors.Color) with
     Dynamic => "glClearBufferfv", Wrapper => "GL.Buffers.Clear_Draw_Buffer";
   procedure Clear_Buffer_Depth
     (Buffer : Low_Level.Enums.Only_Depth_Buffer; Drawbuffer : Low_Level.Zero;
      Value : access constant Buffers.Depth) with
     Dynamic => "glClearBufferfv", Wrapper => "GL.Buffers.Clear_Depth_Buffer";
   procedure Clear_Buffer_Stencil
     (Buffer : Low_Level.Enums.Only_Stencil_Buffer; Drawbuffer : Low_Level.Zero;
      Value : access constant Buffers.Stencil_Index) with
     Dynamic => "glClearBufferiv", Wrapper => "GL.Buffers.Clear_Stencil_Buffer";
   procedure Clear_Buffer_Depth_Stencil
     (Buffer : Low_Level.Enums.Only_Depth_Stencil_Buffer;
      Drawbuffer : Low_Level.Zero;
      Depth : Buffers.Depth; Stencil : Buffers.Stencil_Index) with
     Dynamic => "glClearBufferfi",
     Wrapper => "GL.Buffers.Clear_Depth_And_Stencil_Buffer";

   -----------------------------------------------------------------------------
   --                        Depth And Stencil Buffers                        --
   -----------------------------------------------------------------------------

   procedure Depth_Mask (Value : Low_Level.Bool) with
     Static => "glDepthMask", Wrapper => "GL.Buffers.Depth_Mask";
   procedure Depth_Func (Func : Compare_Function) with
     Static => "glDepthFunc", Wrapper => "GL.Buffers.Set_Depth_Function";
   procedure Stencil_Func_Separate (Face : Culling.Face_Selector;
     Func : Compare_Function; Ref : Int; Mask : UInt) with
     Dynamic => "glStencilFuncSeparate",
     Wrapper => "GL.Buffers.Set_Stencil_Function";
   procedure Stencil_Op_Separate  (Face : Culling.Face_Selector;
     S_Fail, Dp_Fail, Dp_Pass : Buffers.Stencil_Action) with
     Dynamic => "glStencilOpSeparate",
     Wrapper => "GL.Buffers.Set_Stencil_Operation";
   procedure Stencil_Mask_Separate (Face : Culling.Face_Selector;
     Mask : UInt) with
     Dynamic => "glStencilMaskSeparate",
     Wrapper => "GL.Buffers.Set_Stencil_Mask";

   -----------------------------------------------------------------------------
   --                                Textures                                 --
   -----------------------------------------------------------------------------

   procedure Tex_Parameter_Float (Target     : Low_Level.Enums.Texture_Kind;
                                  Param_Name : Enums.Textures.Parameter;
                                  Value      : Single) with
     Static  => "glTexParameterf",
     Wrapper => "GL.Objects.Textures.Set_Minimum_LoD",
     Wrapper => "GL.Objects.Textures.Set_Maximum_LoD",
     Wrapper => "GL.Objects.Textures.Set_Texture_Priority";
   procedure Tex_Parameter_Int (Target     : Low_Level.Enums.Texture_Kind;
                                Param_Name : Enums.Textures.Parameter;
                                Value      : Int) with
     Static  => "glTexParameteri",
     Wrapper => "GL.Objects.Textures.Set_Lowest_Mipmap_Level",
     Wrapper => "GL.Objects.Textures.Set_Highest_Mipmap_Level";
   procedure Tex_Parameter_Min_Filter
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Value      : Objects.Textures.Minifying_Function) with
     Static  => "glTexParameteri",
     Wrapper => "GL.Objects.Textures.Set_Minifying_Filter";
   procedure Tex_Parameter_Mag_Filter
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Value      : Objects.Textures.Magnifying_Function) with
     Static => "glTexParameteri",
     Wrapper => "GL.Objects.Textures.Set_Magnifying_Filter";
   procedure Tex_Parameter_Wrap_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Value      : Objects.Textures.Wrapping_Mode) with
     Static  => "glTexParameteri",
     Wrapper => "GL.Objects.Textures.Set_X_Wrapping",
     Wrapper => "GL.Objects.Textures.Set_Y_Wrapping",
     Wrapper => "GL.Objects.Textures.Set_Z_Wrapping";
   procedure Tex_Parameter_Comp_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Value      : Enums.Textures.Compare_Kind) with
     Static  => "glTexParameteri",
     Wrapper => "GL.Objects.Textures.Toggle_Compare_X_To_Texture";
   procedure Tex_Parameter_Comp_Func (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : Compare_Function) with
     Static  => "glTexParameteri",
     Wrapper => "GL.Objects.Textures.Set_Compare_Function";
   procedure Tex_Parameter_Depth_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Value      : Objects.Textures.Depth_Mode) with
     Static  => "glTexParameteri",
     Wrapper => "GL.Objects.Textures.Set_Depth_Texture_Mode";
   procedure Tex_Parameter_Bool (Target     : Low_Level.Enums.Texture_Kind;
                                 Param_Name : Enums.Textures.Parameter;
                                 Value      : Low_Level.Bool) with
     Static  => "glTexParameteri",
     Wrapper => "GL.Objects.Textures.Toggle_Mipmap_Autoupdate";
   procedure Tex_Parameter_Floats (Target     : Low_Level.Enums.Texture_Kind;
                                   Param_Name : Enums.Textures.Parameter;
                                   Values     : Low_Level.Single_Array) with
     Static  => "glTexParameterfv",
     Wrapper => "GL.Objects.Textures.Set_Border_Color";
   procedure Get_Tex_Parameter_Float (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : out Single) with
     Static  => "glGetTexParameterfv",
     Wrapper => "GL.Objects.Textures.Minimum_LoD",
     Wrapper => "GL.Objects.Textures.Maximum_LoD",
     Wrapper => "GL.Objects.Textures.Texture_Priority";
   procedure Get_Tex_Parameter_Floats
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Values     : in out Low_Level.Single_Array) with
     Static  => "glGetTexParameterfv",
     Wrapper => "GL.Objects.Textures.Border_Color";
   procedure Get_Tex_Parameter_Int (Target     : Low_Level.Enums.Texture_Kind;
                                    Param_Name : Enums.Textures.Parameter;
                                    Values     : out Int) with
     Static  => "glGetTexParameteriv",
     Wrapper => "GL.Objects.Textures.Minifying_Filter",
     Wrapper => "GL.Objects.Textures.Magnifying_Filter",
     Wrapper => "GL.Objects.Textures.Lowest_Mipmap_Level",
     Wrapper => "GL.Objects.Textures.Highest_Mipmap_Level";
   procedure Get_Tex_Parameter_Ints (Target     : Low_Level.Enums.Texture_Kind;
                                     Param_Name : Enums.Textures.Parameter;
                                     Values     : in out Low_Level.Int_Array)
     with Static => "glGetTexParameteriv";
   procedure Get_Tex_Parameter_Wrap_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Values     : out Objects.Textures.Wrapping_Mode) with
     Static  => "glGetTexParameteriv",
     Wrapper => "GL.Objects.Textures.X_Wrapping",
     Wrapper => "GL.Objects.Textures.Y_Wrapping",
     Wrapper => "GL.Objects.Textures.Z_Wrapping";
   procedure Get_Tex_Parameter_Comp_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Values     : out Enums.Textures.Compare_Kind) with
     Static  => "glGetTexParameteriv",
     Wrapper => "GL.Objects.Textures.Compare_X_To_Texture_Enabled";
   procedure Get_Tex_Parameter_Comp_Func
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Values     : out Compare_Function) with
     Static  => "glGetTexParameteriv",
     Wrapper => "GL.Objects.Textures.Current_Compare_Function";
   procedure Get_Tex_Parameter_Depth_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Values     : out Objects.Textures.Depth_Mode) with
     Static  => "glGetTexParameteriv",
     Wrapper => "GL.Objects.Textures.Depth_Texture_Mode";
   procedure Get_Tex_Parameter_Bool (Target     : Low_Level.Enums.Texture_Kind;
                                     Param_Name : Enums.Textures.Parameter;
                                     Values     : out Low_Level.Bool) with
     Static  => "glGetTexParameteriv",
     Wrapper => "GL.Objects.Textures.Mipmap_Autoupdate_Enabled";
   procedure Get_Tex_Level_Parameter_Size
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Size) with
     Static  => "glGetTexLevelParameteriv",
     Wrapper => "GL.Objects.Textures.Width",
     Wrapper => "GL.Objects.Textures.Height",
     Wrapper => "GL.Objects.Textures.Depth",
     Wrapper => "GL.Objects.Textures.Red_Size",
     Wrapper => "GL.Objects.Textures.Green_Size",
     Wrapper => "GL.Objects.Textures.Blue_Size",
     Wrapper => "GL.Objects.Textures.Alpha_Size",
     Wrapper => "GL.Objects.Textures.Depth_Size",
     Wrapper => "GL.Objects.Textures.Compressed_Image_Size";
   procedure Get_Tex_Level_Parameter_Format
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Pixels.Internal_Format) with
     Static => "glGetTexLevelParameteriv",
     Wrapper => "GL.Objects.Textures.Format";
   procedure Get_Tex_Level_Parameter_Type
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Pixels.Channel_Data_Type) with
     Static  => "glGetTexLevelParameteriv",
     Wrapper => "GL.Objects.Textures.Red_Type",
     Wrapper => "GL.Objects.Textures.Green_Type",
     Wrapper => "GL.Objects.Textures.Blue_Type",
     Wrapper => "GL.Objects.Textures.Alpha_Type",
     Wrapper => "GL.Objects.Textures.Depth_Type";
   procedure Get_Tex_Level_Parameter_Bool
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Low_Level.Bool) with
     Static  => "glGetTexLevelParameteriv",
     Wrapper => "GL.Objects.Textures.Compressed";
   procedure Gen_Textures (N : Size; Textures : out UInt) with
     Static => "glGenTextures", Wrapper => "GL.Objects.Initialize_Id";

   procedure Bind_Texture (Target  : Low_Level.Enums.Texture_Kind;
                           Texture : UInt) with
     Static => "glBindTexture", Wrapper => "GL.Objects.Textures.Bind";
   procedure Delete_Textures (N : Size; Textures : Low_Level.UInt_Array) with
     Static => "glDeleteTextures";
   function Is_Texture (Texture : UInt) return Boolean with
     Static => "glIsTexture", Wrapper => "GL.Objects.Textures.Is_Texture";
   procedure Tex_Image_1D (Target : Low_Level.Enums.Texture_Kind;
                           Level  : Objects.Textures.Mipmap_Level;
                           Internal_Format : Pixels.Internal_Format;
                           Width  : Size;
                           Border : Low_Level.Zero;
                           Format : Pixels.Data_Format;
                           Data_Type : Pixels.Data_Type;
                           Data   : Objects.Textures.Image_Source) with
     Static  => "glTexImage1D",
     Wrapper => "GL.Objects.Textures.With_1D_Loader.Load_Empty_Texture",
     Wrapper => "GL.Objects.Textures.With_1D_Loader.Load_From_Data";
   procedure Compressed_Tex_Image_1D
     (Target          : Low_Level.Enums.Texture_Kind;
      Level           : Objects.Textures.Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width           : Size;
      Border          : Low_Level.Zero;
      Image_Size      : Size;
      Data            : Objects.Textures.Image_Source) with
     Dynamic => "glCompressedTexImage1D",
     Wrapper => "GL.Objects.Textures.With_1D_Loader.Load_Compressed";
   procedure Tex_Sub_Image_1D (Target : Low_Level.Enums.Texture_Kind;
     Level  : Objects.Textures.Mipmap_Level;
     X_Offset, Y_Offset :Int;
     Width, Height : Size;
     Format : Pixels.Data_Format;
     Data_Type : Pixels.Data_Type;
     Data : Objects.Textures.Image_Source) with
     Dynamic => "glTexSubImage1D",
     Wrapper => "GL.Objects.Textures.With_1D_Loader.Load_Sub_Image_From_Data";
   procedure Tex_Storage_1D
     (Target : Low_Level.Enums.Texture_Kind; Level : Objects.Textures.Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width : Size) with
     Dynamic => "glTexStorage1D",
     Wrapper => "GL.Objects.Textures.With_1D_Loader.Storage";
   procedure Tex_Image_2D (Target : Low_Level.Enums.Texture_Kind;
                           Level  : Objects.Textures.Mipmap_Level;
                           Internal_Format : Pixels.Internal_Format;
                           Width, Height : Size;
                           Border : Low_Level.Zero;
                           Format : Pixels.Data_Format;
                           Data_Type : Pixels.Data_Type;
                           Data : Objects.Textures.Image_Source) with
     Static  => "glTexImage2D",
     Wrapper => "GL.Objects.Textures.With_2D_Loader.Load_Empty_Texture",
     Wrapper => "GL.Objects.Textures.With_2D_Loader.Load_From_Data";
   procedure Compressed_Tex_Image_2D
     (Target          : Low_Level.Enums.Texture_Kind;
      Level           : Objects.Textures.Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width, Height   : Size;
      Border          : Low_Level.Zero;
      Image_Size      : Size;
      Data            : Objects.Textures.Image_Source) with
     Dynamic => "glCompressedTexImage2D",
     Wrapper => "GL.Objects.Textures.With_2D_Loader.Load_Compressed";
   procedure Tex_Sub_Image_2D (Target : Low_Level.Enums.Texture_Kind;
     Level  : Objects.Textures.Mipmap_Level;
     X_Offset, Y_Offset :Int;
     Width, Height : Size;
     Format : Pixels.Data_Format;
     Data_Type : Pixels.Data_Type;
     Data : Objects.Textures.Image_Source) with
     Static  => "glTexSubImage2D",
     Wrapper => "GL.Objects.Textures.With_2D_Loader.Load_Sub_Image_From_Data";
   procedure Tex_Storage_2D
     (Target : Low_Level.Enums.Texture_Kind; Level : Objects.Textures.Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width, Height : Size) with
     Dynamic => "glTexStorage2D",
     Wrapper => "GL.Objects.Textures.With_2D_Loader.Storage";
   procedure Tex_Image_3D (Target : Low_Level.Enums.Texture_Kind;
                           Level  : Objects.Textures.Mipmap_Level;
                           Internal_Format : Pixels.Internal_Format;
                           Width, Height, Depth : Size;
                           Border : Low_Level.Zero;
                           Format : Pixels.Data_Format;
                           Data_Type : Pixels.Data_Type;
                           Data : Objects.Textures.Image_Source) with
     Dynamic => "glTexImage3D",
     Wrapper => "GL.Objects.Textures.With_3D_Loader.Load_Empty_Texture",
     Wrapper => "GL.Objects.Textures.With_3D_Loader.Load_From_Data";
   procedure Compressed_Tex_Image_3D
     (Target          : Low_Level.Enums.Texture_Kind;
      Level           : Objects.Textures.Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width, Height, Depth : Size;
      Border          : Low_Level.Zero;
      Image_Size      : Size;
      Data            : Objects.Textures.Image_Source) with
     Dynamic => "glCompressedTexImage3D",
     Wrapper => "GL.Objects.Textures.With_3D_Loader.Load_Compressed";
   procedure Tex_Sub_Image_3D (Target : Low_Level.Enums.Texture_Kind;
     Level  : Objects.Textures.Mipmap_Level;
     X_Offset, Y_Offset :Int;
     Width, Height : Size;
     Format : Pixels.Data_Format;
     Data_Type : Pixels.Data_Type;
     Data : Objects.Textures.Image_Source) with
     Dynamic => "glTexSubImage3D",
     Wrapper => "GL.Objects.Textures.With_3D_Loader.Load_Sub_Image_From_Data";
   procedure Tex_Storage_3D
     (Target : Low_Level.Enums.Texture_Kind; Level : Objects.Textures.Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width, Height, Depth : Size) with
     Dynamic => "glTexStorage3D",
     Wrapper => "GL.Objects.Textures.With_3D_Loader.Storage";
   procedure Tex_Env_Float (Target     : Enums.Textures.Env_Target;
                            Param_Name : Enums.Textures.Env_Parameter;
                            Value      : Single) with
     Static  => "glTexEnvf",
     Wrapper => "GL.Fixed.Textures.Set_RGB_Scale",
     Wrapper => "GL.Fixed.Textures.Set_Alpha_Scale",
     Wrapper => "GL.Fixed.Textures.Set_LoD_Bias";
   procedure Tex_Env_Int (Target     : Enums.Textures.Env_Target;
                          Param_Name : Enums.Textures.Env_Parameter;
                          Value      : Int) with Static  => "glTexEnvi";
   procedure Tex_Env_Tex_Func (Target     : Enums.Textures.Env_Target;
                               Param_Name : Enums.Textures.Env_Parameter;
                               Value      : Fixed.Textures.Texture_Function)
     with Static  => "glTexEnvi",
          Wrapper => "GL.Fixed.Textures.Set_Tex_Function";
   procedure Tex_Env_Combine_Func
     (Target     : Enums.Textures.Env_Target;
      Param_Name : Enums.Textures.Env_Parameter;
      Value      : Fixed.Textures.Combine_Function) with
     Static  => "glTexEnvi",
     Wrapper => "GL.Fixed.Textures.Set_RGB_Combine",
     Wrapper => "GL.Fixed.Textures.Set_Alpha_Combine";
   procedure Tex_Env_Source (Target     : Enums.Textures.Env_Target;
                             Param_Name : Enums.Textures.Env_Parameter;
                             Value      : Fixed.Textures.Source_Kind) with
     Static  => "glTexEnvi",
     Wrapper => "GL.Fixed.Textures.Set_RGB_Source",
     Wrapper => "GL.Fixed.Textures.Set_Alpha_Source";
   procedure Tex_Env_Arr (Target     : Enums.Textures.Env_Target;
                          Param_Name : Enums.Textures.Env_Parameter;
                          Value      : Low_Level.Single_Array) with
     Static => "glTexEnvfv", Wrapper => "GL.Fixed.Textures.Set_Env_Color";
   procedure Tex_Env_Bool (Target     : Enums.Textures.Env_Target;
                           Param_Name : Enums.Textures.Env_Parameter;
                           Value      : Low_Level.Bool) with
     Static  => "glTexEnvi",
     Wrapper => "GL.Fixed.Textures.Toggle_Point_Sprite_Coord_Replace";
   procedure Get_Tex_Env_Float (Target     : Enums.Textures.Env_Target;
                                Param_Name : Enums.Textures.Env_Parameter;
                                Value      : out Single) with
     Static  => "glGetTexEnvfv",
     Wrapper => "GL.Fixed.Textures.RGB_Scale",
     Wrapper => "GL.Fixed.Textures.Alpha_Scale",
     Wrapper => "GL.Fixed.Textures.LoD_Bias";
   procedure Get_Tex_Env_Tex_Func
     (Target     : Enums.Textures.Env_Target;
      Param_Name : Enums.Textures.Env_Parameter;
      Value      : out Fixed.Textures.Texture_Function) with
     Static => "glGetTexEnviv", Wrapper => "GL.Fixed.Textures.Tex_Function";
   procedure Get_Tex_Env_Combine_Func
     (Target     : Enums.Textures.Env_Target;
      Param_Name : Enums.Textures.Env_Parameter;
      Value      : out Fixed.Textures.Combine_Function) with
     Static  => "glGetTexEnviv",
     Wrapper => "GL.Fixed.Textures.RGB_Combine",
     Wrapper => "GL.Fixed.Textures.Alpha_Combine";
   procedure Get_Tex_Env_Source (Target     : Enums.Textures.Env_Target;
                                 Param_Name : Enums.Textures.Env_Parameter;
                                 Value      : out Fixed.Textures.Source_Kind)
     with Static  => "glGetTexEnviv",
          Wrapper => "GL.Fixed.Textures.RGB_Source",
          Wrapper => "GL.Fixed.Textures.Alpha_Source";
   procedure Get_Tex_Env_Arr (Target     : Enums.Textures.Env_Target;
                              Param_Name : Enums.Textures.Env_Parameter;
                              Value      : in out Low_Level.Single_Array) with
     Static => "glGetTexEnvfv", Wrapper => "GL.Fixed.Textures.Env_Color";
   procedure Get_Tex_Env_Bool (Target     : Enums.Textures.Env_Target;
                               Param_Name : Enums.Textures.Env_Parameter;
                               Value      : out Low_Level.Bool) with
     Static  => "glGetTexEnviv",
     Wrapper => "GL.Fixed.Textures.Point_Sprite_Coord_Replace";
   procedure Active_Texture (Texture : Int) with
     Dynamic => "glActiveTexture",
     Wrapper => "GL.Objects.Textures.Set_Active_Unit";
   procedure Generate_Mipmap (Target : Low_Level.Enums.Texture_Kind) with
     Dynamic => "glGenerateMipmap",
     Wrapper => "GL.Objects.Textures.Generate_Mipmap";
   procedure Invalidate_Tex_Image
     (Texture : UInt; Level : Objects.Textures.Mipmap_Level) with
     Dynamic => "glInvalidateTexImage",
     Wrapper => "GL.Objects.Textures.Invalidate_Image";
   procedure Invalidate_Tex_Sub_Image
     (Texture : UInt; Level : Objects.Textures.Mipmap_Level;
      X_Offset, Y_Offset, Z_Offset : Int; Width, Height, Depth : Size) with
     Dynamic => "glInvalidateTexSubImage",
     Wrapper => "GL.Objects.Textures.Invalidate_Sub_Image";

   -----------------------------------------------------------------------------
   --                             Buffer Objects                              --
   -----------------------------------------------------------------------------

   procedure Gen_Buffers (N : Size; Buffers : out UInt) with
     Dynamic => "glGenBuffers", Wrapper => "GL.Objects.Initialize_Id";
   procedure Delete_Buffers (N : Size; Buffers : Low_Level.UInt_Array) with
     Dynamic => "glDeleteBuffers";
   procedure Bind_Buffer (Target : Low_Level.Enums.Buffer_Kind; Buffer : UInt)
     with Dynamic =>"glBindBuffer", Wrapper => "GL.Objects.Buffers.Bind";
   procedure Bind_Buffer_Base (Target : Low_Level.Enums.Buffer_Kind; Index : UInt; Buffer : UInt)
    with Dynamic =>"glBindBufferBase", Wrapper => "GL.Objects.Buffers.Bind_Buffer_Base";
   procedure Buffer_Data
     (Target : Low_Level.Enums.Buffer_Kind;
      Size : Low_Level.SizeIPtr; Data : System.Address;
      Usage : Objects.Buffers.Buffer_Usage) with
     Dynamic => "glBufferData",
     Wrapper => "GL.Objects.Buffers.Load_To_Buffer",
     Wrapper => "GL.Objects.Buffers.Allocate";
   procedure Texture_Buffer_Data
    (Target : Low_Level.Enums.Buffer_Kind;
    Internal_Format : Pixels.Internal_Format; Buffer : UInt) with
    Dynamic => "glTexBuffer",
    Wrapper => "GL.Objects.Buffers.Allocate";
   function Map_Buffer (Target : Low_Level.Enums.Buffer_Kind;
                        Acc : Objects.Access_Kind) return System.Address with
     Dynamic => "glMapBuffer", Wrapper => "GL.Objects.Buffers.Map";
   procedure Buffer_Pointer (Target : Low_Level.Enums.Buffer_Kind;
                             Pname  : Enums.Buffer_Pointer_Param;
                             Params : out System.Address) with
     Dynamic => "glGetBufferPointerv", Wrapper => "GL.Objects.Buffers.Pointer";
   procedure Buffer_Sub_Data (Target : Low_Level.Enums.Buffer_Kind;
                              Offset : Low_Level.IntPtr;
                              Size   : Low_Level.SizeIPtr;
                              Data   : System.Address) with
     Dynamic => "glBufferSubData", Wrapper => "GL.Objects.Buffers.Set_Sub_Data";
   procedure Unmap_Buffer (Target : Low_Level.Enums.Buffer_Kind) with
     Dynamic => "glUnmapBuffer", Wrapper => "GL.Objects.Buffers.Unmap";
   procedure Get_Buffer_Parameter_Access_Kind
     (Target : Low_Level.Enums.Buffer_Kind; Value : Enums.Buffer_Param;
      Data   : out Objects.Access_Kind) with
     Dynamic => "glGetBufferParameteriv",
     Wrapper => "GL.Objects.Buffers.Access_Type";
   procedure Get_Buffer_Parameter_Bool
     (Target : Low_Level.Enums.Buffer_Kind; Value : Enums.Buffer_Param;
      Data   : out Low_Level.Bool) with
     Dynamic => "glGetBufferParameteriv",
     Wrapper => "GL.Objects.Buffers.Mapped";
   procedure Get_Buffer_Parameter_Size
     (Target : Low_Level.Enums.Buffer_Kind; Value : Enums.Buffer_Param;
      Data   : out Size) with
     Dynamic => "glGetBufferParameteriv",
     Wrapper => "GL.Objects.Buffers.Size";
   procedure Get_Buffer_Parameter_Usage
     (Target : Low_Level.Enums.Buffer_Kind; Value : Enums.Buffer_Param;
      Data   : out Objects.Buffers.Buffer_Usage) with
     Dynamic => "glGetBufferParameteriv",
     Wrapper => "GL.Objects.Buffers.Usage";
   procedure Invalidate_Buffer_Data (Buffer : UInt) with
     Dynamic => "glInvalidateBufferData",
     Wrapper => "GL.Objects.Buffers.Invalidate_Data";
   procedure Invalidate_Buffer_Sub_Data
     (Buffer : UInt; Offset : Low_Level.IntPtr; Length : Low_Level.SizeIPtr)
     with Dynamic => "glInvalidateBufferSubData",
          Wrapper => "GL.Objects.Buffers.Invalidate_Sub_Data";

   -----------------------------------------------------------------------------
   --                           Vertex Array Objects                          --
   -----------------------------------------------------------------------------

   procedure Gen_Vertex_Arrays (N : Size; Arrays : out UInt) with
     Dynamic => "glGenVertexArrays",
     Wrapper => "GL.Objects.Initialize_Id";
   procedure Delete_Vertex_Arrays (N : Size; Arrays : Low_Level.UInt_Array)
     with Dynamic => "glDeleteVertexArrays";
   procedure Bind_Vertex_Array (Arr : UInt) with
     Dynamic => "glBindVertexArray", Wrapper => "GL.Objects.Vertex_Arrays.Bind";

   -----------------------------------------------------------------------------
   --                        Renderbuffer objects                             --
   -----------------------------------------------------------------------------

   procedure Gen_Renderbuffers (N : Size; Renderbuffers : out UInt) with
     Dynamic => "glGenRenderbuffers",
     Wrapper => "GL.Objects.Initialize_Id";
   procedure Delete_Renderbuffers
     (N : Size; Renderbuffers : Low_Level.UInt_Array) with
     Dynamic => "glDeleteBuffers";
   procedure Renderbuffer_Storage
     (Target : Low_Level.Enums.Renderbuffer_Kind;
      Internal_Format : Pixels.Internal_Format; Width, Height : Size) with
     Dynamic => "glRenderbufferStorage",
     Wrapper => "GL.Objects.Renderbuffers.Allocate";
   procedure Renderbuffer_Storage_Multisample
     (Target : Low_Level.Enums.Renderbuffer_Kind; Samples : Size;
      Internal_Format : Pixels.Internal_Format; Width, Height : Size) with
     Dynamic => "glRenderbufferStorageMultisample",
     Wrapper => "GL.Objects.Renderbuffers.Allocate";
   procedure Bind_Renderbuffer (Target : Low_Level.Enums.Renderbuffer_Kind;
                                Renderbuffer : UInt) with
     Dynamic => "glBindRenderbuffer",
     Wrapper => "GL.Objects.Renderbuffers.Bind";
   procedure Get_Renderbuffer_Parameter_Int
     (Target : Low_Level.Enums.Renderbuffer_Kind;
      Pname  : Enums.Getter.Renderbuffer_Parameter; Params : out Int) with
     Dynamic => "glGetRenderbufferParameteriv",
     Wrapper => "GL.Objects.Renderbuffers.Width",
     Wrapper => "GL.Objects.Renderbuffers.Height",
     Wrapper => "GL.Objects.Renderbuffers.Red_Size",
     Wrapper => "GL.Objects.Renderbuffers.Green_Size",
     Wrapper => "GL.Objects.Renderbuffers.Blue_Size",
     Wrapper => "GL.Objects.Renderbuffers.Alpha_Size",
     Wrapper => "GL.Objects.Renderbuffers.Depth_Size",
     Wrapper => "GL.Objects.Renderbuffers.Stencil_Size";
   procedure Get_Renderbuffer_Parameter_Internal_Format
     (Target : Low_Level.Enums.Renderbuffer_Kind;
      Pname  : Enums.Getter.Renderbuffer_Parameter;
      Params : out Pixels.Internal_Format) with
     Dynamic => "glGetRenderbufferParameteriv",
     Wrapper => "GL.Objects.Renderbuffers.Internal_Format";

   -----------------------------------------------------------------------------
   --                  Framebuffer objects and handling                       --
   -----------------------------------------------------------------------------

   procedure Read_Pixels (X, Y : Int; Width, Height : Size;
                          Format : Pixels.Framebuffer_Format;
                          Data_Type : Pixels.Data_Type;
                          Data : System.Address) with
     Static => "glReadPixels", Wrapper => "GL.Framebuffer.Read_Pixels";
   procedure Logic_Op (Value : Framebuffer.Logic_Op) with
     Static => "glLogicOp", Wrapper => "GL.Framebuffer.Set_Logic_Op_Mode";
   procedure Clamp_Color (Target: Enums.Clamp_Color_Param;
                          Clamp: Low_Level.Bool) with
     Dynamic => "glClampColor",
     Wrapper => "GL.Framebuffer.Set_Clamp_Read_Color";
   procedure Read_Buffer (Value : Framebuffer.Read_Buffer_Selector) with
     Static => "glReadBuffer", Wrapper => "GL.Framebuffer.Set_Read_Buffer";
   procedure Gen_Framebuffers (N : Size; Framebuffers : out UInt) with
     Dynamic => "glGenFramebuffers",
     Wrapper => "GL.Objects.Initialize_Id";
   procedure Delete_Framebuffers
     (N : Size; Framebuffers : Low_Level.UInt_Array) with
     Dynamic => "glDeleteFramebuffers";
   procedure Bind_Framebuffer
     (Target : Low_Level.Enums.Framebuffer_Kind; Framebuffer : UInt) with
     Dynamic => "glBindFramebuffer",
     Wrapper => "GL.Objects.Framebuffers.Bind";
   function Check_Framebuffer_Status (Target : Low_Level.Enums.Framebuffer_Kind)
     return Objects.Framebuffers.Framebuffer_Status with
     Dynamic => "glCheckFramebufferStatus",
     Wrapper => "GL.Objects.Framebuffers.Status";
   procedure Framebuffer_Renderbuffer
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Attachment : Objects.Framebuffers.Attachment_Point;
      Renderbuffer_Target : Low_Level.Enums.Renderbuffer_Kind;
      Renderbuffer : UInt) with
     Dynamic => "glFramebufferRenderbuffer",
     Wrapper => "GL.Objects.Framebuffers.Attach_Renderbuffer";
   procedure Framebuffer_Texture
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Attachment : Objects.Framebuffers.Attachment_Point;
      Texture : UInt; Level : Objects.Textures.Mipmap_Level) with
     Dynamic => "glFramebufferTexture",
     Wrapper => "GL.Objects.Framebuffers.Attach_Texture";
   procedure Framebuffer_Texture_Layer
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Attachment : Objects.Framebuffers.Attachment_Point; Texture : UInt;
      Level : Objects.Textures.Mipmap_Level; Layer : Int) with
     Dynamic => "glFramebufferTextureLayer",
     Wrapper => "GL.Objects.Framebuffers.Attach_Texture_Layer";
   procedure Blit_Framebuffer
     (Src_X0, Src_X1, Src_Y0, Src_X1, Dst_X0, Dst_X1, Dst_Y0, Dst_Y1 : Int;
      Mask : Low_Level.Bitfield; Filter : Objects.Textures.Magnifying_Function)
     with Dynamic => "glBlitFramebuffer",
          Wrapper => "GL.Objects.Framebuffers.Blit";
   procedure Invalidate_Framebuffer
     (Target : Low_Level.Enums.Framebuffer_Kind; Num_Attachments : Size;
      Attachments : Objects.Framebuffers.Attachment_List) with
     Dynamic => "glInvalidateFramebuffer",
     Wrapper => "GL.Objects.Framebuffers.Invalidate";
   procedure Invalidate_Sub_Framebuffer
     (Target : Low_Level.Enums.Framebuffer_Kind; Num_Attachments : Size;
      Attachments : Objects.Framebuffers.Attachment_List; X, Y : Int;
      Width, Height : Size) with
     Dynamic => "glInvalidateSubFramebuffer",
     Wrapper => "GL.Objects.Framebuffers.Invalidate_Sub";
   procedure Framebuffer_Parameter_Size
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Pname  : Enums.Framebuffer_Param; Param : Size) with
     Dynamic => "glFramebufferParameteri",
     Wrapper => "GL.Objects.Framebuffers.Set_Default_Width",
     Wrapper => "GL.Objects.Framebuffers.Set_Default_Height",
     Wrapper => "GL.Objects.Framebuffers.Set_Default_Layers",
     Wrapper => "GL.Objects.Framebuffers.Set_Default_Samples";
   procedure Framebuffer_Parameter_Bool
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Pname  : Enums.Framebuffer_Param; Param : Low_Level.Bool) with
     Dynamic => "glFramebufferParameteri",
     Wrapper => "GL.Objects.Framebuffers.Set_Default_Fixed_Sample_Locactions";
   procedure Get_Framebuffer_Parameter_Size
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Pname  : Enums.Framebuffer_Param; Params : out Size) with
     Dynamic => "glGetFramebufferParameteriv",
     Wrapper => "GL.Objects.Framebuffers.Default_Width",
     Wrapper => "GL.Objects.Framebuffers.Default_Height",
     Wrapper => "GL.Objects.Framebuffers.Default_Layers",
     Wrapper => "GL.Objects.Framebuffers.Default_Samples";
   procedure Get_Framebuffer_Parameter_Bool
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Pname  : Enums.Framebuffer_Param; Params : out Low_Level.Bool) with
     Dynamic => "glGetFramebufferParameteriv",
     Wrapper => "GL.Objects.Framebuffers.Default_Fixed_Sample_Locations";

   -----------------------------------------------------------------------------
   --                                 Shaders                                 --
   -----------------------------------------------------------------------------

   procedure Get_Shader_Param (Shader : UInt; Pname : Enums.Shader_Param;
                               Param : out Int) with
     Dynamic => "glGetShaderiv", Wrapper => "GL.Objects.Shaders.Compile_Status";
   procedure Get_Shader_Type
     (Shader : UInt; Pname : Enums.Shader_Param;
      Param : out Objects.Shaders.Shader_Type) with
     Dynamic => "glGetShaderiv", Wrapper => "GL.Objects.Shaders.Create_From_Id";
   function Create_Shader (Shader_Type : Objects.Shaders.Shader_Type)
                          return UInt with
     Dynamic => "glCreateShader", Wrapper => "GL.Objects.Initialize_Id";
   procedure Delete_Shader (Shader : UInt) with Dynamic => "glDeleteShader";
   procedure Shader_Source
     (Shader : UInt; Count : Size;
      Str : Low_Level.Char_Access_Array; Length : Low_Level.Int_Array) with
     Dynamic => "glShaderSource", Wrapper => "GL.Objects.Shaders.Set_Source";
   procedure Get_Shader_Source (Shader : UInt; Buffer_Size : Size;
                                Length : out Size; Value : in out String) with
     Dynamic => "glGetShaderSource",
     Wrapper => "GL.Objects.Shaders.Source";
   procedure Compile_Shader (Shader : UInt) with
     Dynamic => "glCompileShader", Wrapper => "GL.Objects.Shaders.Compile";
   procedure Release_Shader_Compiler with
     Dynamic => "glReleaseShaderCompiler",
     Wrapper => "GL.Objects.Shaders.Release_Shader_Compiler";
   procedure Get_Shader_Info_Log (Shader : UInt; Buffer_Size : Size;
                                  Length : out Size; Value : in out String) with
     Dynamic => "glGetShaderInfoLog", Wrapper => "GL.Objects.Shaders.Info_Log";
   function Create_Program return UInt with
     Dynamic => "glCreateProgram",
     Wrapper => "GL.Objects.Initialize_Id";
   procedure Delete_Program (Program : UInt) with Dynamic => "glDeleteProgram";
   procedure Get_Program_Param (Program : UInt; Pname : Enums.Program_Param;
                                Params : out Int) with
     Dynamic => "glGetProgramiv",
     Wrapper => "GL.Objects.Programs.Link_Status",
     Wrapper => "GL.Objects.Programs.Delete_Status",
     Wrapper => "GL.Objects.Programs.Validate_Status",
     Wrapper => "GL.Objects.Programs.Active_Attributes",
     Wrapper => "GL.Objects.Programs.Active_Attribute_Max_Length",
     Wrapper => "GL.Objects.Programs.Active_Uniform_Blocks",
     Wrapper => "GL.Objects.Programs.Active_Uniform_Block_Max_Name_Length",
     Wrapper => "GL.Objects.Programs.Active_Uniforms",
     Wrapper => "GL.Objects.Programs.Active_Uniform_Max_Length",
     Wrapper => "GL.Objects.Programs.Tess_Control_Output_Vertices",
     Wrapper => "GL.Objects.Programs.Tess_Gen_Mode",
     Wrapper => "GL.Objects.Programs.Tess_Gen_Point_Mode",
     Wrapper => "GL.Objects.Programs.Tess_Gen_Spacing",
     Wrapper => "GL.Objects.Programs.Tess_Gen_Vertex_Order",
     Wrapper => "GL.Objects.Programs.Transform_Feedback_Buffer_Mode",
     Wrapper => "GL.Objects.Programs.Transform_Feedback_Varyings",
     Wrapper => "GL.Objects.Programs.Transform_Feedback_Varying_Max_Length",
     Wrapper => "GL.Objects.Programs.Begin_Transform_Feedback",
     Wrapper => "GL.Objects.Programs.End_Transform_Feedback";
   procedure Attach_Shader (Program, Shader : UInt) with
     Dynamic => "glAttachShader", Wrapper => "GL.Objects.Programs.Attach";
   procedure Link_Program (Program : UInt) with
     Dynamic => "glLinkProgram", Wrapper => "GL.Objects.Programs.Link";
   procedure Get_Program_Info_Log (Program : UInt; Buffer_Size : Size;
                                   Length : out Size; Value : in out String)
     with Dynamic => "glGetProgramInfoLog",
          Wrapper => "GL.Objects.Programs.Info_Log";
   procedure Get_Program_Stage
     (Program : UInt; Shader_Type : Objects.Shaders.Shader_Type;
      Pname   : Enums.Program_Stage_Param; Params : out Size) with
     Dynamic => "glGetProgramStageiv",
     Wrapper => "GL.Objects.Programs.Active_Subroutines",
     Wrapper => "GL.Objects.Programs.Active_Subroutine_Uniforms",
     Wrapper => "GL.Objects.Programs.Active_Subroutine_Uniform_Locations",
     Wrapper => "GL.Objects.Programs.Active_Subroutine_Uniform_Max_Length",
     Wrapper => "GL.Objects.Programs.Active_Subroutine_Max_Length";
   function Get_Subroutine_Index
     (Program : UInt; Shader_Type : Objects.Shaders.Shader_Type;
      Name : Interfaces.C.char_array)
     return Objects.Programs.Subroutine_Index_Type with
     Dynamic => "glGetSubroutineIndex",
     Wrapper => "GL.Objects.Programs.Subroutine_Index";
   function Get_Subroutine_Uniform_Location
     (Program : UInt; Shader_Type : Objects.Shaders.Shader_Type;
      Name : Interfaces.C.char_array)
     return Objects.Programs.Uniform_Location_Type with
     Dynamic => "glGetSubroutineUniformLocation",
     Wrapper => "GL.Objects.Programs.Subroutine_Uniform_Locations";
   procedure Use_Program (Program : UInt) with
     Dynamic => "glUseProgram", Wrapper => "GL.Objects.Programs.Use_Program";
   procedure Validate_Program (Program : UInt) with
     Dynamic => "glValidateProgram", Wrapper => "GL.Objects.Programs.Validate";
   function Get_Uniform_Location
     (Program : UInt; Name : C.char_array) return Uniforms.Uniform with
     Dynamic => "glGetUniformLocation",
     Wrapper => "GL.Objects.Programs.Uniform_Location";
   procedure Bind_Attrib_Location
     (Program : UInt; Index : Attributes.Attribute; Name : C.char_array) with
     Dynamic => "glBindAttribLocation",
     Wrapper => "GL.Objects.Programs.Bind_Attrib_Location";
   function Get_Attrib_Location
     (Program : UInt; Name : C.char_array) return Attributes.Attribute with
     Dynamic => "glGetAttribLocation",
     Wrapper => "GL.Objects.Programs.Attrib_Location";
   procedure Vertex_Attrib_Pointer
     (Index : Attributes.Attribute; Size : Component_Count;
      N_Type : Numeric_Type; Normalized : Low_Level.Bool; Stride : Size;
      Pointer : Int) with
     Dynamic => "glVertexAttribPointer",
     Wrapper => "GL.Attributes.Set_Vertex_Attrib_Pointer";
   procedure Vertex_AttribI_Pointer
     (Index : Attributes.Attribute; Size : Component_Count;
      N_Type : Numeric_Type; Stride : Size; Pointer : Int) with
     Dynamic => "glVertexAttribIPointer",
     Wrapper => "GL.Attributes.Set_Vertex_Integer_Attrib_Pointer";
   procedure Vertex_AttribL_Pointer
     (Index : Attributes.Attribute; Size : Component_Count;
      N_Type :Numeric_Type; Stride : Size; Pointer : Int) with
     Dynamic => "glVertexAttribLPointer",
     Wrapper => "GL.Attributes.Set_Vertex_Double_Attrib_Pointer";
   procedure Enable_Vertex_Attrib_Array (Index : Attributes.Attribute) with
     Dynamic => "glEnableVertexAttribArray",
     Wrapper => "GL.Attributes.Enable_Vertex_Attrib_Array";
   procedure Disable_Vertex_Attrib_Array (Index : Attributes.Attribute) with
     Dynamic => "glDisableVertexAttribArray",
     Wrapper => "GL.Attributes.Disable_Vertex_Attrib_Array";
   procedure Get_Attached_Shaders
     (Program : UInt; Max_Count : Size; Count : out Size;
      Shaders : in out UInt_Array) with
     Dynamic => "glGetAttachedShaders",
     Wrapper => "GL.Objects.Programs.Attached_Shaders";
   procedure Bind_Frag_Data_Location
     (Program : UInt; Color_Number : Buffers.Draw_Buffer_Index;
      Name : Interfaces.C.char_array)
     is Dynamic ("glBindFragDataLocation"),
        Wrapper ("GL.Objects.Programs.Bind_Frag_Data_Location");
   function Get_Frag_Data_Location
     (Program : UInt; Name : Interfaces.C.char_array) return Int with
     Dynamic => "glGetFragDataLocation",
     Wrapper => "GL.Objects.Programs.Frag_Data_Location";
   procedure Begin_Transform_Feedback (Primitive_Mode : Connection_Mode) with
     Dynamic => "glBeginTransformFeedback",
     Wrapper => "GL.Objects.Programs.Begin_Transform_Feedback";
   procedure End_Transform_Feedback with
     Dynamic => "glEndTransformFeedback",
     Wrapper => "GL.Objects.Programs.End_Transform_Feedback";
   procedure Get_Transform_Feedback_Varying
    (Program :  UInt; Index : Int; Buffer_Size : Size;
     Length : out Size; V_Length : out Size; V_Type : out GL.Objects.Programs.Buffer_Mode;
     Name : in out Interfaces.C.char_array) with
     Dynamic => "glGetTransformFeedbackVarying",
     Wrapper => "GL.Objects.Programs.Get_Transform_Feedback_Varying";
   procedure Transform_Feedback_Varyings
    (Program :  UInt; Count : Size; Varyings : Low_Level.Char_Access_Array;
     Buffer_Mode : GL.Objects.Programs.Buffer_Mode) with
     Dynamic => "glTransformFeedbackVaryings",
     Wrapper => "GL.Objects.Programs.Transform_Feedback_Varyings";

   -----------------------------------------------------------------------------
   --                              Tessellation                               --
   -----------------------------------------------------------------------------

   procedure Set_Patch_Parameter_Int
     (Pname : Enums.Patch_Parameter_Int; Value : Int) with
     Dynamic => "glPatchParameteri",
     Wrapper => "GL.Tessellation.Set_Patch_Vertices";
   procedure Set_Patch_Parameter_Float_Array
     (Pname : Enums.Patch_Parameter_Float_Array; Value : Types.Single_Array)
     with Dynamic => "glPatchParameterfv",
          Wrapper => "GL.Tessellation.Set_Patch_Default_Inner_Level",
          Wrapper => "GL.Tessellation.Set_Patch_Default_Outer_Level";

   -----------------------------------------------------------------------------
   --                  Transformation to window coordinates                   --
   -----------------------------------------------------------------------------

   procedure Depth_Range (Near, Far : Double) with
     Static => "glDepthRange", Wrapper => "GL.Window.Set_Depth_Range";
   procedure Viewport (X, Y : Int; Width, Height : Size) with
     Static => "glViewport", Wrapper => "GL.Window.Set_Viewport";
end GL.API;
