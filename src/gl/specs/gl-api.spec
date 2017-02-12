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
   function Get_Error return Errors.Error_Code is Static ("glGetError");
   procedure Flush is Static ("glFlush");
   procedure Finish is Static ("glFinish");

   -----------------------------------------------------------------------------
   --                           Parameter Getters                             --
   -----------------------------------------------------------------------------

   procedure Get_Boolean (Name   : Enums.Getter.Parameter;
                          Target : access Low_Level.Bool) is Static ("glGetBooleanv");
   procedure Get_Double (Name   : Enums.Getter.Parameter;
                         Target : access Double) is Static ("glGetDoublev");
   procedure Get_Double_Vec2 (Name   : Enums.Getter.Parameter;
                              Target : in out Doubles.Vector2) is Static ("glGetDoublev");
   procedure Get_Single (Name   : Enums.Getter.Parameter;
                         Target : access Single) is Static ("glGetFloatv");
   procedure Get_Single_Vec2 (Name   : Enums.Getter.Parameter;
                              Target : in out Singles.Vector2) is Static ("glGetFloatv");
   procedure Get_Color (Name   : Enums.Getter.Parameter;
                        Target : in out Colors.Color) is Static ("glGetFloatv");
   procedure Get_Integer (Name   : Enums.Getter.Parameter;
                          Target : access Int) is Static ("glGetIntegerv");
   procedure Get_Int_Vec4 (Name   : Enums.Getter.Parameter;
                           Target : in out Ints.Vector4) is Static ("glGetIntegerv");
   procedure Get_Unsigned_Integer (Name   : Enums.Getter.Parameter;
                                   Target : access UInt) is Static ("glGetIntegerv");
   procedure Get_Size (Name   : Enums.Getter.Parameter;
                       Target : access Size) is Static ("glGetIntegerv");
   procedure Get_Color_Control (Name   : Enums.Getter.Parameter;
                                Target : access Fixed.Lighting.Color_Control) is
     Static ("glGetIntegerv");
   procedure Get_Shade_Model (Name   : Enums.Getter.Parameter;
                              Target : access Fixed.Lighting.Shade_Model) is
     Static ("glGetIntegerv");
   procedure Get_Blend_Factor (Name : Enums.Getter.Parameter;
                               Target : access Blending.Blend_Factor) is
     Static ("glGetIntegerv");
   procedure Get_Alignment (Name : Enums.Getter.Parameter;
                            Target : access Pixels.Alignment) is
     Static ("glGetIntegerv");
   procedure Get_Blend_Equation (Name : Enums.Getter.Parameter;
                                 Target : access Blending.Equation) is
     Static ("glGetIntegerv");
   procedure Get_Compare_Function (Name : Enums.Getter.Parameter;
                                   Target : access Compare_Function) is
     Static ("glGetIntegerv");
   procedure Get_Orientation (Name : Enums.Getter.Parameter;
                              Target : access Orientation) is
     Static ("glGetIntegerv");
   procedure Get_Face_Selector (Name : Enums.Getter.Parameter;
                                Target : access Culling.Face_Selector) is
     Static ("glGetIntegerv");
   procedure Get_Polygon_Mode (Name   : Enums.Getter.Parameter;
                               Target : access Rasterization.Polygon_Mode_Type) is
     Static ("glGetIntegerv");
   procedure Get_Logic_Op (Name : Enums.Getter.Parameter;
                           Target : access Framebuffer.Logic_Op) is
     Static ("glGetIntegerv");
   procedure Get_Stencil_Action (Name : Enums.Getter.Parameter;
                                 Target : access Buffers.Stencil_Action) is
     Static ("glGetIntegerv");
   procedure Get_Read_Buffer_Selector
     (Name   : Enums.Getter.Parameter;
      Target : access Framebuffer.Read_Buffer_Selector) is Static ("glGetIntegerv");
   procedure Get_Light_Color (Name   : Enums.Light_Name;
                              Pname  : Enums.Light_Param;
                              Target : in out Colors.Color) is Static ("glGetLightfv");
   function Get_String (Name : Enums.Getter.String_Parameter)
                        return C.Strings.chars_ptr is Static ("glGetString");
   function Get_String_I (Name  : Enums.Getter.String_Parameter;
                          Index : Uint) return C.Strings.chars_ptr is
     Dynamic ("glGetStringi");

   -----------------------------------------------------------------------------
   --                                 Toggles                                 --
   -----------------------------------------------------------------------------

   procedure Enable (Subject : Toggles.Toggle) is Static ("glEnable");
   procedure Disable (Subject : Toggles.Toggle) is Static ("glDisable");
   function Is_Enabled (Subject : Toggles.Toggle) return Low_Level.Bool is
     Static ("glIsEnabled");

   -----------------------------------------------------------------------------
   --                                 Culling                                 --
   -----------------------------------------------------------------------------

   procedure Cull_Face (Selector : Culling.Face_Selector) is Static ("glCullFace");
   procedure Front_Face (Face : Orientation) is Static ("glFrontFace");

   -----------------------------------------------------------------------------
   --                               Pixel Stuff                               --
   -----------------------------------------------------------------------------

   procedure Pixel_Store (Param : Enums.Pixel_Store_Param;
                          Value : Low_Level.Bool) is Static ("glPixelStorei");
   procedure Pixel_Store (Param : Enums.Pixel_Store_Param;
                          Value : Size) is Static ("glPixelStorei");
   procedure Pixel_Store (Param : Enums.Pixel_Store_Param;
                          Value : Pixels.Alignment) is Static ("glPixelStorei");

   -----------------------------------------------------------------------------
   --            Matrix stack API (deprecated as of OpenGL 3.0)               --
   -----------------------------------------------------------------------------

   procedure Matrix_Mode (Mode : Enums.Matrix_Mode) is Static ("glMatrixMode");
   procedure Frustum (Left, Right, Bottom, Top, zNear, zFar : Double) is
     Static ("glFrustum");
   procedure Ortho (Left, Right, Bottom, Top, zNear, zFar : Double) is
     Static ("glOrtho");
   procedure Load_Identity is Static ("glLoadIdentity");
   procedure Push_Matrix is Static ("glPushMatrix");
   procedure Pop_Matrix is Static ("glPopMatrix");
   procedure Rotate (Angle, X, Y, Z : Double) is Static ("glRotated");
   procedure Scale (X, Y, Z : Double) is Static ("glScaled");
   procedure Translate (X, Y, Z : Double) is Static ("glTranslated");

   -----------------------------------------------------------------------------
   --              Immediate API (deprecated as of OpenGL 3.0)                --
   -----------------------------------------------------------------------------

   procedure GL_Begin (Mode : Connection_Mode) is Static ("glBegin");
   procedure GL_End is Static ("glEnd");
   procedure Color (Value : Colors.Color) is Static ("glColor4fv");
   procedure Secondary_Color (Value : Colors.Color) is
     Dynamic ("glSecondaryColor3dv");
   procedure Fog_Coord (Value : Double) is Dynamic ("glFogCoordd");

   -----------------------------------------------------------------------------
   --        Fixed Function Pipeline (deprecated as of OpenGL 3.0)            --
   -----------------------------------------------------------------------------

   procedure Vertex_Pointer (Count     : Int;
                             Data_Type : Signed_Numeric_Type;
                             Stride    : Size;
                             Pointer   : Int) is Static ("glVertexPointer");
   procedure Index_Pointer (Data_Type : Signed_Numeric_Type;
                            Stride    : Size;
                            Pointer   : Int) is Static ("glIndexPointer");
   procedure Color_Pointer (Count     : Int;
                            Data_Type : Signed_Numeric_Type;
                            Stride    : Size;
                            Pointer   : Int) is Static ("glColorPointer");
   procedure Enable_Client_State (Cap : Fixed.Client_Side_Capability) is
     Static ("glEnableClientState");
   procedure Disable_Client_State (Cap : Fixed.Client_Side_Capability) is
     Static ("glDisableClientState");
   procedure Draw_Arrays (Mode  : Connection_Mode;
                          First : Int; Count : Size) is Static ("glDrawArrays");
   procedure Draw_Elements (Mode       : Connection_Mode;
                            Count      : Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Indices    : Low_Level.Zero) is
     Static ("glDrawElements");

   -----------------------------------------------------------------------------
   --               Lighting API (deprecated as of OpenGL 3.0)                --
   -----------------------------------------------------------------------------

   procedure Light_Model_Color (Param : Enums.Light_Model_Ambient_Parameter;
                                Color : Colors.Color) is
     Static ("glLightModelfv");
   procedure Light_Model_Color_Control (Param : Enums.Light_Model_CC_Parameter;
     Value : access constant Fixed.Lighting.Color_Control) is
     Static ("glLightModeliv");
   procedure Light_Model_Toggles (Param : Enums.Light_Model_Toggle_Parameter;
                                  Value : access constant Int) is
     Static ("glLightModeliv");
   procedure Shade_Model (Mode : Fixed.Lighting.Shade_Model) is
     Static ("glShadeModel");
   procedure Light_Color (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
                          Param : Colors.Color) is Static ("glLightfv");

   -----------------------------------------------------------------------------
   --                               Blending                                  --
   -----------------------------------------------------------------------------

   procedure Blend_Func (Src_Factor, Dst_Factor : Blending.Blend_Factor) is
     Static ("glBlendFunc");
   procedure Blend_Func_I (Buf : Buffers.Draw_Buffer_Index;
     Src_Factor, Dst_Factor : Blending.Blend_Factor) is
     Dynamic ("glBlendFunci");
   procedure Blend_Func_Separate
     (Src_Rgb, Dst_Rgb, Src_Alpha, Dst_Alpha : Blending.Blend_Factor) is
     Dynamic ("glBlendFuncSeparate");
   procedure Blend_Func_Separate_I (Buf : Buffers.Draw_Buffer_Index;
     Src_Rgb, Dst_Rgb, Src_Alpha, Dst_Alpha : Blending.Blend_Factor) is
     Dynamic ("glBlendFuncSeparate");
   procedure Blend_Color (Red, Green, Blue, Alpha : Colors.Component) is
     Dynamic ("glBlendColor");
   procedure Blend_Equation (Mode : Blending.Equation) is
     Dynamic ("glBlendEquation");
   procedure Blend_Equation_I (Buf : Buffers.Draw_Buffer_Index;
                               Mode : Blending.Equation) is
     Dynamic ("glBlendEquationi");
   procedure Blend_Equation_Separate (Mode_Rgb, Mode_Alpha : Blending.Equation)
     is Dynamic ("glBlendEquationSeparate");
   procedure Blend_Equation_Separate_I (Buf : Buffers.Draw_Buffer_Index;
     Mode_Rgb, Mode_Alpha : Blending.Equation) is Dynamic ("glBlendEquationi");

   -----------------------------------------------------------------------------
   --                             Rasterization                               --
   -----------------------------------------------------------------------------

   procedure Line_Width (Value : Single) is Static ("glLineWidth");
   procedure Polygon_Mode (Face : Culling.Face_Selector;
                           Value : Rasterization.Polygon_Mode_Type) is
     Static ("glPolygonMode");
   procedure Set_Point_Size (Value : Single) is Static ("glPointSize");
   procedure Set_Point_Parameter_Single (Pname : Enums.Point_Param;
     Param : Single) is Dynamic ("glPointParameterf");

   -----------------------------------------------------------------------------
   --                                Buffers                                  --
   -----------------------------------------------------------------------------

   procedure Clear (Bits : Low_Level.Bitfield) is Static ("glClear");
   procedure Draw_Buffer (Mode : Buffers.Explicit_Color_Buffer_Selector) is
     Static ("glDrawBuffer");
   procedure Draw_Buffers (N : UInt; Bufs : Buffers.Explicit_Color_Buffer_List)
     is Dynamic ("glDrawBuffers");
   procedure Clear_Color (Red, Green, Blue, Alpha : Colors.Component) is
     Static ("glClearColor");
   procedure Clear_Depth (Depth : Buffers.Depth) is Static ("glClearDepth");
   procedure Clear_Stencil (Index : Buffers.Stencil_Index) is
     Static ("glClearStencil");

   -- dropped in OpenGL 3
   procedure Clear_Accum (Red, Green, Blue, Alpha : Colors.Component) is
     Dynamic ("glClearAccum");

   procedure Clear_Buffer (Buffer : Buffers.Color_Buffer_Selector;
                           Drawbuffer : Low_Level.Zero; Value : Colors.Color) is
     Dynamic ("glClearBufferfv");
   procedure Clear_Draw_Buffer
     (Buffer : Low_Level.Enums.Only_Color_Buffer;
      Drawbuffer : Buffers.Draw_Buffer_Index; Value : Colors.Color) is
     Dynamic ("glClearBufferfv");
   procedure Clear_Buffer_Depth
     (Buffer : Low_Level.Enums.Only_Depth_Buffer; Drawbuffer : Low_Level.Zero;
      Value : access constant Buffers.Depth) is Dynamic ("glClearBufferfv");
   procedure Clear_Buffer_Stencil
     (Buffer : Low_Level.Enums.Only_Stencil_Buffer; Drawbuffer : Low_Level.Zero;
      Value : access constant Buffers.Stencil_Index) is
     Dynamic ("glClearBufferiv");
   procedure Clear_Buffer_Depth_Stencil
     (Buffer : Low_Level.Enums.Only_Depth_Stencil_Buffer;
      Drawbuffer : Low_Level.Zero;
      Depth : Buffers.Depth; Stencil : Buffers.Stencil_Index) is
     Dynamic ("glClearBufferfi");

   -----------------------------------------------------------------------------
   --                        Depth And Stencil Buffers                        --
   -----------------------------------------------------------------------------

   procedure Depth_Mask (Value : Low_Level.Bool) is Static ("glDepthMask");
   procedure Depth_Func (Func : Compare_Function) is Static ("glDepthFunc");
   procedure Stencil_Func_Separate (Face : Culling.Face_Selector;
     Func : Compare_Function; Ref : Int; Mask : UInt) is
     Dynamic ("glStencilFuncSeparate");
   procedure Stencil_Op_Separate  (Face : Culling.Face_Selector;
     S_Fail, Dp_Fail, Dp_Pass : Buffers.Stencil_Action) is
     Dynamic ("glStencilOpSeparate");
   procedure Stencil_Mask_Separate (Face : Culling.Face_Selector;
     Mask : UInt) is Dynamic ("glStencilMaskSeparate");

   -----------------------------------------------------------------------------
   --                                Textures                                 --
   -----------------------------------------------------------------------------

   procedure Tex_Parameter_Float (Target     : Low_Level.Enums.Texture_Kind;
                                  Param_Name : Enums.Textures.Parameter;
                                  Value      : Single) is
     Static ("glTexParameterf");
   procedure Tex_Parameter_Int (Target     : Low_Level.Enums.Texture_Kind;
                                Param_Name : Enums.Textures.Parameter;
                                Value      : Int) is Static ("glTexParameteri");
   procedure Tex_Parameter_Min_Filter
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Value      : Objects.Textures.Minifying_Function) is
     Static ("glTexParameteri");
   procedure Tex_Parameter_Mag_Filter
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Value      : Objects.Textures.Magnifying_Function) is
     Static ("glTexParameteri");
   procedure Tex_Parameter_Wrap_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Value      : Objects.Textures.Wrapping_Mode) is
     Static ("glTexParameteri");
   procedure Tex_Parameter_Comp_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Value      : Enums.Textures.Compare_Kind) is Static ("glTexParameteri");
   procedure Tex_Parameter_Comp_Func (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : Compare_Function) is
     Static ("glTexParameteri");
   procedure Tex_Parameter_Depth_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Value      : Objects.Textures.Depth_Mode) is Static ("glTexParameteri");
   procedure Tex_Parameter_Bool (Target     : Low_Level.Enums.Texture_Kind;
                                 Param_Name : Enums.Textures.Parameter;
                                 Value      : Low_Level.Bool) is
     Static ("glTexParameteri");
   procedure Tex_Parameter_Floats (Target     : Low_Level.Enums.Texture_Kind;
                                   Param_Name : Enums.Textures.Parameter;
                                   Values     : Low_Level.Single_Array) is
     Static ("glTexParameterfv");
   procedure Get_Tex_Parameter_Float (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : out Single) is
     Static ("glGetTexParameterfv");
   procedure Get_Tex_Parameter_Floats
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Values     : in out Low_Level.Single_Array) is
     Static ("glGetTexParameterfv");
   procedure Get_Tex_Parameter_Int (Target     : Low_Level.Enums.Texture_Kind;
                                    Param_Name : Enums.Textures.Parameter;
                                    Values     : out Int) is
     Static ("glGetTexParameteriv");
   procedure Get_Tex_Parameter_Ints (Target     : Low_Level.Enums.Texture_Kind;
                                     Param_Name : Enums.Textures.Parameter;
                                     Values     : in out Low_Level.Int_Array) is
     Static ("glGetTexParameteriv");
   procedure Get_Tex_Parameter_Wrap_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Values     : out Objects.Textures.Wrapping_Mode) is
     Static ("glGetTexParameteriv");
   procedure Get_Tex_Parameter_Comp_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Values     : out Enums.Textures.Compare_Kind) is
     Static ("glGetTexParameteriv");
   procedure Get_Tex_Parameter_Comp_Func
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Values     : out Compare_Function) is Static ("glGetTexParameteriv");
   procedure Get_Tex_Parameter_Depth_Mode
     (Target     : Low_Level.Enums.Texture_Kind;
      Param_Name : Enums.Textures.Parameter;
      Values     : out Objects.Textures.Depth_Mode) is
     Static ("glGetTexParameteriv");
   procedure Get_Tex_Parameter_Bool (Target     : Low_Level.Enums.Texture_Kind;
                                     Param_Name : Enums.Textures.Parameter;
                                     Values     : out Low_Level.Bool) is
     Static ("glGetTexParameteriv");
   procedure Get_Tex_Level_Parameter_Size
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Size) is Static ("glGetTexLevelParameteriv");
   procedure Get_Tex_Level_Parameter_Format
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Pixels.Internal_Format) is
     Static ("glGetTexLevelParameteriv");
   procedure Get_Tex_Level_Parameter_Type
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Pixels.Channel_Data_Type) is
     Static ("glGetTexLevelParameteriv");
   procedure Get_Tex_Level_Parameter_Bool
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Low_Level.Bool) is Static ("glGetTexLevelParameteriv");
   procedure Gen_Textures (N : Size; Textures : access UInt) is
     Static ("glGenTextures");
   procedure Bind_Texture (Target  : Low_Level.Enums.Texture_Kind;
                           Texture : UInt) is Static ("glBindTexture");
   procedure Delete_Textures (N : Size; Textures : Low_Level.UInt_Array) is
     Static ("glDeleteTextures");
   procedure Tex_Image_1D (Target : Low_Level.Enums.Texture_Kind;
                           Level  : Objects.Textures.Mipmap_Level;
                           Internal_Format : Pixels.Internal_Format;
                           Width  : Size;
                           Border : Low_Level.Zero;
                           Format : Pixels.Data_Format;
                           Data_Type : Pixels.Data_Type;
                           Data   : System.Address) is Static ("glTexImage1D");
   procedure Compressed_Tex_Image_1D
     (Target          : Low_Level.Enums.Texture_Kind;
      Level           : Objects.Textures.Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width           : Size;
      Border          : Low_Level.Zero;
      Image_Size      : Size;
      Data            : System.Address) is Dynamic ("glCompressedTexImage1D");
   procedure Tex_Image_2D (Target : Low_Level.Enums.Texture_Kind;
                           Level  : Objects.Textures.Mipmap_Level;
                           Internal_Format : Pixels.Internal_Format;
                           Width, Height : Size;
                           Border : Low_Level.Zero;
                           Format : Pixels.Data_Format;
                           Data_Type : Pixels.Data_Type;
                           Data : System.Address) is Static ("glTexImage2D");
   procedure Compressed_Tex_Image_2D
     (Target          : Low_Level.Enums.Texture_Kind;
      Level           : Objects.Textures.Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width, Height   : Size;
      Border          : Low_Level.Zero;
      Image_Size      : Size;
      Data            : System.Address) is Dynamic ("glCompressedTexImage2D");
   procedure Tex_Image_3D (Target : Low_Level.Enums.Texture_Kind;
                           Level  : Objects.Textures.Mipmap_Level;
                           Internal_Format : Pixels.Internal_Format;
                           Width, Height, Depth : Size;
                           Border : Low_Level.Zero;
                           Format : Pixels.Data_Format;
                           Data_Type : Pixels.Data_Type;
                           Data : System.Address) is Dynamic ("glTexImage3D");
   procedure Compressed_Tex_Image_3D
     (Target          : Low_Level.Enums.Texture_Kind;
      Level           : Objects.Textures.Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width, Height, Depth : Size;
      Border          : Low_Level.Zero;
      Image_Size      : Size;
      Data            : System.Address) is Dynamic ("glCompressedTexImage3D");
   procedure Tex_Env_Float (Target     : Enums.Textures.Env_Target;
                            Param_Name : Enums.Textures.Env_Parameter;
                            Value      : Single) is Static ("glTexEnvf");
   procedure Tex_Env_Int (Target     : Enums.Textures.Env_Target;
                          Param_Name : Enums.Textures.Env_Parameter;
                          Value      : Int) is Static ("glTexEnvi");
   procedure Tex_Env_Tex_Func (Target     : Enums.Textures.Env_Target;
                               Param_Name : Enums.Textures.Env_Parameter;
                               Value      : Fixed.Textures.Texture_Function) is
     Static ("glTexEnvi");
   procedure Tex_Env_Combine_Func
     (Target     : Enums.Textures.Env_Target;
      Param_Name : Enums.Textures.Env_Parameter;
      Value      : Fixed.Textures.Combine_Function) is Static ("glTexEnvi");
   procedure Tex_Env_Source (Target     : Enums.Textures.Env_Target;
                             Param_Name : Enums.Textures.Env_Parameter;
                             Value      : Fixed.Textures.Source_Kind) is
     Static ("glTexEnvi");
   procedure Tex_Env_Arr (Target     : Enums.Textures.Env_Target;
                          Param_Name : Enums.Textures.Env_Parameter;
                          Value      : Low_Level.Single_Array) is
     Static ("glTexEnvfv");
   procedure Tex_Env_Bool (Target     : Enums.Textures.Env_Target;
                           Param_Name : Enums.Textures.Env_Parameter;
                           Value      : Low_Level.Bool) is Static ("glTexEnvi");
   procedure Get_Tex_Env_Float (Target     : Enums.Textures.Env_Target;
                                Param_Name : Enums.Textures.Env_Parameter;
                                Value      : out Single) is
     Static ("glGetTexEnvfv");
   procedure Get_Tex_Env_Tex_Func
     (Target     : Enums.Textures.Env_Target;
      Param_Name : Enums.Textures.Env_Parameter;
      Value      : out Fixed.Textures.Texture_Function) is
     Static ("glGetTexEnviv");
   procedure Get_Tex_Env_Combine_Func
     (Target     : Enums.Textures.Env_Target;
      Param_Name : Enums.Textures.Env_Parameter;
      Value      : out Fixed.Textures.Combine_Function) is
     Static ("glGetTexEnviv");
   procedure Get_Tex_Env_Source (Target     : Enums.Textures.Env_Target;
                                 Param_Name : Enums.Textures.Env_Parameter;
                                 Value      : out Fixed.Textures.Source_Kind) is
     Static ("glGetTexEnviv");
   procedure Get_Tex_Env_Arr (Target     : Enums.Textures.Env_Target;
                              Param_Name : Enums.Textures.Env_Parameter;
                              Value      : in out Low_Level.Single_Array) is
     Static ("glGetTexEnvfv");
   procedure Get_Tex_Env_Bool (Target     : Enums.Textures.Env_Target;
                               Param_Name : Enums.Textures.Env_Parameter;
                               Value      : out Low_Level.Bool) is
     Static ("glGetTexEnviv");
   procedure Active_Texture (Texture : Int) is Dynamic ("glActiveTexture");
   procedure Generate_Mipmap (Target : Low_Level.Enums.Texture_Kind) is
     Dynamic ("glGenerateMipmap");
   procedure Invalidate_Tex_Image
     (Texture : UInt; Level : Objects.Textures.Mipmap_Level) is
     Dynamic ("glInvalidateTexImage");
   procedure Invalidate_Tex_Sub_Image
     (Texture : UInt; Level : Objects.Textures.Mipmap_Level;
      X_Offset, Y_Offset, Z_Offset : Int; Width, Height, Depth : Size) is
     Dynamic ("glInvalidateTexSubImage");
   procedure Tex_Storage_1D
     (Target : Low_Level.Enums.Texture_Kind; Levels : Size;
      Internal_Format : Pixels.Internal_Format;
      Width : Size) is Dynamic ("glTexStorage1D");
   procedure Tex_Storage_2D
     (Target : Low_Level.Enums.Texture_Kind; Levels : Size;
      Internal_Format : Pixels.Internal_Format;
      Width, Height : Size) is Dynamic ("glTexStorage2D");
   procedure Tex_Storage_3D
     (Target : Low_Level.Enums.Texture_Kind; Levels : Size;
      Internal_Format : Pixels.Internal_Format;
      Width, Height, Depth : Size) is Dynamic ("glTexStorage3D");

   -----------------------------------------------------------------------------
   --                             Buffer Objects                              --
   -----------------------------------------------------------------------------

   procedure Gen_Buffers (N : Size; Buffers : out UInt) is
     Dynamic ("glGenBuffers");
   procedure Delete_Buffers (N : Size; Buffers : Low_Level.UInt_Array) is
     Dynamic ("glDeleteBuffers");
   procedure Bind_Buffer (Target : Low_Level.Enums.Buffer_Kind; Buffer : UInt)
     is Dynamic ("glBindBuffer");
   procedure Buffer_Data (Target : Low_Level.Enums.Buffer_Kind;
     Size : Low_Level.SizeIPtr; Data : System.Address;
     Usage : Objects.Buffers.Buffer_Usage) is Dynamic ("glBufferData");
   function Map_Buffer (Target : Low_Level.Enums.Buffer_Kind;
                        Acc : Objects.Access_Kind) return System.Address is
     Dynamic ("glMapBuffer");
   procedure Buffer_Pointer (Target : Low_Level.Enums.Buffer_Kind;
                             Pname  : Enums.Buffer_Pointer_Param;
                             Params : out System.Address) is
     Dynamic ("glGetBufferPointerv");
   procedure Buffer_Sub_Data (Target : Low_Level.Enums.Buffer_Kind;
                              Offset : Low_Level.IntPtr;
                              Size   : Low_Level.SizeIPtr;
                              Data   : System.Address) is
     Dynamic ("glBufferSubData");

   -- glMapBuffer: returns instance of generic Interfaces.C.Pointers.Pointer,
   -- therefore declared in GL.Objects.Buffers

   procedure Unmap_Buffer (Target : Low_Level.Enums.Buffer_Kind) is
     Dynamic ("glUnmapBuffer");
   procedure Get_Buffer_Parameter_Access_Kind
     (Target : Low_Level.Enums.Buffer_Kind; Value : Enums.Buffer_Param;
      Data   : out Objects.Access_Kind) is Dynamic ("glGetBufferParameteriv");
   procedure Get_Buffer_Parameter_Bool
     (Target : Low_Level.Enums.Buffer_Kind; Value : Enums.Buffer_Param;
      Data   : out Low_Level.Bool) is Dynamic ("glGetBufferParameteriv");
   procedure Get_Buffer_Parameter_Size
     (Target : Low_Level.Enums.Buffer_Kind; Value : Enums.Buffer_Param;
      Data   : out Size) is Dynamic ("glGetBufferParameteriv");
   procedure Get_Buffer_Parameter_Usage
     (Target : Low_Level.Enums.Buffer_Kind; Value : Enums.Buffer_Param;
      Data   : out Objects.Buffers.Buffer_Usage) is
     Dynamic ("glGetBufferParameteriv");
   procedure Invalidate_Buffer_Data (Buffer : UInt) is
     Dynamic ("glInvalidateBufferData");
   procedure Invalidate_Buffer_Sub_Data
     (Buffer : UInt; Offset : Low_Level.IntPtr; Length : Low_Level.SizeIPtr) is
     Dynamic ("glInvalidateBufferSubData");

   -----------------------------------------------------------------------------
   --                           Vertex Array Objects                          --
   -----------------------------------------------------------------------------

   procedure Gen_Vertex_Arrays (N : Size; Arrays : out UInt) is
     Dynamic ("glGenVertexArrays");
   procedure Delete_Vertex_Arrays (N : Size; Arrays : Low_Level.UInt_Array) is
     Dynamic ("glDeleteVertexArrays");
   procedure Bind_Vertex_Array (Arr : UInt) is Dynamic ("glBindVertexArray");

   -----------------------------------------------------------------------------
   --                        Renderbuffer objects                             --
   -----------------------------------------------------------------------------

   procedure Gen_Renderbuffers (N : Size; Renderbuffers : out UInt) is
     Dynamic ("glGenRenderbuffers");
   procedure Delete_Renderbuffers
     (N : Size; Renderbuffers : Low_Level.UInt_Array) is
     Dynamic ("glDeleteBuffers");
   procedure Renderbuffer_Storage
     (Target : Low_Level.Enums.Renderbuffer_Kind;
      Internal_Format : Pixels.Internal_Format; Width, Height : Size) is
     Dynamic ("glRenderbufferStorage");
   procedure Renderbuffer_Storage_Multisample
     (Target : Low_Level.Enums.Renderbuffer_Kind; Samples : Size;
      Internal_Format : Pixels.Internal_Format; Width, Height : Size) is
     Dynamic ("glRenderbufferStorageMultisample");
   procedure Bind_Renderbuffer (Target : Low_Level.Enums.Renderbuffer_Kind;
                                Renderbuffer : UInt) is
     Dynamic ("glBindRenderbuffer");
   procedure Get_Renderbuffer_Parameter_Int
     (Target : Low_Level.Enums.Renderbuffer_Kind;
      Pname  : Enums.Getter.Renderbuffer_Parameter; Params : out Int) is
     Dynamic ("glGetRenderbufferParameteriv");
   procedure Get_Renderbuffer_Parameter_Internal_Format
     (Target : Low_Level.Enums.Renderbuffer_Kind;
      Pname  : Enums.Getter.Renderbuffer_Parameter;
      Params : out Pixels.Internal_Format) is
     Dynamic ("glGetRenderbufferParameteriv");

   -----------------------------------------------------------------------------
   --                  Framebuffer objects and handling                       --
   -----------------------------------------------------------------------------

   procedure Read_Pixels (X, Y : Int; Width, Height : Size;
                          Format : Pixels.Framebuffer_Format;
                          Data_Type : Pixels.Data_Type;
                          Data : System.Address) is Static ("glReadPixels");
   procedure Logic_Op (Value : Framebuffer.Logic_Op) is Static ("glLogicOp");
   procedure Clamp_Color (Target: Enums.Clamp_Color_Param;
                          Clamp: Low_Level.Bool) is Dynamic ("glClampColor");
   procedure Read_Buffer (Value : Framebuffer.Read_Buffer_Selector) is
     Static ("glReadBuffer");
   procedure Gen_Framebuffers (N : Size; Framebuffers : out UInt) is
     Dynamic ("glGenFramebuffers");
   procedure Delete_Framebuffers
     (N : Size; Framebuffers : Low_Level.UInt_Array) is
     Dynamic ("glDeleteFramebuffers");
   procedure Bind_Framebuffer
     (Target : Low_Level.Enums.Framebuffer_Kind; Framebuffer : UInt) is
     Dynamic ("glBindFramebuffer");
   function Check_Framebuffer_Status (Target : Low_Level.Enums.Framebuffer_Kind)
     return Objects.Framebuffers.Framebuffer_Status is
     Dynamic ("glCheckFramebufferStatus");
   procedure Framebuffer_Renderbuffer
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Attachment : Objects.Framebuffers.Attachment_Point;
      Renderbuffer_Target : Low_Level.Enums.Renderbuffer_Kind;
      Renderbuffer : UInt) is Dynamic ("glFramebufferRenderbuffer");
   procedure Framebuffer_Texture
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Attachment : Objects.Framebuffers.Attachment_Point;
      Texture : UInt; Level : Objects.Textures.Mipmap_Level) is
     Dynamic ("glFramebufferTexture");
   procedure Framebuffer_Texture_Layer
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Attachment : Objects.Framebuffers.Attachment_Point; Texture : UInt;
      Level : Objects.Textures.Mipmap_Level; Layer : Int) is
     Dynamic ("glFramebufferTextureLayer");
   procedure Blit_Framebuffer
     (Src_X0, Src_X1, Src_Y0, Src_X1, Dst_X0, Dst_X1, Dst_Y0, Dst_Y1 : Int;
      Mask : Low_Level.Bitfield; Filter : Objects.Textures.Magnifying_Function)
     is Dynamic ("glBlitFramebuffer");
   procedure Invalidate_Framebuffer
     (Target : Low_Level.Enums.Framebuffer_Kind; Num_Attachments : Size;
      Attachments : Objects.Framebuffers.Attachment_List) is
     Dynamic ("glInvalidateFramebuffer");
   procedure Invalidate_Sub_Framebuffer
     (Target : Low_Level.Enums.Framebuffer_Kind; Num_Attachments : Size;
      Attachments : Objects.Framebuffers.Attachment_List; X, Y : Int;
      Width, Height : Size) is Dynamic ("glInvalidateSubFramebuffer");
   procedure Framebuffer_Parameter_Size
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Pname  : Enums.Framebuffer_Param; Param : Size) is
     Dynamic ("glFramebufferParameteri");
   procedure Framebuffer_Parameter_Bool
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Pname  : Enums.Framebuffer_Param; Param : Low_Level.Bool) is
     Dynamic ("glFramebufferParameteri");
   procedure Get_Framebuffer_Parameter_Size
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Pname  : Enums.Framebuffer_Param; Params : out Size) is
     Dynamic ("glGetFramebufferParameteriv");
   procedure Get_Framebuffer_Parameter_Bool
     (Target : Low_Level.Enums.Framebuffer_Kind;
      Pname  : Enums.Framebuffer_Param; Params : out Low_Level.Bool) is
     Dynamic ("glGetFramebufferParameteriv");

   -----------------------------------------------------------------------------
   --                                 Shaders                                 --
   -----------------------------------------------------------------------------

   procedure Get_Shader_Param (Shader : UInt; Pname : Enums.Shader_Param;
                               Param : out Int) is Dynamic ("glGetShaderiv");
   procedure Get_Shader_Type (Shader : UInt; Pname : Enums.Shader_Param;
     Param : out Objects.Shaders.Shader_Type) is Dynamic ("glGetShaderiv");
   function Create_Shader (Shader_Type : Objects.Shaders.Shader_Type)
     return Uint is Dynamic ("glCreateShader");
   procedure Delete_Shader (Shader : UInt) is Dynamic ("glDeleteShader");
   procedure Shader_Source (Shader : UInt; Count : Size;
     Str : Low_Level.CharPtr_Array; Length : Low_Level.Int_Array) is
     Dynamic ("glShaderSource");
   procedure Get_Shader_Source (Shader : UInt; Buffer_Size : Size;
                                Length : out Size; Value : in out String) is
     Dynamic ("glGetShaderSource");
   procedure Compile_Shader (Shader : UInt) is Dynamic ("glCompileShader");
   procedure Release_Shader_Compiler is Dynamic ("glReleaseShaderCompiler");
   procedure Get_Shader_Info_Log (Shader : UInt; Buffer_Size : Size;
                                  Length : out Size; Value : in out String) is
     Dynamic ("glGetShaderInfoLog");
   function Create_Program return UInt is Dynamic ("glCreateProgram");
   procedure Delete_Program (Program : UInt) is Dynamic ("glDeleteProgram");
   procedure Get_Program_Param (Program : UInt; Pname : Enums.Program_Param;
                                Params : out Int) is Dynamic ("glGetProgramiv");
   procedure Attach_Shader (Program, Shader : UInt) is
     Dynamic ("glAttachShader");
   procedure Link_Program (Program : UInt) is Dynamic ("glLinkProgram");
   procedure Get_Program_Info_Log (Program : UInt; Buffer_Size : Size;
                                   Length : out Size; Value : in out String) is
     Dynamic ("glGetProgramInfoLog");
   procedure Get_Program_Stage
     (Program : UInt; Shader_Type : Objects.Shaders.Shader_Type;
      Pname   : Enums.Program_Stage_Param; Params : out Size) is
     Dynamic ("glGetProgramStageiv");
   function Get_Subroutine_Index
     (Program : UInt; Shader_Type : Objects.Shaders.Shader_Type;
      Name : Interfaces.C.char_array)
     return Objects.Programs.Subroutine_Index_Type is
     Dynamic ("glGetSubroutineIndex");
   function Get_Subroutine_Uniform_Location
     (Program : UInt; Shader_Type : Objects.Shaders.Shader_Type;
      Name : Interfaces.C.char_array)
     return Objects.Programs.Uniform_Location_Type is
     Dynamic ("glGetSubroutineUniformLocation");
   procedure Use_Program (Program : UInt) is Dynamic ("glUseProgram");
   procedure Validate_Program (Program : UInt) is Dynamic ("glValidateProgram");
   function Get_Uniform_Location
     (Program : UInt; Name : C.char_array) return Uniforms.Uniform is
     Dynamic ("glGetUniformLocation");
   procedure Bind_Attrib_Location
     (Program : UInt; Index : Attributes.Attribute; Name : C.char_array) is
     Dynamic ("glBindAttribLocation");
   function Get_Attrib_Location
     (Program : UInt; Name : C.char_array) return Attributes.Attribute is
     Dynamic ("glGetAttribLocation");
   procedure Vertex_Attrib_Pointer
     (Index : Attributes.Attribute; Size : Component_Count;
      N_Type : Numeric_Type; Normalized : Low_Level.Bool; Stride : Size;
      Pointer : Int) is Dynamic ("glVertexAttribPointer");
   procedure Vertex_AttribI_Pointer
     (Index : Attributes.Attribute; Size : Component_Count;
      N_Type :Numeric_Type; Stride : Size; Pointer : Int) is
     Dynamic ("glVertexAttribIPointer");
   procedure Vertex_AttribL_Pointer
     (Index : Attributes.Attribute; Size : Component_Count;
      N_Type :Numeric_Type; Stride : Size; Pointer : Int) is
     Dynamic ("glVertexAttribLPointer");
   procedure Enable_Vertex_Attrib_Array (Index : Attributes.Attribute) is
     Dynamic ("glEnableVertexAttribArray");
   procedure Disable_Vertex_Attrib_Array (Index : Attributes.Attribute) is
     Dynamic ("glDisableVertexAttribArray");
   procedure Get_Attached_Shaders
     (Program : UInt; Max_Count : Size; Count : out Size;
      Shaders : in out UInt_Array) is Dynamic ("glGetAttachedShaders");

   -----------------------------------------------------------------------------
   --                              Tessellation                               --
   -----------------------------------------------------------------------------

   procedure Set_Patch_Parameter_Int
     (Pname : Enums.Patch_Parameter_Int; Value : Int) is
     Dynamic ("glPatchParameteri");
   procedure Set_Patch_Parameter_Float_Array
     (Pname : Enums.Patch_Parameter_Float_Array; Value : Types.Single_Array) is
     Dynamic ("glPatchParameterfv");

   -----------------------------------------------------------------------------
   --                  Transformation to window coordinates                   --
   -----------------------------------------------------------------------------

   procedure Depth_Range (Near, Far : Double) is Static ("glDepthRange");
   procedure Viewport (X, Y : Int; Width, Height : Size) is
     Static ("glViewport");
end GL.API;