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

with GL.Runtime_Loading;

with GL.Attributes;
with GL.Blending;
with GL.Buffers;
with GL.Enums.Getter;
with GL.Enums.Textures;
with GL.Fixed.Textures;
with GL.Fixed.Lighting;
with GL.Low_Level.Enums;
with GL.Objects.Textures.Loader_2D;
with GL.Objects.Buffer;
with GL.Objects.Framebuffers;
with GL.Objects.Shaders;
with GL.Pixel_Data;
with GL.Toggles;
with GL.Types.Colors;
with GL.Uniforms;

with Interfaces.C.Strings;

with System;

private package GL.API is
   pragma Preelaborate;

   use GL.Types;

   -- implementation is platform-specific. Therefore, gl-api.adb is in the
   -- platform-specific source folders.
   function GL_Subprogram_Reference (Function_Name : String) return System.Address;
   
   package Loader is new Runtime_Loading (GL_Subprogram_Reference);

   -- Everything newer than OpenGL 1.1 will not be statically bound,
   -- but loaded with GL.Low_Level.Loader at runtime.
   --
   -- Also, all functions that have been deprecated with OpenGL 3.0
   -- will not be statically bound, as they may be omitted by implementors
   -- when they choose to only implement the OpenGL Core Profile.
   


   subtype Zero is Int range 0 .. 0;

   function Get_Error return Enums.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Error,
                  External_Name => "glGetError");

   procedure Flush;
   pragma Import (Convention => StdCall, Entity => Flush,
                  External_Name => "glFlush");

   procedure Finish;
   pragma Import (Convention => StdCall, Entity => Finish,
                  External_Name => "glFinish");

   -----------------------------------------------------------------------------
   --                           Parameter Getters                             --
   -----------------------------------------------------------------------------

   procedure Get_Boolean (Name   : Enums.Getter.Parameter;
                          Target : access Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Get_Boolean,
                  External_Name => "glGetBooleanv");

   procedure Get_Double (Name   : Enums.Getter.Parameter;
                         Target : access Double);
   pragma Import (Convention => StdCall, Entity => Get_Double,
                  External_Name => "glGetDoublev");

   
   procedure Get_Double_Vec2 (Name   : Enums.Getter.Parameter;
                              Target : in out Doubles.Vector2);
   pragma Import (Convention => StdCall, Entity => Get_Double_Vec2,
                  External_Name => "glGetDoublev");
   
   procedure Get_Color (Name : Enums.Getter.Parameter;
                        Target : in out Colors.Color);
   pragma Import (Convention => StdCall, Entity => Get_Color,
                  External_Name => "glGetFloatv");

   procedure Get_Integer (Name   : Enums.Getter.Parameter;
                          Target : access Int);
   pragma Import (Convention => StdCall, Entity => Get_Integer,
                  External_Name => "glGetIntegerv");
   
   procedure Get_Int_Vec4 (Name   : Enums.Getter.Parameter;
                           Target : in out Ints.Vector4);
   pragma Import (Convention => StdCall, Entity => Get_Int_Vec4,
                  External_Name => "glGetIntegerv");

   procedure Get_Color_Control (Name   : Enums.Getter.Parameter;
                                Target : access Fixed.Lighting.Color_Control);
   pragma Import (Convention => StdCall, Entity => Get_Color_Control,
                  External_Name => "glGetIntegerv");

   procedure Get_Shade_Model (Name   : Enums.Getter.Parameter;
                              Target : access Fixed.Lighting.Shade_Model);
   pragma Import (Convention => StdCall, Entity => Get_Shade_Model,
                  External_Name => "glGetIntegerv");

   procedure Get_Light_Color (Name   : Enums.Light_Name;
                              Pname  : Enums.Light_Param;
                              Target : in out Colors.Color);
   pragma Import (Convention => StdCall, Entity => Get_Light_Color,
                  External_Name => "glGetLightfv");
   
   function Get_String (Name : Enums.Getter.String_Parameter) 
                        return C.Strings.chars_ptr;  
   pragma Import (Convention => StdCall, Entity => Get_String,
                  External_Name => "glGetString");
   
   function Get_String_I is new Loader.Function_With_2_Params
     ("glGetStringi", Enums.Getter.String_Parameter, UInt, C.Strings.chars_ptr);

   -----------------------------------------------------------------------------
   --                                 Toggles                                 --
   -----------------------------------------------------------------------------

   procedure Enable (Subject : Toggles.Toggle);
   pragma Import (Convention => StdCall, Entity => Enable,
                  External_Name => "glEnable");

   procedure Disable (Subject : Toggles.Toggle);
   pragma Import (Convention => StdCall, Entity => Disable,
                  External_Name => "glDisable");

   function Is_Enabled (Subject : Toggles.Toggle) return Low_Level.Bool;
   pragma Import (Convention => StdCall, Entity => Is_Enabled,
                  External_Name => "glIsEnabled");

   -----------------------------------------------------------------------------
   --            Matrix stack API (deprecated as of OpenGL 3.0)               --
   -----------------------------------------------------------------------------

   procedure Matrix_Mode (Mode : Enums.Matrix_Mode);
   pragma Import (Convention => StdCall, Entity => Matrix_Mode,
                  External_Name => "glMatrixMode");

   procedure Frustum (Left, Right, Bottom, Top, zNear, zFar : Double);
   pragma Import (Convention => StdCall, Entity => Frustum,
                  External_Name => "glFrustum");

   procedure Ortho (Left, Right, Bottom, Top, zNear, zFar : Double);
   pragma Import (Convention => StdCall, Entity => Ortho,
                  External_Name => "glOrtho");

   procedure Load_Identity;
   pragma Import (Convention => StdCall, Entity => Load_Identity,
                  External_Name => "glLoadIdentity");

   procedure Push_Matrix;
   pragma Import (Convention => StdCall, Entity => Push_Matrix,
                  External_Name => "glPushMatrix");

   procedure Pop_Matrix;
   pragma Import (Convention => StdCall, Entity => Pop_Matrix,
                  External_Name => "glPopMatrix");

   procedure Rotate (Angle, X, Y, Z : Double);
   pragma Import (Convention => StdCall, Entity => Rotate,
                  External_Name => "glRotated");

   procedure Scale (X, Y, Z : Double);
   pragma Import (Convention => StdCall, Entity => Scale,
                  External_Name => "glScaled");

   procedure Translate (X, Y, Z : Double);
   pragma Import (Convention => StdCall, Entity => Translate,
                  External_Name => "glTranslated");

   -----------------------------------------------------------------------------
   --              Immediate API (deprecated as of OpenGL 3.0)                --
   -----------------------------------------------------------------------------

   procedure GL_Begin (Mode : Connection_Mode);
   pragma Import (Convention => StdCall, Entity => GL_Begin,
                  External_Name => "glBegin");

   procedure GL_End;
   pragma Import (Convention => StdCall, Entity => GL_End,
                  External_Name => "glEnd");

   procedure Color (Value : Colors.Color);
   pragma Import (Convention => StdCall, Entity => Color,
                  External_Name => "glColor4fv");

   procedure Secondary_Color is new Loader.Procedure_With_1_Param
      ("glSecondaryColor3dv", Colors.Basic_Color);

   procedure Fog_Coord is new Loader.Procedure_With_1_Param
      ("glFogCoordd", Double);

   -----------------------------------------------------------------------------
   --        Fixed Function Pipeline (deprecated as of OpenGL 3.0)            --
   -----------------------------------------------------------------------------

   procedure Vertex_Pointer (Count     : Int;
                             Data_Type : Signed_Numeric_Type;
                             Stride    : Size;
                             Pointer   : Int);
   pragma Import (Convention => StdCall, Entity => Vertex_Pointer,
                  External_Name => "glVertexPointer");

   procedure Index_Pointer (Data_Type : Signed_Numeric_Type;
                            Stride    : Size;
                            Pointer   : Int);
   pragma Import (Convention => StdCall, Entity => Index_Pointer,
                  External_Name => "glIndexPointer");

   procedure Color_Pointer (Count     : Int;
                            Data_Type : Signed_Numeric_Type;
                            Stride    : Size;
                            Pointer   : Int);
   pragma Import (Convention => StdCall, Entity => Color_Pointer,
                  External_Name => "glColorPointer");

   procedure Enable_Client_State (Cap : Fixed.Client_Side_Capability);
   pragma Import (Convention => StdCall, Entity => Enable_Client_State,
                  External_Name => "glEnableClientState");

   procedure Disable_Client_State (Cap : Fixed.Client_Side_Capability);
   pragma Import (Convention => StdCall, Entity => Disable_Client_State,
                  External_Name => "glDisableClientState");

   procedure Draw_Arrays (Mode  : Connection_Mode;
                          First : Int; Count : Size);
   pragma Import (Convention => StdCall, Entity => Draw_Arrays,
                  External_Name => "glDrawArrays");

   procedure Draw_Elements (Mode       : Connection_Mode;
                            Count      : Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Indices    : Zero);
   pragma Import (Convention => StdCall, Entity => Draw_Elements,
                  External_Name => "glDrawElements");

   -----------------------------------------------------------------------------
   --               Lighting API (deprecated as of OpenGL 3.0)                --
   -----------------------------------------------------------------------------

   procedure Light_Model_Color (Param : Enums.Light_Model_Ambient_Parameter;
                                Color : Colors.Color);
   pragma Import (Convention => StdCall, Entity => Light_Model_Color,
                  External_Name => "glLightModelfv");

   procedure Light_Model_Color_Control (Param : Enums.Light_Model_CC_Parameter;
     Value : access constant Fixed.Lighting.Color_Control);
   pragma Import (Convention => StdCall, Entity => Light_Model_Color_Control,
                  External_Name => "glLightModeliv");

   procedure Light_Model_Toggles (Param : Enums.Light_Model_Toggle_Parameter;
                                  Value : access constant Int);
   pragma Import (Convention => StdCall, Entity => Light_Model_Toggles,
                  External_Name => "glLightModeliv");

   procedure Shade_Model (Mode : Fixed.Lighting.Shade_Model);
   pragma Import (Convention => StdCall, Entity => Shade_Model,
                  External_Name => "glShadeModel");

   procedure Light_Color (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
                          Param : Colors.Color);
   pragma Import (Convention => StdCall, Entity => Light_Color,
                  External_Name => "glLightfv");
   
   -----------------------------------------------------------------------------
   --                               Blending                                  --
   -----------------------------------------------------------------------------

   procedure Blend_Func (Src_Factor, Dst_Factor : Blending.Blend_Factor);
   pragma Import (Convention => StdCall, Entity => Blend_Func,
                  External_Name => "glBlendFunc");
   
   procedure Blend_Func_I (Buf : Buffers.Draw_Buffer_Index;
                           Src_Factor, Dst_Factor : Blending.Blend_Factor);
   pragma Import (Convention => StdCall, Entity => Blend_Func_I,
                  External_Name => "glBlendFunci");
   
   -----------------------------------------------------------------------------
   --                                Buffers                                  --
   -----------------------------------------------------------------------------

   procedure Clear (Bits : Low_Level.Bitfield);
   pragma Import (Convention => StdCall, Entity => Clear,
                  External_Name => "glClear");

   procedure Draw_Buffer (Mode : Buffers.Explicit_Color_Buffer_Selector);
   pragma Import (Convention => StdCall, Entity => Draw_Buffer,
                  External_Name => "glDrawBuffer");
   
   procedure Draw_Buffers is new Loader.Procedure_With_2_Params
     ("glDrawBuffers", UInt, Buffers.Explicit_Color_Buffer_List);

   procedure Clear_Color (Red, Green, Blue, Alpha : Colors.Component);
   pragma Import (Convention => StdCall, Entity => Clear_Color,
                  External_Name => "glClearColor");

   procedure Clear_Depth (Depth : Buffers.Depth);
   pragma Import (Convention => StdCall, Entity => Clear_Depth,
                  External_Name => "glClearDepth");

   procedure Clear_Stencil (Index : Buffers.Stencil_Index);
   pragma Import (Convention => StdCall, Entity => Clear_Stencil,
                  External_Name => "glClearStencil");

   -- dropped in OpenGL 3
   procedure Clear_Accum is new Loader.Procedure_With_4_Params
      ("glClearAccum", Colors.Component, Colors.Component, Colors.Component,
       Colors.Component);

   procedure Clear_Buffer is new Loader.Procedure_With_3_Params
     ("glClearBufferfv", Buffers.Color_Buffer_Selector, Zero, Colors.Color);

   procedure Clear_Draw_Buffer is new Loader.Procedure_With_3_Params
     ("glClearBufferfv", Low_Level.Enums.Only_Color_Buffer,
      Buffers.Draw_Buffer_Index, Colors.Color);
   
   type Depth_Pointer is access constant Buffers.Depth;
   procedure Clear_Buffer_Depth is new Loader.Procedure_With_3_Params
     ("glClearBufferfv", Low_Level.Enums.Only_Depth_Buffer, Zero,
      Depth_Pointer);

   type Stencil_Pointer is access constant Buffers.Stencil_Index;
   procedure Clear_Buffer_Stencil is new Loader.Procedure_With_3_Params
     ("glClearBufferiv", Low_Level.Enums.Only_Stencil_Buffer, Zero,
      Stencil_Pointer);

   procedure Clear_Buffer_Depth_Stencil is new Loader.Procedure_With_4_Params
     ("glClearBufferfi", Low_Level.Enums.Only_Depth_Stencil_Buffer, Zero,
      Buffers.Depth, Buffers.Stencil_Index);

   -----------------------------------------------------------------------------
   --                                Textures                                 --
   -----------------------------------------------------------------------------

   procedure Tex_Parameter_Float (Target     : Low_Level.Enums.Texture_Kind;
                                  Param_Name : Enums.Textures.Parameter;
                                  Value      : Single);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Float,
                  External_Name => "glTexParameterf");

   procedure Tex_Parameter_Int (Target     : Low_Level.Enums.Texture_Kind;
                                Param_Name : Enums.Textures.Parameter;
                                Value      : Int);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Int,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Min_Filter (Target     : Low_Level.Enums.Texture_Kind;
                                       Param_Name : Enums.Textures.Parameter;
                                       Value      : Objects.Textures.Minifying_Function);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Min_Filter,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Mag_Filter (Target     : Low_Level.Enums.Texture_Kind;
                                       Param_Name : Enums.Textures.Parameter;
                                       Value      : Objects.Textures.Magnifying_Function);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Mag_Filter,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Wrap_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : Objects.Textures.Wrapping_Mode);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Wrap_Mode,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Comp_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : Enums.Textures.Compare_Kind);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Comp_Mode,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Comp_Func (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : Objects.Textures.Compare_Function);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Comp_Func,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Depth_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                       Param_Name : Enums.Textures.Parameter;
                                       Value      : Objects.Textures.Depth_Mode);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Depth_Mode,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Bool (Target     : Low_Level.Enums.Texture_Kind;
                                 Param_Name : Enums.Textures.Parameter;
                                 Value      : Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Bool,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Floats (Target     : Low_Level.Enums.Texture_Kind;
                                   Param_Name : Enums.Textures.Parameter;
                                   Values     : Low_Level.Single_Array);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Floats,
                  External_Name => "glTexParameterfv");

   procedure Get_Tex_Parameter_Float (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Values     : in out Low_Level.Single_Array);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Float,
                  External_Name => "glGetTexParameterfv");

   procedure Get_Tex_Parameter_Int (Target     : Low_Level.Enums.Texture_Kind;
                                    Param_Name : Enums.Textures.Parameter;
                                    Values     : in out Low_Level.Int_Array);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Int,
                  External_Name => "glGetTexParameteriv");

   procedure Get_Tex_Parameter_Wrap_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                          Param_Name : Enums.Textures.Parameter;
                                          Values     : out Objects.Textures.Wrapping_Mode);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Wrap_Mode,
                  External_Name => "glGetTexParameteriv");

   procedure Get_Tex_Parameter_Comp_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                          Param_Name : Enums.Textures.Parameter;
                                          Values     : out Enums.Textures.Compare_Kind);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Comp_Mode,
                  External_Name => "glGetTexParameteriv");

   procedure Get_Tex_Parameter_Comp_Func (Target     : Low_Level.Enums.Texture_Kind;
                                          Param_Name : Enums.Textures.Parameter;
                                          Values     : out Objects.Textures.Compare_Function);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Comp_Func,
                  External_Name => "glGetTexParameteriv");

   procedure Get_Tex_Parameter_Depth_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                           Param_Name : Enums.Textures.Parameter;
                                           Values     : out Objects.Textures.Depth_Mode);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Depth_Mode,
                  External_Name => "glGetTexParameteriv");

   procedure Get_Tex_Parameter_Bool (Target     : Low_Level.Enums.Texture_Kind;
                                     Param_Name : Enums.Textures.Parameter;
                                     Values     : out Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Bool,
                  External_Name => "glGetTexParameteriv");

   procedure Gen_Textures (N : Size; Textures : access UInt);
   pragma Import (Convention => StdCall, Entity => Gen_Textures,
                  External_Name => "glGenTextures");

   procedure Bind_Texture (Target  : Low_Level.Enums.Texture_Kind;
                           Texture : UInt);
   pragma Import (Convention => StdCall, Entity => Bind_Texture,
                  External_Name => "glBindTexture");

   procedure Delete_Textures (N : Size; Textures : Low_Level.UInt_Array);
   pragma Import (Convention => StdCall, Entity => Delete_Textures,
                  External_Name => "glDeleteTextures");

   procedure Tex_Image_2D (Target : Objects.Textures.Loader_2D.Target_Kind;
                           Level  : Objects.Textures.Mipmap_Level;
                           Internal_Format : Pixel_Data.Internal_Format;
                           Width, Height : Size;
                           Border : Low_Level.Bool;
                           Format : Pixel_Data.Format;
                           Data_Type : Pixel_Data.Data_Type;
                           Data : System.Address);
   pragma Import (Convention => StdCall, Entity => Tex_Image_2D,
                  External_Name => "glTexImage2D");

    procedure Tex_Env_Float (Target     : Enums.Textures.Env_Target;
                            Param_Name : Enums.Textures.Env_Parameter;
                            Value      : Single);
   pragma Import (Convention => StdCall, Entity => Tex_Env_Float,
                  External_Name => "glTexEnvf");

   procedure Tex_Env_Int (Target     : Enums.Textures.Env_Target;
                          Param_Name : Enums.Textures.Env_Parameter;
                          Value      : Int);
   pragma Import (Convention => StdCall, Entity => Tex_Env_Int,
                  External_Name => "glTexEnvi");

   procedure Tex_Env_Tex_Func (Target     : Enums.Textures.Env_Target;
                               Param_Name : Enums.Textures.Env_Parameter;
                               Value      : Fixed.Textures.Texture_Function);
   pragma Import (Convention => StdCall, Entity => Tex_Env_Tex_Func,
                  External_Name => "glTexEnvi");

   procedure Tex_Env_Combine_Func (Target     : Enums.Textures.Env_Target;
                                   Param_Name : Enums.Textures.Env_Parameter;
                                   Value      : Fixed.Textures.Combine_Function);
   pragma Import (Convention => StdCall, Entity => Tex_Env_Combine_Func,
                  External_Name => "glTexEnvi");

   procedure Tex_Env_Source (Target     : Enums.Textures.Env_Target;
                             Param_Name : Enums.Textures.Env_Parameter;
                             Value      : Fixed.Textures.Source_Kind);
   pragma Import (Convention => StdCall, Entity => Tex_Env_Source,
                  External_Name => "glTexEnvi");

   procedure Tex_Env_Arr (Target     : Enums.Textures.Env_Target;
                          Param_Name : Enums.Textures.Env_Parameter;
                          Value      : Low_Level.Single_Array);
   pragma Import (Convention => StdCall, Entity => Tex_Env_Arr,
                  External_Name => "glTexEnvfv");

   procedure Tex_Env_Bool (Target     : Enums.Textures.Env_Target;
                           Param_Name : Enums.Textures.Env_Parameter;
                           Value      : Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Tex_Env_Bool,
                  External_Name => "glTexEnvi");

   procedure Get_Tex_Env_Float (Target     : Enums.Textures.Env_Target;
                                Param_Name : Enums.Textures.Env_Parameter;
                                Value      : out Single);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Env_Float,
                  External_Name => "glGetTexEnvfv");

   procedure Get_Tex_Env_Tex_Func (Target     : Enums.Textures.Env_Target;
                                   Param_Name : Enums.Textures.Env_Parameter;
                                   Value      : out Fixed.Textures.Texture_Function);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Env_Tex_Func,
                  External_Name => "glGetTexEnviv");

   procedure Get_Tex_Env_Combine_Func (Target     : Enums.Textures.Env_Target;
                                       Param_Name : Enums.Textures.Env_Parameter;
                                       Value      : out Fixed.Textures.Combine_Function);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Env_Combine_Func,
                  External_Name => "glGetTexEnviv");

   procedure Get_Tex_Env_Source (Target     : Enums.Textures.Env_Target;
                                 Param_Name : Enums.Textures.Env_Parameter;
                                 Value      : out Fixed.Textures.Source_Kind);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Env_Source,
                  External_Name => "glGetTexEnviv");

   procedure Get_Tex_Env_Arr (Target     : Enums.Textures.Env_Target;
                              Param_Name : Enums.Textures.Env_Parameter;
                              Value      : in out Low_Level.Single_Array);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Env_Arr,
                    External_Name => "glGetTexEnvfv");

   procedure Get_Tex_Env_Bool (Target     : Enums.Textures.Env_Target;
                               Param_Name : Enums.Textures.Env_Parameter;
                               Value      : out Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Env_Bool,
                  External_Name => "glGetTexEnviv");

   procedure Active_Texture is new Loader.Procedure_With_1_Param
      ("glActiveTexture", Int);

   -----------------------------------------------------------------------------
   --                             Buffer Objects                              --
   -----------------------------------------------------------------------------

   procedure Gen_Buffers is new Loader.Getter_With_2_Params
      ("glGenBuffers", Size, UInt);

   procedure Delete_Buffers is new Loader.Array_Proc_With_2_Params
      ("glDeleteBuffers", Size, UInt, Low_Level.UInt_Array);

   procedure Bind_Buffer is new Loader.Procedure_With_2_Params
      ("glBindBuffer", Low_Level.Enums.Buffer_Kind, UInt);

   procedure Buffer_Data is new Loader.Procedure_With_4_Params
      ("glBufferData", Low_Level.Enums.Buffer_Kind, Low_Level.SizeIPtr,
       System.Address, Objects.Buffer.Buffer_Usage);

   -----------------------------------------------------------------------------
   --                           Vertex Array Objects                          --
   -----------------------------------------------------------------------------

   procedure Gen_Vertex_Arrays is new Loader.Getter_With_2_Params
     ("glGenVertexArrays", Size, UInt);

   procedure Delete_Vertex_Arrays is new Loader.Array_Proc_With_2_Params
     ("glDeleteVertexArrays", Size, UInt, Low_Level.UInt_Array);

   procedure Bind_Vertex_Array is new Loader.Procedure_With_1_Param
     ("glBindVertexArray", UInt);
   
   -----------------------------------------------------------------------------
   --                        Renderbuffer objects                             --
   -----------------------------------------------------------------------------
   
   procedure Gen_Renderbuffers is new Loader.Getter_With_2_Params
     ("glGenRenderbuffers", Size, UInt);
   
   procedure Delete_Renderbuffers is new Loader.Array_Proc_With_2_Params
     ("glDeleteBuffers", Size, UInt, Low_Level.UInt_Array);
   
   procedure Renderbuffer_Storage is new Loader.Procedure_With_4_Params
     ("glRenderbufferStorage", Low_Level.Enums.Renderbuffer_Kind,
      Pixel_Data.Internal_Format, Size, Size);
   
   procedure Renderbuffer_Storage_Multisample is new
     Loader.Procedure_With_5_Params ("glRenderbufferStorageMultisample",
                                     Low_Level.Enums.Renderbuffer_Kind, Size,
                                     Pixel_Data.Internal_Format, Size, Size);
   
   procedure Bind_Renderbuffer is new Loader.Procedure_With_2_Params
     ("glBindRenderbuffer", Low_Level.Enums.Renderbuffer_Kind, UInt);
   
   procedure Get_Renderbuffer_Parameter_Int is new Loader.Getter_With_3_Params
     ("glGetRenderbufferParameteriv", Low_Level.Enums.Renderbuffer_Kind,
      Enums.Getter.Renderbuffer_Parameter, Int);
   
   procedure Get_Renderbuffer_Parameter_Internal_Format is new
     Loader.Getter_With_3_Params ("glGetRenderbufferParameteriv",
                                  Low_Level.Enums.Renderbuffer_Kind,
                                  Enums.Getter.Renderbuffer_Parameter,
                                  Pixel_Data.Internal_Format);
   
   -----------------------------------------------------------------------------
   --                         Framebuffer objects                             --
   -----------------------------------------------------------------------------

   procedure Gen_Framebuffers is new Loader.Getter_With_2_Params
     ("glGenFramebuffers", Size, UInt);
   
   procedure Delete_Framebuffers is new Loader.Array_Proc_With_2_Params
     ("glDeleteFramebuffers", Size, UInt, Low_Level.UInt_Array);
   
   procedure Bind_Framebuffer is new Loader.Procedure_With_2_Params
     ("glBindFramebuffer", Low_Level.Enums.Framebuffer_Kind, UInt);
   
   function Check_Framebuffer_Status is new Loader.Function_With_1_Param
     ("glCheckFramebufferStatus", Low_Level.Enums.Framebuffer_Kind,
      Objects.Framebuffers.Framebuffer_Status);
   
   procedure Framebuffer_Renderbuffer is new Loader.Procedure_With_4_Params
     ("glFramebufferRenderbuffer", Low_Level.Enums.Framebuffer_Kind,
      Objects.Framebuffers.Attachment_Point,
      Low_Level.Enums.Renderbuffer_Kind, UInt);
   
   procedure Framebuffer_Texture is new Loader.Procedure_With_4_Params
     ("glFramebufferTexture", Low_Level.Enums.Framebuffer_Kind,
      Objects.Framebuffers.Attachment_Point, UInt,
      Objects.Textures.Mipmap_Level);
   
   procedure Framebuffer_Texture_Layer is new Loader.Procedure_With_5_Params
     ("glFramebufferTextureLayer", Low_Level.Enums.Framebuffer_Kind,
      Objects.Framebuffers.Attachment_Point, UInt,
      Objects.Textures.Mipmap_Level, Int);
   
   procedure Blit_Framebuffer is new Loader.Procedure_With_10_Params
     ("glBlitFramebuffer", Int, Int, Int, Int, Int, Int, Int, Int,
      Low_Level.Bitfield, Objects.Textures.Magnifying_Function);
   
   procedure Invalidate_Framebuffer is new Loader.Array_Proc_With_3_Params
     ("glInvalidateFramebuffer", Low_Level.Enums.Framebuffer_Kind, Size,
      Objects.Framebuffers.Attachment_Point,
      Objects.Framebuffers.Attachment_List);
   
   -----------------------------------------------------------------------------
   --                                 Shaders                                 --
   -----------------------------------------------------------------------------

   procedure Get_Shader_Param is new Loader.Getter_With_3_Params
     ("glGetShaderiv", UInt, Enums.Shader_Param, Int);

   function Create_Shader is new Loader.Function_With_1_Param
     ("glCreateShader", Objects.Shaders.Shader_Type, UInt);

   procedure Delete_Shader is new Loader.Procedure_With_1_Param
     ("glDeleteShader", UInt);

   procedure Shader_Source is new Loader.Procedure_With_4_Params
     ("glShaderSource", UInt, Size, Low_Level.CharPtr_Array,
      Low_Level.Int_Array);

   procedure Get_Shader_Source is
     new Loader.String_Getter_With_4_Params
     ("glGetShaderSource", Size, UInt);

   procedure Compile_Shader is new Loader.Procedure_With_1_Param
     ("glCompileShader", UInt);

   procedure Get_Shader_Info_Log is
     new Loader.String_Getter_With_4_Params
     ("glGetShaderInfoLog", Size, UInt);

   function Create_Program is new Loader.Function_Without_Params
     ("glCreateProgram", UInt);

   procedure Delete_Program is new Loader.Procedure_With_1_Param
     ("glDeleteProgram", UInt);

   procedure Get_Program_Param is new Loader.Getter_With_3_Params
     ("glGetProgramiv", UInt, Enums.Program_Param, Int);

   procedure Attach_Shader is new Loader.Procedure_With_2_Params
     ("glAttachShader", UInt, UInt);

   procedure Link_Program is new Loader.Procedure_With_1_Param
     ("glLinkProgram", UInt);

   procedure Get_Program_Info_Log is
     new Loader.String_Getter_With_4_Params
     ("glGetProgramInfoLog", Size, UInt);

   procedure Use_Program is new Loader.Procedure_With_1_Param
     ("glUseProgram", UInt);

   function Get_Uniform_Location is new Loader.Function_With_2_Params
     ("glGetUniformLocation", UInt, C.char_array, Uniforms.Uniform);

   procedure Bind_Attrib_Location is new Loader.Procedure_With_3_Params
     ("glBindAttribLocation", UInt, Attributes.Attribute, C.char_array);

   function Get_Attrib_Location is new Loader.Function_With_2_Params
     ("glGetAttribLocation", UInt, C.char_array, Attributes.Attribute);

   procedure Vertex_Attrib_Pointer is new Loader.Procedure_With_6_Params
     ("glVertexAttribPointer", Attributes.Attribute, Component_Count, Numeric_Type,
      Low_Level.Bool, Size, Int);

   procedure Vertex_AttribI_Pointer is new Loader.Procedure_With_5_Params
     ("glVertexAttribIPointer", Attributes.Attribute, Component_Count, Numeric_Type,
      Size, Int);

   procedure Vertex_AttribL_Pointer is new Loader.Procedure_With_5_Params
     ("glVertexAttribLPointer", Attributes.Attribute, Component_Count, Numeric_Type,
      Size, Int);

   procedure Enable_Vertex_Attrib_Array is new Loader.Procedure_With_1_Param
     ("glEnableVertexAttribArray", Attributes.Attribute);

   procedure Disable_Vertex_Attrib_Array is new Loader.Procedure_With_1_Param
     ("glDisableVertexAttribArray", Attributes.Attribute);
   
   -----------------------------------------------------------------------------
   --                  Transformation to window coordinates                   --
   -----------------------------------------------------------------------------
   
   procedure Depth_Range (Near, Far : Double);
   pragma Import (Convention => StdCall, Entity => Depth_Range,
                  External_Name => "glDepthRange");
   
   procedure Viewport (X, Y : Int; Width, Height : Size);
   pragma Import (Convention => StdCall, Entity => Viewport,
                  External_Name => "glViewport");
   
end GL.API;
