--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

with GL.Enums;
with GL.Enums.Getter;
with GL.Enums.Textures;
with GL.Low_Level.Loader;
with GL.Low_Level.Enums;
with GL.Matrices;
with GL.Immediate;
with GL.Vectors;
with GL.Colors;
with GL.Normals;
with GL.Buffers;
with GL.Objects.Textures;
with GL.Objects.Textures.Loader_2D;
with GL.Pixel_Data;
with GL.Environment.Textures;
with GL.Common;
with GL.Toggles;

with System;

private package GL.API is
   -- Everything newer than OpenGL 1.1 will not be statically bound,
   -- but loaded with GL.Low_Level.Loader at runtime.
   --
   -- Also, all functions that have been deprecated with OpenGL 3.0
   -- will not be statically bound, as they may be omitted by implementors
   -- when they choose to only implement the OpenGL Core Profile.
   
   subtype Zero is Low_Level.Int range 0 .. 0;

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
                         Target : access Low_Level.Double);
   pragma Import (Convention => StdCall, Entity => Get_Double,
                  External_Name => "glGetDoublev");

   procedure Get_Color (Name : Enums.Getter.Parameter;
                        Target : in out Colors.Color);
   pragma Import (Convention => StdCall, Entity => Get_Color,
                  External_Name => "glGetFloatv");

   procedure Get_Integer (Name   : Enums.Getter.Parameter;
                          Target : access C.int);
   pragma Import (Convention => StdCall, Entity => Get_Integer,
                  External_Name => "glGetIntegerv");

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

   procedure Matrix_Mode is new Low_Level.Loader.Procedure_With_1_Param
      ("glMatrixMode", Enums.Matrix_Mode);
   
   procedure Frustum is new Low_Level.Loader.Procedure_With_6_Params
      ("glFrustum", Low_Level.Double, Low_Level.Double, Low_Level.Double,
       Low_Level.Double, Low_Level.Double, Low_Level.Double);

   procedure Ortho is new Low_Level.Loader.Procedure_With_6_Params
      ("glOrtho", Low_Level.Double, Low_Level.Double, Low_Level.Double,
       Low_Level.Double, Low_Level.Double, Low_Level.Double);

   procedure Load_Identity is new Low_Level.Loader.Procedure_Without_Params
      ("glLoadIdentity");

   procedure Load_Matrix is new Low_Level.Loader.Procedure_With_1_Param
      ("glLoadMatrixd", Matrices.Matrix);
   
   procedure Mult_Matrix is new Low_Level.Loader.Procedure_With_1_Param
      ("glMultMatrixd", Matrices.Matrix);

   procedure Push_Matrix is new Low_Level.Loader.Procedure_Without_Params
      ("glPushMatrix");

   procedure Pop_Matrix is new Low_Level.Loader.Procedure_Without_Params
      ("glPopMatrix");

   procedure Rotate is new Low_Level.Loader.Procedure_With_4_Params
      ("glRotated", Low_Level.Double, Low_Level.Double, Low_Level.Double,
       Low_Level.Double);
   
   procedure Scale is new Low_Level.Loader.Procedure_With_3_Params
      ("glScaled", Low_Level.Double, Low_Level.Double, Low_Level.Double);
   
   procedure Translate is new Low_Level.Loader.Procedure_With_3_Params
      ("glTranslated", Low_Level.Double, Low_Level.Double, Low_Level.Double);

   -----------------------------------------------------------------------------
   --              Immediate API (deprecated as of OpenGL 3.0)                --
   -----------------------------------------------------------------------------

   procedure GL_Begin is new Low_Level.Loader.Procedure_With_1_Param
      ("glBegin", Immediate.Connection_Mode);

   procedure GL_End is new Low_Level.Loader.Procedure_Without_Params
      ("glEnd");
   
   procedure Vertex is new Low_Level.Loader.Procedure_With_1_Param
      ("glVertex4dv", Vectors.Vector);

   procedure Color is new Low_Level.Loader.Procedure_With_1_Param
      ("glColor4dv", Colors.Color);
   
   procedure Secondary_Color is new Low_Level.Loader.Procedure_With_1_Param
      ("glSecondaryColor3dv", Colors.Basic_Color);

   procedure Fog_Coord is new Low_Level.Loader.Procedure_With_1_Param
      ("glFogCoordd", Low_Level.Double);
   
   procedure Normal is new Low_Level.Loader.Procedure_With_1_Param
      ("glNormal3dv", Normals.Normal);
   
   procedure Tex_Coord is new Low_Level.Loader.Procedure_With_1_Param
      ("glTexCoord4dv", Vectors.Vector);
   
   -----------------------------------------------------------------------------
   --                                Buffers                                  --
   -----------------------------------------------------------------------------

   procedure Clear (Bits : Low_Level.Bitfield);
   pragma Import (Convention => StdCall, Entity => Clear,
                  External_Name => "glClear");

   procedure Draw_Buffer (Mode : Buffers.Color_Buffer_Selector);
   pragma Import (Convention => StdCall, Entity => Draw_Buffer,
                  External_Name => "glDrawBuffer");
   
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
   procedure Clear_Accum is new Low_Level.Loader.Procedure_With_4_Params
      ("glClearAccum", Colors.Component, Colors.Component, Colors.Component,
       Colors.Component);
   
   procedure Clear_Buffer (Buffer      : Buffers.Color_Buffer_Selector;
                           Draw_Buffer : Zero;
                           Value       : Colors.Color);
   pragma Import (Convention => StdCall, Entity => Clear_Buffer,
                  External_Name => "glClearBufferfv");
   
   procedure Clear_Buffer_Depth (Buffer      : Low_Level.Enums.Only_Depth_Buffer;
                                 Draw_Buffer : Zero;
                                 Value       : access constant Buffers.Depth);
   pragma Import (Convention => StdCall, Entity => Clear_Buffer_Depth,
                  External_Name => "glClearBufferfv");
   
   procedure Clear_Buffer_Stencil (Buffer      : Low_Level.Enums.Only_Stencil_Buffer;
                                   Draw_Buffer : Zero;
                                   Value       : access constant Buffers.Stencil_Index);
   pragma Import (Convention => StdCall, Entity => Clear_Buffer_Stencil,
                  External_Name => "glClearBufferiv");
   
   procedure Clear_Buffer_Depth_Stencil (
      Buffer      : Low_Level.Enums.Only_Depth_Stencil_Buffer;
      Draw_Buffer : Zero;
      Depth       : Buffers.Depth;
      Stencil     : Buffers.Stencil_Index);
   pragma Import (Convention => StdCall, Entity => Clear_Buffer_Depth_Stencil,
                  External_Name => "glClearBufferfi");

   -----------------------------------------------------------------------------
   --                                Textures                                 --
   -----------------------------------------------------------------------------

   procedure Tex_Parameter_Float (Target     : Low_Level.Enums.Texture_Kind;
                                  Param_Name : Enums.Textures.Parameter;
                                  Value      : Low_Level.Single);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Float,
                  External_Name => "glTexParameterf");

   procedure Tex_Parameter_Int (Target     : Low_Level.Enums.Texture_Kind;
                                Param_Name : Enums.Textures.Parameter;
                                Value      : Low_Level.Int);
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
                                      Value      : Common.Compare_Function);
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
                                          Values     : out Common.Compare_Function);
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

   procedure Gen_Textures (N : Low_Level.SizeI; Textures : access Low_Level.UInt);
   pragma Import (Convention => StdCall, Entity => Gen_Textures,
                  External_Name => "glGenTextures");

   procedure Bind_Texture (Target  : Low_Level.Enums.Texture_Kind;
                           Texture : Low_Level.UInt);
   pragma Import (Convention => StdCall, Entity => Bind_Texture,
                  External_Name => "glBindTexture");

   procedure Delete_Textures (N : Low_Level.SizeI; Textures : Low_Level.UInt_Array);
   pragma Import (Convention => StdCall, Entity => Delete_Textures,
                  External_Name => "glDeleteTextures");

   procedure Tex_Image_2D (Target : Objects.Textures.Loader_2D.Target_Kind;
                           Level  : Low_Level.Int;
                           Internal_Format : Pixel_Data.Internal_Format;
                           Width, Height : Low_Level.SizeI;
                           Border : Low_Level.Bool;
                           Format : Pixel_Data.Format;
                           Data_Type : Pixel_Data.Data_Type;
                           Data : System.Address);
   pragma Import (Convention => StdCall, Entity => Tex_Image_2D,
                  External_Name => "glTexImage2D");

   
   procedure Tex_Env_Float is new Low_Level.Loader.Procedure_With_3_Params
      ("glTexEnvf", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Low_Level.Single);
   
   procedure Tex_Env_Int is new Low_Level.Loader.Procedure_With_3_Params
      ("glTexEnvi", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Low_Level.Int);

   procedure Tex_Env_Tex_Func is new Low_Level.Loader.Procedure_With_3_Params
      ("glTexEnvi", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Environment.Textures.Texture_Function);

   procedure Tex_Env_Combine_Func is new Low_Level.Loader.Procedure_With_3_Params
      ("glTexEnvi", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Environment.Textures.Combine_Function);

   procedure Tex_Env_Source is new Low_Level.Loader.Procedure_With_3_Params
      ("glTexEnvi", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Environment.Textures.Source_Kind);
   
   -- this is not a getter, but we need to use it here as we have an
   -- indefinite type as parameter
   procedure Tex_Env_Arr is new Low_Level.Loader.Getter_with_3_Params
      ("glTexEnvfv", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Low_Level.Single_Array);

   procedure Tex_Env_Bool is new Low_Level.Loader.Procedure_With_3_Params
      ("glTexEnvi", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Low_Level.Bool);

   procedure Get_Tex_Env_Float is new Low_Level.Loader.Getter_With_3_Params
      ("glGetTexEnvfv", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Low_Level.Single);
   
   procedure Get_Tex_Env_Tex_Func is new Low_Level.Loader.Getter_With_3_Params
      ("glGetTexEnviv", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Environment.Textures.Texture_Function);
   
   procedure Get_Tex_Env_Combine_Func is new Low_Level.Loader.Getter_With_3_Params
      ("glGetTexEnviv", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Environment.Textures.Combine_Function);
   
   procedure Get_Tex_Env_Source is new Low_Level.Loader.Getter_With_3_Params
      ("glGetTexEnviv", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Environment.Textures.Source_Kind);

   procedure Get_Tex_Env_Arr is new Low_Level.Loader.Getter_With_3_Params
      ("glGetTexEnvfv", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Low_Level.Single_Array);
   
   procedure Get_Tex_Env_Bool is new Low_Level.Loader.Getter_With_3_Params
      ("glGetTexEnviv", Enums.Textures.Env_Target, Enums.Textures.Env_Parameter,
       Low_Level.Bool);
end GL.API;
