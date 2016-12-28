with GL.Runtime_Loading;

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
   pragma Preelaborate;
   use GL.Types;

   subtype Zero is Int range 0 .. 0;

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
end GL.API;