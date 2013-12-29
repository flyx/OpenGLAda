--
--  Copyright  (c) 2002-2003, David Holm
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are
--  met:
--
--   * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright
--     notice,
--     this list of conditions and the following disclaimer in the
--     documentation
--     and/or other materials provided with the distribution.
--   * The names of its contributors may not be used to endorse or promote
--     products derived from this software without specific prior written
--     permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES;
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--

with Interfaces.C.Strings;
with System;

package GL.WGL is
   pragma Preelaborate;

   WGL_FONT_LINES                 : constant := 8#0000#;
   WGL_FONT_POLYGONS              : constant := 1;
   WGL_SWAP_MAIN_PLANE            : constant := 1;
   WGL_SWAP_OVERLAY1              : constant := 2;
   WGL_SWAP_OVERLAY2              : constant := 4;
   WGL_SWAP_OVERLAY3              : constant := 8;
   WGL_SWAP_OVERLAY4              : constant := 16#0010#;
   WGL_SWAP_OVERLAY5              : constant := 16#0020#;
   WGL_SWAP_OVERLAY6              : constant := 16#0040#;
   WGL_SWAP_OVERLAY7              : constant := 16#0080#;
   WGL_SWAP_OVERLAY8              : constant := 16#0100#;
   WGL_SWAP_OVERLAY9              : constant := 16#0200#;
   WGL_SWAP_OVERLAY10             : constant := 16#0400#;
   WGL_SWAP_OVERLAY11             : constant := 16#0800#;
   WGL_SWAP_OVERLAY12             : constant := 16#1000#;
   WGL_SWAP_OVERLAY13             : constant := 16#2000#;
   WGL_SWAP_OVERLAY14             : constant := 16#4000#;
   WGL_SWAP_OVERLAY15             : constant := 16#8000#;
   WGL_SWAP_UNDERLAY1             : constant := 16#0001_0000#;
   WGL_SWAP_UNDERLAY2             : constant := 16#0002_0000#;
   WGL_SWAP_UNDERLAY3             : constant := 16#0004_0000#;
   WGL_SWAP_UNDERLAY4             : constant := 16#0008_0000#;
   WGL_SWAP_UNDERLAY5             : constant := 16#0010_0000#;
   WGL_SWAP_UNDERLAY6             : constant := 16#0020_0000#;
   WGL_SWAP_UNDERLAY7             : constant := 16#0040_0000#;
   WGL_SWAP_UNDERLAY8             : constant := 16#0080_0000#;
   WGL_SWAP_UNDERLAY9             : constant := 16#0100_0000#;
   WGL_SWAP_UNDERLAY10            : constant := 16#0200_0000#;
   WGL_SWAP_UNDERLAY11            : constant := 16#0400_0000#;
   WGL_SWAP_UNDERLAY12            : constant := 16#0800_0000#;
   WGL_SWAP_UNDERLAY13            : constant := 16#1000_0000#;
   WGL_SWAP_UNDERLAY14            : constant := 16#2000_0000#;
   WGL_SWAP_UNDERLAY15            : constant := 16#4000_0000#;

   type Pixel_Format_Descriptor is
       record
           nSize           : Interfaces.C.short;
           nVersion        : Interfaces.C.short;
           dwFlags         : Interfaces.C.long;
           iPixelType      : Interfaces.C.char;
           cColorBits      : Interfaces.C.char;
           cRedBits        : Interfaces.C.char;
           cRedShift       : Interfaces.C.char;
           cGreenBits      : Interfaces.C.char;
           cGreenShift     : Interfaces.C.char;
           cBlueBits       : Interfaces.C.char;
           cBlueShift      : Interfaces.C.char;
           cAlphaBits      : Interfaces.C.char;
           cAlphaShift     : Interfaces.C.char;
           cAccumBits      : Interfaces.C.char;
           cAccumRedBits   : Interfaces.C.char;
           cAccumGreenBits : Interfaces.C.char;
           cAccumBlueBits  : Interfaces.C.char;
           cAccumAlphaBits : Interfaces.C.char;
           cDepthBits      : Interfaces.C.char;
           cStencilBits    : Interfaces.C.char;
           cAuxBuffers     : Interfaces.C.char;
           iLayerType      : Interfaces.C.char;
           bReserved       : Interfaces.C.char;
           dwLayerMask     : Interfaces.C.long;
           dwVisibleMask   : Interfaces.C.long;
           dwDamageMask    : Interfaces.C.long;
       end record;
   pragma Convention (C_Pass_By_Copy, Pixel_Format_Descriptor);

   type Point_Float is
      record
          x : Interfaces.C.C_float;
          y : Interfaces.C.C_float;
      end record;
   pragma Convention (C_Pass_By_Copy, Point_Float);

   type Glyph_Metrics_Float is
      record
          gmfBlackBoxX     : Interfaces.C.C_float;
          gmfBlackBoxY     : Interfaces.C.C_float;
          gmfptGlyphOrigin : Point_Float;
          gmfCellIncX      : Interfaces.C.C_float;
          gmfCellIncY      : Interfaces.C.C_float;
      end record;
   pragma Convention (C_Pass_By_Copy, Glyph_Metrics_Float);

   type COLORREF is new Interfaces.C.long;
   type COLORREF_Type is access all COLORREF;

   type Layer_Plane_Descriptor is
      record
          nSize           : Interfaces.C.short;
          nVersion        : Interfaces.C.short;
          dwFlags         : Interfaces.C.long;
          iPixelType      : Interfaces.C.char;
          cColorBits      : Interfaces.C.char;
          cRedBits        : Interfaces.C.char;
          cRedShift       : Interfaces.C.char;
          cGreenBits      : Interfaces.C.char;
          cGreenShift     : Interfaces.C.char;
          cBlueBits       : Interfaces.C.char;
          cBlueShift      : Interfaces.C.char;
          cAlphaBits      : Interfaces.C.char;
          cAlphaShift     : Interfaces.C.char;
          cAccumBits      : Interfaces.C.char;
          cAccumRedBits   : Interfaces.C.char;
          cAccumGreenBits : Interfaces.C.char;
          cAccumBlueBits  : Interfaces.C.char;
          cAccumAlphaBits : Interfaces.C.char;
          cDepthBits      : Interfaces.C.char;
          cStencilBits    : Interfaces.C.char;
          cAuxBuffers     : Interfaces.C.char;
          iLayerPlane     : Interfaces.C.char;
          bReserved       : Interfaces.C.char;
          crTransparent   : COLORREF;
      end record;
   pragma Convention (C_Pass_By_Copy, Layer_Plane_Descriptor);

   type Layer_Plane_Descriptor_Type is access all Layer_Plane_Descriptor;
   type Glyph_Metrics_Float_Type is access all Glyph_Metrics_Float;
   type Pixel_Format_Descriptor_Type is access all Pixel_Format_Descriptor;

   type HANDLE is new System.Address;
   subtype HDC is HANDLE;
   subtype HGLRC is HANDLE;

   function wglDeleteContext (Rendering_Context : HGLRC)
      return Interfaces.C.int;

   function wglMakeCurrent (Device_Context    : HDC;
                            Rendering_Context : HGLRC)
                               return Interfaces.C.int;

   function wglSetPixelFormat (Device_Context    : HDC;
                               Pixel_Format      : Interfaces.C.int;
                               Pixel_Format_Desc :
                                  access Pixel_Format_Descriptor_Type)
                                     return Interfaces.C.int;

   function wglSwapBuffers (Device_Context : HDC)
      return Interfaces.C.int;

   function wglGetCurrentDC return HANDLE;

   function wglCreateContext (Device_Context : HDC)
      return HANDLE;

   function wglCreateLayerContext (Device_Context : HDC;
                                   Layer_Plane : Interfaces.C.int)
                                      return HANDLE;

   function wglGetCurrentContext return HANDLE;

   function wglGetProcAddress (Proc_Desc : Interfaces.C.Strings.chars_ptr)
                               return System.Address;

   function wglChoosePixelFormat (Device_Context    : HDC;
                                  Pixel_Format_Desc :
                                     access Pixel_Format_Descriptor_Type)
                                        return Interfaces.C.int;


   function wglCopyContext (Rendering_Context_Source : HGLRC;
                            Rendering_Context_Dest   : HGLRC;
                            Mask                     :
                               Interfaces.C.unsigned)
                                  return Interfaces.C.int;

   function wglDescribeLayerPlane (Device_Context : HDC;
                                   Pixel_Format   : Interfaces.C.int;
                                   Layer_Plane    : Interfaces.C.int;
                                   Bytes          : Interfaces.C.unsigned;
                                   Plane_Desc     :
                                     Layer_Plane_Descriptor_Type)
                                        return Interfaces.C.int;

   function wglDescribePixelFormat (Device_Context    : HDC;
                                    Layer_Plane       : Interfaces.C.int;
                                    Bytes             :
                                       Interfaces.C.unsigned;
                                    Pixel_Format_Desc :
                                       Pixel_Format_Descriptor_Type)
                                          return Interfaces.C.int;

   function wglGetLayerPaletteEntries (Device_Context : HDC;
                                       Layer_Plane    : Interfaces.C.int;
                                       Start          : Interfaces.C.int;
                                       Entries        : Interfaces.C.int;
                                       Color_Ref      :
                                          access Interfaces.C.long)
                                             return Interfaces.C.int;

   function wglGetPixelFormat (Device_Context : HDC)
      return Interfaces.C.int;

   function wglRealizeLayerPalette (Device_Context : HDC;
                                    Layer_Plane    : Interfaces.C.int;
                                    Realize        : Boolean)
                                       return Interfaces.C.int;

   function wglSetLayerPaletteEntries (Device_Context  : HDC;
                                       Layer_Plane     : Interfaces.C.int;
                                       Start           : Interfaces.C.int;
                                       Entries         : Interfaces.C.int;
                                       Color_Reference : COLORREF_Type)
                                          return Interfaces.C.int;

   function wglShareLists (Existing_Rendering_Context : HGLRC;
                           New_Rendering_Context      : HGLRC)
                              return Interfaces.C.int;

   function wglSwapLayerBuffers (Device_Context : HDC;
                                 Planes         : Interfaces.C.unsigned)
                                    return Interfaces.C.int;

   function wglUseFontBitmapsA (Device_Context : HDC;
                                First          : Interfaces.C.unsigned;
                                Count          : Interfaces.C.unsigned;
                                List_Base      : Interfaces.C.unsigned)
                                   return Interfaces.C.int;

   function wglUseFontBitmapsW (Device_Context : HDC;
                                First          : Interfaces.C.unsigned;
                                Count          : Interfaces.C.unsigned;
                                List_Base      : Interfaces.C.unsigned)
                                   return Interfaces.C.int;

   function wglUseFontOutlinesA (Device_Context    : HDC;
                                 First             : Interfaces.C.unsigned;
                                 Count             : Interfaces.C.unsigned;
                                 List_Base         : Interfaces.C.unsigned;
                                 Deviation         : Interfaces.C.C_float;
                                 Extrusion         : Interfaces.C.C_float;
                                 Format            : Interfaces.C.int;
                                 Glyph_Data_Buffer : Glyph_Metrics_Float_Type)
                                    return Interfaces.C.int;

   function wglUseFontOutlinesW (Device_Context    : HDC;
                                 First             : Interfaces.C.unsigned;
                                 Count             : Interfaces.C.unsigned;
                                 List_Base         : Interfaces.C.unsigned;
                                 Deviation         : Interfaces.C.C_float;
                                 Extrusion         : Interfaces.C.C_float;
                                 Format            : Interfaces.C.int;
                                 Glyph_Data_Buffer : Glyph_Metrics_Float_Type)
                                    return Interfaces.C.int;

   function SwapBuffers (Device_Context : HDC) return
      Interfaces.C.int;

   function ChoosePixelFormat (Device_Context    : HDC;
                               Pixel_Format_Desc :
                                 access Pixel_Format_Descriptor_Type)
                                    return Interfaces.C.int;

   function DescribePixelFormat (Device_Context    : HDC;
                                 Pixel_Format      : Interfaces.C.int;
                                 Bytes             : Interfaces.C.unsigned;
                                 Pixel_Format_Desc :
                                    Pixel_Format_Descriptor_Type)
                                       return Interfaces.C.int;

   function GetPixelFormat (Device_Context : HDC)
      return Interfaces.C.int;

   function SetPixelFormat (Device_Context    : HDC;
                            Pixel_Format      : Interfaces.C.int;
                            Pixel_Format_Desc :
                              access Pixel_Format_Descriptor_Type)
                                 return Interfaces.C.int;

private

   pragma Import (StdCall, wglDeleteContext, "wglDeleteContext");

   pragma Import (StdCall, wglMakeCurrent, "wglMakeCurrent");

   pragma Import (StdCall, wglSetPixelFormat, "wglSetPixelFormat");

   pragma Import (StdCall, wglSwapBuffers, "wglSwapBuffers");

   pragma Import (StdCall, wglGetCurrentDC, "wglGetCurrentDC");

   pragma Import (StdCall, wglCreateContext, "wglCreateContext");

   pragma Import (StdCall, wglCreateLayerContext, "wglCreateLayerContext");

   pragma Import (StdCall, wglGetCurrentContext, "wglGetCurrentContext");

   pragma Import (StdCall, wglGetProcAddress, "wglGetProcAddress");

   pragma Import (StdCall, wglChoosePixelFormat, "wglChoosePixelFormat");

   pragma Import (StdCall, wglCopyContext, "wglCopyContext");

   pragma Import (StdCall, wglDescribeLayerPlane, "wglDescribeLayerPlane");

   pragma Import (StdCall, wglDescribePixelFormat, "wglDescribePixelFormat");

   pragma Import (StdCall, wglGetLayerPaletteEntries, "wglGetLayerPaletteEntries");

   pragma Import (StdCall, wglGetPixelFormat, "wglGetPixelFormat");

   pragma Import (StdCall, wglRealizeLayerPalette, "wglRealizeLayerPalette");

   pragma Import (StdCall, wglSetLayerPaletteEntries, "wglSetLayerPaletteEntries");

   pragma Import (StdCall, wglShareLists, "wglShareLists");

   pragma Import (StdCall, wglSwapLayerBuffers, "wglSwapLayerBuffers");

   pragma Import (StdCall, wglUseFontBitmapsA, "wglUseFontBitmapsA");

   pragma Import (StdCall, wglUseFontBitmapsW, "wglUseFontBitmapsW");

   pragma Import (StdCall, wglUseFontOutlinesA, "wglUseFontOutlinesA");

   pragma Import (StdCall, wglUseFontOutlinesW, "wglUseFontOutlinesW");

   pragma Import (StdCall, SwapBuffers, "SwapBuffers");

   pragma Import (StdCall, ChoosePixelFormat, "ChoosePixelFormat");

   pragma Import (StdCall, DescribePixelFormat, "DescribePixelFormat");

   pragma Import (StdCall, GetPixelFormat, "GetPixelFormat");

   pragma Import (StdCall, SetPixelFormat, "SetPixelFormat");

end GL.WGL;
