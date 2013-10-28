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

with System;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;

with GL.Low_Level;
with GL.Types;

package GL.CGL is
   pragma Preelaborate;
   
   use GL.Types;

   --  CGL types and constants

   subtype CGLContextObject      is System.Address;
   subtype CGLPixelFormatObject  is System.Address;
   subtype CGLRendererInfoObject is System.Address;
   subtype CGLPBufferObject      is System.Address;
   subtype CGLShareGroup         is System.Address;

   type CGLPixelFormatAttribute is (Terminator               ,
                                    kCGLPFAAllRenderers      ,
                                    kCGLPFATripleBuffer      ,
                                    kCGLPFADoubleBuffer      ,
                                    kCGLPFAStereo            ,
                                    kCGLPFAAuxBuffers        ,
                                    kCGLPFAColorSize         ,
                                    kCGLPFAAlphaSize         ,
                                    kCGLPFADepthSize         ,
                                    kCGLPFAStencilSize       ,
                                    kCGLPFAAccumSize         ,
                                    kCGLPFAMinimumPolicy     ,
                                    kCGLPFAMaximumPolicy     ,
                                    kCGLPFAOffScreen         ,
                                    kCGLPFAFullScreen        ,
                                    kCGLPFASampleBuffers     ,
                                    kCGLPFASamples           ,
                                    kCGLPFAAuxDepthStencil   ,
                                    kCGLPFAColorFloat        ,
                                    kCGLPFAMultisample       ,
                                    kCGLPFASupersample       ,
                                    kCGLPFASampleAlpha       ,

                                    kCGLPFARendererID        ,
                                    kCGLPFASingleRenderer    ,
                                    kCGLPFANoRecovery        ,
                                    kCGLPFAAccelerated       ,
                                    kCGLPFAClosestPolicy     ,
                                    kCGLPFARobust            ,
                                    kCGLPFABackingStore      ,
                                    kCGLPFAMPSafe            ,
                                    kCGLPFAWindow            ,
                                    kCGLPFAMultiScreen       ,
                                    kCGLPFACompliant         ,
                                    kCGLPFADisplayMask       ,
                                    kCGLPFAPBuffer           ,
                                    kCGLPFARemotePBuffer     ,
                                    kCGLPFAAllowOfflineRenderers,
                                    kCGLPFAAcceleratedCompute,
                                    kCGLPFAOpenGLProfile     ,
                                    kCGLPFAVirtualScreenCount
                                   );

   type CGLRendererProperty is (kCGLRPOffScreen             ,
                                kCGLRPFullScreen            ,
                                kCGLRPRendererID            ,
                                kCGLRPAccelerated           ,
                                kCGLRPRobust                ,
                                kCGLRPBackingStore          ,
                                kCGLRPMPSafe                ,
                                kCGLRPWindow                ,
                                kCGLRPMultiScreen           ,
                                kCGLRPCompliant             ,
                                kCGLRPDisplayMask           ,
                                kCGLRPBufferModes           ,
                                kCGLRPColorModes            ,
                                kCGLRPAccumModes            ,
                                kCGLRPDepthModes            ,
                                kCGLRPStencilModes          ,
                                kCGLRPMaxAuxBuffers         ,
                                kCGLRPMaxSampleBuffers      ,
                                kCGLRPMaxSamples            ,
                                kCGLRPSampleModes           ,
                                kCGLRPSampleAlpha           ,
                                kCGLRPVideoMemory           ,
                                kCGLRPTextureMemory         ,
                                kCGLRPGPUVertProcCapable    ,
                                kCGLRPGPUFragProcCapable    ,
                                kCGLRPRendererCount         ,
                                kCGLRPOnline                ,
                                kCGLRPAcceleratedCompute    ,
                                kCGLRPVideoMemoryMegabytes  ,
                                kCGLRPTextureMemoryMegabytes
                               );

   type CGLContextEnable is (kCGLCESwapRectangle  ,
                             kCGLCESwapLimit      ,
                             kCGLCERasterization  ,
                             kCGLCEStateValidation,
                             kCGLCESurfaceBackingSize,
                             kCGLCEDisplayListOptimization,
                             kCGLCEMPEngine       ,
                             kCGLCECrashOnRemovedFunctions
                            );

   type CGLContextParameter is (kCGLCPSwapRectangle         ,
                                kCGLCPSwapInterval          ,
                                kCGLCPDispatchTableSize     ,
                                kCGLCPClientStorage         ,
                                kCGLCPSurfaceTexture        ,
                                kCGLCPSurfaceOrder          ,
                                kCGLCPSurfaceOpacity        ,
                                kCGLCPSurfaceBackingSize    ,
                                kCGLCPSurfaceSurfaceVolatile,
                                kCGLCPReclaimResources      ,
                                kCGLCPCurrentRendererID     ,
                                kCGLCPGPUVertexProcessing   ,
                                kCGLCPGPUFragmentProcessing ,
                                kCGLCPHasDrawable           ,
                                kCGLCPMPSwapsInFlight
                               );

   type CGLGlobalOption is (kCGLGOFormatCacheSize ,
                            kCGLGOClearFormatCache,
                            kCGLGORetainRenderers ,
                            kCGLGOResetLibrary    ,
                            kCGLGOUseErrorHandler ,
                            kCGLGOUseBuildCache
                           );

   type CGLOpenGLProfile is (kCGLOGLPVersion_Legacy  ,
                             kCGLOGLPVersion_3_2_Core
                            );

   type CGLError is (kCGLNoError           ,
                     kCGLBadAttribute      ,
                     kCGLBadProperty       ,
                     kCGLBadPixelFormat    ,
                     kCGLBadRendererInfo   ,
                     kCGLBadContext        ,
                     kCGLBadDrawable       ,
                     kCGLBadDisplay        ,
                     kCGLBadState          ,
                     kCGLBadValue          ,
                     kCGLBadMatch          ,
                     kCGLBadEnumeration    ,
                     kCGLBadOffScreen      ,
                     kCGLBadFullScreen     ,
                     kCGLBadWindow         ,
                     kCGLBadAddress        ,
                     kCGLBadCodeModule     ,
                     kCGLBadAlloc          ,
                     kCGLBadConnection
                    );

   kCGLMonoscopicBit  : constant := 16#00000001#;
   kCGLStereoscopicBit: constant := 16#00000002#;
   kCGLSingleBufferBit: constant := 16#00000004#;
   kCGLDoubleBufferBit: constant := 16#00000008#;
   kCGLTripleBufferBit: constant := 16#00000010#;
   kCGL0Bit           : constant := 16#00000001#;
   kCGL1Bit           : constant := 16#00000002#;
   kCGL2Bit           : constant := 16#00000004#;
   kCGL3Bit           : constant := 16#00000008#;
   kCGL4Bit           : constant := 16#00000010#;
   kCGL5Bit           : constant := 16#00000020#;
   kCGL6Bit           : constant := 16#00000040#;
   kCGL8Bit           : constant := 16#00000080#;
   kCGL10Bit          : constant := 16#00000100#;
   kCGL12Bit          : constant := 16#00000200#;
   kCGL16Bit          : constant := 16#00000400#;
   kCGL24Bit          : constant := 16#00000800#;
   kCGL32Bit          : constant := 16#00001000#;
   kCGL48Bit          : constant := 16#00002000#;
   kCGL64Bit          : constant := 16#00004000#;
   kCGL96Bit          : constant := 16#00008000#;
   kCGL128Bit         : constant := 16#00010000#;
   kCGLRGB444Bit      : constant := 16#00000040#;
   kCGLARGB4444Bit    : constant := 16#00000080#;
   kCGLRGB444A8Bit    : constant := 16#00000100#;
   kCGLRGB555Bit      : constant := 16#00000200#;
   kCGLARGB1555Bit    : constant := 16#00000400#;
   kCGLRGB555A8Bit    : constant := 16#00000800#;
   kCGLRGB565Bit      : constant := 16#00001000#;
   kCGLRGB565A8Bit    : constant := 16#00002000#;
   kCGLRGB888Bit      : constant := 16#00004000#;
   kCGLARGB8888Bit    : constant := 16#00008000#;
   kCGLRGB888A8Bit    : constant := 16#00010000#;
   kCGLRGB101010Bit   : constant := 16#00020000#;
   kCGLARGB2101010Bit : constant := 16#00040000#;
   kCGLRGB101010_A8Bit: constant := 16#00080000#;
   kCGLRGB121212Bit   : constant := 16#00100000#;
   kCGLARGB12121212Bit: constant := 16#00200000#;
   kCGLRGB161616Bit   : constant := 16#00400000#;
   kCGLRGBA16161616Bit: constant := 16#00800000#;
   kCGLRGBFloat64Bit  : constant := 16#01000000#;
   kCGLRGBAFloat64Bit : constant := 16#02000000#;
   kCGLRGBFloat128Bit : constant := 16#04000000#;
   kCGLRGBAFloat128Bit: constant := 16#08000000#;
   kCGLRGBFloat256Bit : constant := 16#10000000#;
   kCGLRGBAFloat256Bit: constant := 16#20000000#;

   kCGLSupersampleBit : constant := 16#00000001#;
   kCGLMultisampleBit : constant := 16#00000002#;

   type CGLPixelFormatAttribute_Array is array (Positive range <>) of
     aliased CGLPixelFormatAttribute;


   --  Pixel format functions

   function CGLChoosePixelFormat (attribs : access CGLPixelFormatAttribute;
                                  pix     : access CGLPixelFormatObject;
                                  npix    : access Int)
                                  return CGLError;

   function CGLDestroyPixelFormat (pix : CGLPixelFormatObject) return CGLError;

   function CGLDescribePixelFormat (pix : CGLPixelFormatObject;
                                    pix_num : Int;
                                    attrib : CGLPixelFormatAttribute;
                                    value : access Int)
                                    return CGLError;

   procedure CGLReleasePixelFormat (pix : in CGLPixelFormatObject);

   function CGLRetainPixelFormat (pix : CGLPixelFormatObject)
                                  return CGLPixelFormatObject;

   function CGLGetPixelFormatRetainCount (pix : CGLPixelFormatObject)
                                          return UInt;

   function CGLQueryRendererInfo (display_mask : UInt;
                                  rend : access CGLRendererInfoObject;
                                  nrend : access Int) return CGLError;

   function CGLDestroyRendererInfo (rend : CGLRendererInfoObject)
                                    return CGLError;

   function CGLDescribeRenderer (rend : CGLRendererInfoObject; rend_num : Int;
                                 prop : CGLRendererProperty;
                                 value : access Int) return CGLError;

   function CGLCreateContext (pix : CGLPixelFormatObject;
                              share : CGLContextObject;
                              ctx : access CGLContextObject) return CGLError;

   function CGLDestroyContext (ctx : CGLContextObject) return CGLError;

   function CGLCopyContext (src, dst : CGLContextObject;
                            mask : Low_Level.Bitfield) return CGLError;

   function CGLRetainContext (ctx : CGLContextObject) return CGLContextObject;

   procedure CGLReleaseContext (ctx : in CGLContextObject);

   function CGLGetContextRetainCount (ctx : CGLContextObject) return UInt;

   function CGLGetPixelFormat (ctx : CGLContextObject) return CGLPixelFormatObject;

   function CGLCreatePBuffer (width, height : Size;
                              target, internalFormat : Low_Level.Enum;
                              max_level : Int;
                              pbuffer : access CGLPBufferObject)
                              return CGLError;

   function CGLDestroyPBuffer (pbuffer : CGLPBufferObject) return CGLError;

   function CGLDescribePBuffer (obj : CGLPBufferObject;
                                width, height : access Size;
                                target, internalFormat : access Low_Level.Enum;
                                mipmap : access Int) return CGLError;

   function CGLTexImagePBuffer (ctx : CGLContextObject;
                                pbuffer : CGLPBufferObject;
                                source : Low_Level.Enum) return CGLError;

   function CGLRetainPBuffer (pbuffer : CGLPBufferObject)
                              return CGLPBufferObject;

   procedure CGLReleasePBuffer (pbuffer : in CGLPBufferObject);

   function CGLGetPBufferRetainCount (pbuffer : CGLPBufferObject) return UInt;

   function CGLSetOffScreen (ctx : CGLContextObject;
                             width, height : Size;
                             rowbytes : Int;
                             baseaddr : Interfaces.C.Extensions.void_ptr)
                             return CGLError;

   function CGLGetOffScreen (ctx : CGLContextObject;
                             width, height : access Size;
                             rowbytes : access Int;
                             baseaddr : access Interfaces.C.Extensions.void_ptr)
                             return CGLError;

   function CGLSetFullScreen (ctx : CGLContextObject) return CGLError;

   function CGLSetFullScreenOnDisplay (ctx : CGLContextObject;
                                       display_mask : UInt) return CGLError;

   function CGLSetPBuffer (ctx : CGLContextObject;
                           pbuffer : CGLPBufferObject;
                           face : Low_Level.Enum;
                           level, screen : Int) return CGLError;

   function CGLGetPBuffer (ctx : CGLContextObject;
                           pbuffer : access CGLPBufferObject;
                           face : access Low_Level.Enum;
                           level, screen : access Int) return CGLError;

   function CGLClearDrawable (ctx : CGLContextObject) return CGLError;

   function CGLFlushDrawable (ctx : CGLContextObject) return CGLError;

   function CGLEnable (ctx : CGLContextObject; pname : CGLContextEnable)
                       return CGLError;

   function CGLDisable (ctx : CGLContextObject; pname : CGLContextEnable)
                        return CGLError;

   function CGLIsEnabled (ctx : CGLContextObject; pname : CGLContextEnable;
                          enable : access Int) return CGLError;

   function CGLSetParameter (ctx : CGLContextObject;
                             pname : CGLContextParameter;
                             params : access constant Int) return CGLError;

   function CGLGetParameter (ctx : CGLContextObject;
                             pname : CGLContextParameter;
                             params : access Int) return CGLError;

   function CGLSetVirtualScreen (ctx : CGLContextObject; screen : Int)
                                 return CGLError;

   function CGLGetVirtualScreen (ctx : CGLContextObject; screen : access Int)
                                 return CGLError;

   function CGLUpdateContext (ctx : CGLContextObject) return CGLError;

   function CGLSetGlobalOption (pname : CGLGlobalOption;
                                params : access constant Int) return CGLError;

   function CGLGetGlobalOption (pname : CGLGlobalOption;
                                params : access Int) return CGLError;

   function CGLSetOption (pname : CGLGlobalOption; param : Int)
                          return CGLError;

   function CGLGetOption (pname : CGLGlobalOption;
                          param : access Int) return CGLError;

   function CGLLockContext (ctx : CGLContextObject) return CGLError;

   function CGLUnlockContext (ctx : CGLContextObject) return CGLError;

   procedure CGLGetVersion (majorvers, minorvers : out Int);

   function CGLErrorString (error : CGLError)
                            return Interfaces.C.Strings.chars_ptr;

   function CGLSetCurrentContext (ctx : CGLContextObject) return CGLError;

   function CGLGetCurrentContext return CGLContextObject;
   
   function CGLGetShareGroup (ctx : CGLContextObject) return CGLShareGroup;

private
   C_Enum_Size : constant := 32;

   for CGLPixelFormatAttribute use (Terminator                =>   0,
                                    kCGLPFAAllRenderers       =>   1,
                                    kCGLPFATripleBuffer       =>   3,
                                    kCGLPFADoubleBuffer       =>   5,
                                    kCGLPFAStereo             =>   6,
                                    kCGLPFAAuxBuffers         =>   7,
                                    kCGLPFAColorSize          =>   8,
                                    kCGLPFAAlphaSize          =>  11,
                                    kCGLPFADepthSize          =>  12,
                                    kCGLPFAStencilSize        =>  13,
                                    kCGLPFAAccumSize          =>  14,
                                    kCGLPFAMinimumPolicy      =>  51,
                                    kCGLPFAMaximumPolicy      =>  52,
                                    kCGLPFAOffScreen          =>  53,
                                    kCGLPFAFullScreen         =>  54,
                                    kCGLPFASampleBuffers      =>  55,
                                    kCGLPFASamples            =>  56,
                                    kCGLPFAAuxDepthStencil    =>  57,
                                    kCGLPFAColorFloat         =>  58,
                                    kCGLPFAMultisample        =>  59,
                                    kCGLPFASupersample        =>  60,
                                    kCGLPFASampleAlpha        =>  61,

                                    kCGLPFARendererID         =>  70,
                                    kCGLPFASingleRenderer     =>  71,
                                    kCGLPFANoRecovery         =>  72,
                                    kCGLPFAAccelerated        =>  73,
                                    kCGLPFAClosestPolicy      =>  74,
                                    kCGLPFARobust             =>  75,
                                    kCGLPFABackingStore       =>  76,
                                    kCGLPFAMPSafe             =>  78,
                                    kCGLPFAWindow             =>  80,
                                    kCGLPFAMultiScreen        =>  81,
                                    kCGLPFACompliant          =>  83,
                                    kCGLPFADisplayMask        =>  84,
                                    kCGLPFAPBuffer            =>  90,
                                    kCGLPFARemotePBuffer      =>  91,
                                    kCGLPFAAllowOfflineRenderers => 96,
                                    kCGLPFAAcceleratedCompute =>  97,
                                    kCGLPFAOpenGLProfile      =>  99,
                                    kCGLPFAVirtualScreenCount => 128
                                   );
   for CGLPixelFormatAttribute'Size use C_Enum_Size;
   pragma Convention (C, CGLPixelFormatAttribute);

   for CGLRendererProperty use (kCGLRPOffScreen              =>  53,
                                kCGLRPFullScreen             =>  54,
                                kCGLRPRendererID             =>  70,
                                kCGLRPAccelerated            =>  73,
                                kCGLRPRobust                 =>  75,
                                kCGLRPBackingStore           =>  76,
                                kCGLRPMPSafe                 =>  78,
                                kCGLRPWindow                 =>  80,
                                kCGLRPMultiScreen            =>  81,
                                kCGLRPCompliant              =>  83,
                                kCGLRPDisplayMask            =>  84,
                                kCGLRPBufferModes            => 100,
                                kCGLRPColorModes             => 103,
                                kCGLRPAccumModes             => 104,
                                kCGLRPDepthModes             => 105,
                                kCGLRPStencilModes           => 106,
                                kCGLRPMaxAuxBuffers          => 107,
                                kCGLRPMaxSampleBuffers       => 108,
                                kCGLRPMaxSamples             => 109,
                                kCGLRPSampleModes            => 110,
                                kCGLRPSampleAlpha            => 111,
                                kCGLRPVideoMemory            => 120,
                                kCGLRPTextureMemory          => 121,
                                kCGLRPGPUVertProcCapable     => 122,
                                kCGLRPGPUFragProcCapable     => 123,
                                kCGLRPRendererCount          => 128,
                                kCGLRPOnline                 => 129,
                                kCGLRPAcceleratedCompute     => 130,
                                kCGLRPVideoMemoryMegabytes   => 131,
                                kCGLRPTextureMemoryMegabytes => 132
                               );
   for CGLRendererProperty'Size use C_Enum_Size;
   pragma Convention (C, CGLRendererProperty);

   for CGLContextEnable use (kCGLCESwapRectangle           => 201,
                             kCGLCESwapLimit               => 203,
                             kCGLCERasterization           => 221,
                             kCGLCEStateValidation         => 301,
                             kCGLCESurfaceBackingSize      => 305,
                             kCGLCEDisplayListOptimization => 307,
                             kCGLCEMPEngine                => 313,
                             kCGLCECrashOnRemovedFunctions => 316
                            );
   for CGLContextEnable'Size use C_Enum_Size;
   pragma Convention (C, CGLContextEnable);

   for CGLContextParameter use (kCGLCPSwapRectangle          => 200,
                                kCGLCPSwapInterval           => 222,
                                kCGLCPDispatchTableSize      => 224,
                                kCGLCPClientStorage          => 226,
                                kCGLCPSurfaceTexture         => 228,
                                kCGLCPSurfaceOrder           => 235,
                                kCGLCPSurfaceOpacity         => 236,
                                kCGLCPSurfaceBackingSize     => 304,
                                kCGLCPSurfaceSurfaceVolatile => 306,
                                kCGLCPReclaimResources       => 308,
                                kCGLCPCurrentRendererID      => 309,
                                kCGLCPGPUVertexProcessing    => 310,
                                kCGLCPGPUFragmentProcessing  => 311,
                                kCGLCPHasDrawable            => 314,
                                kCGLCPMPSwapsInFlight	     => 315
                               );
   for CGLContextParameter'Size use C_Enum_Size;
   pragma Convention (C, CGLContextParameter);

   for CGLGlobalOption use (kCGLGOFormatCacheSize  => 501,
                            kCGLGOClearFormatCache => 502,
                            kCGLGORetainRenderers  => 503,
                            kCGLGOResetLibrary     => 504,
                            kCGLGOUseErrorHandler  => 505,
                            kCGLGOUseBuildCache    => 506
                           );
   for CGLGlobalOption'Size use C_Enum_Size;
   pragma Convention (C, CGLGlobalOption);

   for CGLOpenGLProfile use (kCGLOGLPVersion_Legacy   => 16#1000#,
                             kCGLOGLPVersion_3_2_Core => 16#3200#
                            );
   for CGLOpenGLProfile'Size use C_Enum_Size;
   pragma Convention (C, CGLOpenGLProfile);

   for CGLError use (kCGLNoError            => 0,
                     kCGLBadAttribute       => 10000,
                     kCGLBadProperty        => 10001,
                     kCGLBadPixelFormat     => 10002,
                     kCGLBadRendererInfo    => 10003,
                     kCGLBadContext         => 10004,
                     kCGLBadDrawable        => 10005,
                     kCGLBadDisplay         => 10006,
                     kCGLBadState           => 10007,
                     kCGLBadValue           => 10008,
                     kCGLBadMatch           => 10009,
                     kCGLBadEnumeration     => 10010,
                     kCGLBadOffScreen       => 10011,
                     kCGLBadFullScreen      => 10012,
                     kCGLBadWindow          => 10013,
                     kCGLBadAddress         => 10014,
                     kCGLBadCodeModule      => 10015,
                     kCGLBadAlloc           => 10016,
                     kCGLBadConnection      => 10017
                    );
   for CGLError'Size use C_Enum_Size;
   pragma Convention (C, CGLError);

   pragma Import (C, CGLChoosePixelFormat, "CGLChoosePixelFormat");
   pragma Import (C, CGLDestroyPixelFormat, "CGLDestroyPixelFormat");
   pragma Import (C, CGLDescribePixelFormat, "CGLDescribePixelFormat");
   pragma Import (C, CGLReleasePixelFormat, "CGLReleasePixelFormat");
   pragma Import (C, CGLRetainPixelFormat, "CGLRetainPixelFormat");
   pragma Import (C, CGLGetPixelFormatRetainCount, "CGLGetPixelFormatRetainCount");

   pragma Import (C, CGLQueryRendererInfo, "CGLQueryRendererInfo");
   pragma Import (C, CGLDestroyRendererInfo, "CGLDestroyRendererInfo");
   pragma Import (C, CGLDescribeRenderer, "CGLDescribeRenderer");

   pragma Import (C, CGLCreateContext, "CGLCreateContext");
   pragma Import (C, CGLDestroyContext, "CGLDestroyContext");
   pragma Import (C, CGLCopyContext, "CGLCopyContext");
   pragma Import (C, CGLRetainContext, "CGLRetainContext");
   pragma Import (C, CGLReleaseContext, "CGLReleaseContext");
   pragma Import (C, CGLGetContextRetainCount, "CGLGetContextRetainCount");
   pragma Import (C, CGLGetPixelFormat, "CGLGetPixelFormat");

   pragma Import (C, CGLCreatePBuffer, "CGLCreatePBuffer");
   pragma Import (C, CGLDestroyPBuffer, "CGLDestroyPBuffer");
   pragma Import (C, CGLDescribePBuffer, "CGLDescribePBuffer");
   pragma Import (C, CGLTexImagePBuffer, "CGLTexImagePBuffer");
   pragma Import (C, CGLRetainPBuffer, "CGLRetainPBuffer");
   pragma Import (C, CGLReleasePBuffer, "CGLReleasePBuffer");
   pragma Import (C, CGLGetPBufferRetainCount, "CGLGetPBufferRetainCount");

   pragma Import (C, CGLSetOffScreen, "CGLSetOffScreen");
   pragma Import (C, CGLGetOffScreen, "CGLGetOffScreen");
   pragma Import (C, CGLSetFullScreen, "CGLSetFullScreen");
   pragma Import (C, CGLSetFullScreenOnDisplay, "CGLSetFullScreenOnDisplay");
   pragma Import (C, CGLSetPBuffer, "CGLSetPBuffer");
   pragma Import (C, CGLGetPBuffer, "CGLGetPBuffer");
   pragma Import (C, CGLClearDrawable, "CGLClearDrawable");
   pragma Import (C, CGLFlushDrawable, "CGLFlushDrawable");

   pragma Import (C, CGLEnable, "CGLEnable");
   pragma Import (C, CGLDisable, "CGLDisable");
   pragma Import (C, CGLIsEnabled, "CGLIsEnabled");
   pragma Import (C, CGLSetParameter, "CGLSetParameter");
   pragma Import (C, CGLGetParameter, "CGLGetParameter");

   pragma Import (C, CGLSetVirtualScreen, "CGLSetVirtualScreen");
   pragma Import (C, CGLGetVirtualScreen, "CGLGetVirtualScreen");
   pragma Import (C, CGLUpdateContext, "CGLUpdateContext");

   pragma Import (C, CGLSetGlobalOption, "CGLSetGlobalOption");
   pragma Import (C, CGLGetGlobalOption, "CGLGetGlobalOption");
   pragma Import (C, CGLSetOption, "CGLSetOption");
   pragma Import (C, CGLGetOption, "CGLGetOption");

   pragma Import (C, CGLLockContext, "CGLLockContext");
   pragma Import (C, CGLUnlockContext, "CGLUnlockContext");


   pragma Import (C, CGLGetVersion, "CGLGetVersion");
   pragma Import (C, CGLErrorString, "CGLErrorString");

   pragma Import (C, CGLSetCurrentContext, "CGLSetCurrentContext");
   pragma Import (C, CGLGetCurrentContext, "CGLGetCurrentContext");
   
   pragma Import (C, CGLGetShareGroup, "CGLGetShareGroup");
end GL.CGL;
