--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C;
with System;
private package GL.API.Mac_OS_X is
   pragma Preelaborate;

   -- package for MacOSX-specific stuff

   package IFC renames Interfaces.C;

   subtype CFStringRef    is System.Address;
   subtype CFBundleRef    is System.Address;
   subtype CFAllocatorRef is System.Address;

   type CFStringEncoding is new Integer;
   pragma Convention (C, CFStringEncoding);
   for CFStringEncoding'Size use 32;

   kCFStringEncodingASCII : constant CFStringEncoding := 16#0600#;

   function CFBundleGetBundleWithIdentifier (bundleID : CFStringRef)
     return CFBundleRef;
   pragma Import (Convention => C, Entity => CFBundleGetBundleWithIdentifier,
                  External_Name => "CFBundleGetBundleWithIdentifier");

   function CFStringCreateWithCString (alloc    : CFAllocatorRef;
                                       cStr     : IFC.char_array;
                                       encoding : CFStringEncoding)
                                       return CFStringRef;
   pragma Import (Convention => C, Entity => CFStringCreateWithCString,
                  External_Name => "CFStringCreateWithCString");

   function CFBundleGetFunctionPointerForName (bundle : CFBundleRef;
                                               functionName : CFStringRef)
                                              return System.Address;
   pragma Import (Convention => C, Entity => CFBundleGetFunctionPointerForName,
                  External_Name => "CFBundleGetFunctionPointerForName");

   procedure CFRelease (cf : System.Address);
   pragma Import (Convention => C, Entity => CFRelease,
                  External_Name => "CFRelease");

   function OpenGLFramework return CFBundleRef;


end GL.API.Mac_OS_X;
