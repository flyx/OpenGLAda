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

with Interfaces.C.Strings;
with System;
private package GL.API.Mac_OS_X is
   pragma Preelaborate;
   
   -- package for MacOSX-specific stuff
      
   package IFC renames Interfaces.C.Strings;
   
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
                                       cStr     : IFC.chars_ptr;
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