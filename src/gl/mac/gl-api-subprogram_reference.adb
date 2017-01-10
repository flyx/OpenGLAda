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

private with GL.API.Mac_OS_X;

function GL.API.Subprogram_Reference (Function_Name : String)
  return System.Address is

   -- OSX-specific implementation uses CoreFoundation functions
   use GL.API.Mac_OS_X;

   package IFC renames Interfaces.C.Strings;

   GL_Function_Name_C : IFC.chars_ptr := IFC.New_String (Function_Name);

   Symbol_Name : constant CFStringRef := CFStringCreateWithCString
      (alloc => System.Null_Address, cStr => GL_Function_Name_C,
      encoding => kCFStringEncodingASCII);
   Result : constant System.Address := CFBundleGetFunctionPointerForName
      (bundle => OpenGLFramework,
      functionName => Symbol_Name);
begin
   CFRelease (Symbol_Name);
   IFC.Free (GL_Function_Name_C);
   return Result;
end GL.API.Subprogram_Reference;
