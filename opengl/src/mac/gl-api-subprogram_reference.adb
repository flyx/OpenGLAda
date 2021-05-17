--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

private with GL.API.Mac_OS_X;

function GL.API.Subprogram_Reference (Function_Name : String)
  return System.Address is

   -- OSX-specific implementation uses CoreFoundation functions
   use GL.API.Mac_OS_X;

   package IFC renames Interfaces.C;

   Symbol_Name : constant CFStringRef := CFStringCreateWithCString
      (alloc => System.Null_Address, cStr => IFC.To_C (Function_Name),
      encoding => kCFStringEncodingASCII);
   Result : constant System.Address := CFBundleGetFunctionPointerForName
      (bundle => OpenGLFramework,
      functionName => Symbol_Name);
begin
   CFRelease (Symbol_Name);
   return Result;
end GL.API.Subprogram_Reference;
