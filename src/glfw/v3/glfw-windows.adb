--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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

with Glfw.API;
with Glfw.Enums;

package body Glfw.Windows is

   procedure Init
     (Object        : not null access Window;
      Width, Height : Natural;
      Title         : String;
      Monitor       : Monitors.Monitor;
      Share_Resources_With : access Window'Class := null)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Init unimplemented");
      raise Program_Error with "Unimplemented procedure Init";
   end Init;

   procedure Enable_Callback
     (Object : not null access Window;
      Subject : Callback)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Enable_Callback unimplemented");
      raise Program_Error with "Unimplemented procedure Enable_Callback";
   end Enable_Callback;

   procedure Get_OpenGL_Version (Object : not null access Window;
                                 Major, Minor, Revision : out Natural) is
      Value : Interfaces.C.int;
   begin
      Value := API.Get_Window_Attrib (Object.Handle, Enums.Context_Version_Major);
      Major := Natural (Value);
      Value := API.Get_Window_Attrib (Object.Handle, Enums.Context_Version_Minor);
      Minor := Natural (Value);
      Value := API.Get_Window_Attrib (Object.Handle, Enums.Context_Revision);
      Revision := Natural (Value);
   end Get_OpenGL_Version;

end Glfw.Windows;
