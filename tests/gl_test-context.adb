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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GL.Context;

with GL_Test.Display_Backend;

procedure GL_Test.Context is

   procedure Display_Context_Information is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
   begin
      begin
         Put_Line ("Major version: " & GL.Context.Major_Version'Img);
         Put_Line ("Minor version: " & GL.Context.Minor_Version'Img);
      exception
         when others =>
            Put_Line ("OpenGL version too old for querying major and minor");
      end;
      Put_Line ("Whole version string: " & GL.Context.Version_String);
      Put_Line ("Vendor: " & GL.Context.Renderer);
      Put_Line ("Renderer: " & GL.Context.Renderer);
      Put_Line ("Extensions: ");
      declare
         List : GL.Context.String_List := GL.Context.Extensions;
      begin
         for I in List'Range loop
            declare
               Name : String := To_String (List (I));
            begin
               Put_Line ("  " & Name);
               if not GL.Context.Has_Extension (Name) then
                  Put_Line
                    ("!!! Has_Extension returns false (should not happen)");
               end if;
            end;
         end loop;
      end;
      begin
         Put_Line ("Primary shading language version: " &
                   GL.Context.Primary_Shading_Language_Version);
      exception
         when others =>
            Put_Line ("OpenGL version too old for querying " &
                      "Primary shading language version");
            return;
      end;
      begin
         declare
            List : GL.Context.String_List :=
              GL.Context.Supported_Shading_Language_Versions;
         begin
            Put_Line ("Supported shading language versions:");
            for I in List'Range loop
               declare
                  Version : String := To_String (List (I));
               begin
                  Put_Line ("  " & Version);
                  if not GL.Context.Supports_Shading_Language_Version
                    (Version)
                  then
                     Put_Line ("!!! Supports_Shading_Language_Version " &
                               "returns false (should not happen)");
                  end if;
               end;
            end loop;
         end;
      exception
         when others =>
            Put_Line ("OpenGL version too old for querying supported versions");
      end;
   end Display_Context_Information;

begin
   Display_Backend.Init;
   
   Ada.Text_IO.Put_Line
     ("Getting information without request for OpenGL version.");
   
   -- create a window so we have a context
   Display_Backend.Open_Window (Width  => 500, Height => 500);
 
   Display_Context_Information;
   Ada.Text_IO.New_Line;
 
   Display_Backend.Close_Window;

   Ada.Text_IO.Put_Line
     ("Getting information with requested OpenGL version 2.1:");

   Display_Backend.Configure_Minimum_OpenGL_Version (Major => 2, Minor => 1);

   Display_Backend.Open_Window (Width  => 500, Height => 500);

   Display_Context_Information;

   Display_Backend.Close_Window;

   Ada.Text_IO.Put_Line
     ("Getting information with requested OpenGL version 3.2:");

   Display_Backend.Configure_Minimum_OpenGL_Version (Major => 3, Minor => 2);

   Display_Backend.Open_Window (Width  => 500, Height => 500);

   Display_Context_Information;

   Display_Backend.Shutdown;
end GL_Test.Context;
