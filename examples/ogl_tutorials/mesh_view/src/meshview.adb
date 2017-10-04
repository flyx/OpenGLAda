-- Copyright (c) 2017, Leo Brewin <Leo.Brewin@monash.edu>
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

with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Directories; use Ada.Directories;

with Glfw;

with Initialize;

with meshviewlib; use meshviewlib;

procedure meshview is

   Window_Title      : String := "meshview";

   mesh_not_found    : exception;
   texture_not_found : exception;

begin

   if not Exists (mesh_file_name) then
      raise mesh_not_found;
   end if;

   if not Exists (texture_file_name) then
      raise texture_not_found;
   end if;

   Glfw.Init;

   Initialize (Main_Window, Window_Title);

   Main_Loop (Main_Window);

   Glfw.Shutdown;

exception

   when mesh_not_found =>
      Put_Line ("Could not find the mesh file: '" & mesh_file_name & "'");

   when texture_not_found =>
      Put_Line ("Could not find the texture file: '" & texture_file_name & "'");

   when anError : Constraint_Error =>
      Put ("meshview returned constraint error: ");
      Put_Line (Exception_Information (anError));

   when anError : others =>
      Put_Line ("An exception occurred in meshview.");
      Put_Line (Exception_Information (anError));

end meshview;
