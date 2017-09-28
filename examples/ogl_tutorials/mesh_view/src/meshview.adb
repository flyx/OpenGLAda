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
