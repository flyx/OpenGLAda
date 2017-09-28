with Ada.Command_Line; use Ada.Command_Line;

package body libCLI is

   function get_mesh_name return String is
   begin
      if Argument_Count > 0
         then return Argument (1);
         else return "meshes/cube.obj";
      end if;
   end get_mesh_name;

   function get_texture_name return String is
   begin
      if Argument_Count > 1
         then return Argument (2);
         else return "maps/bwr.png";
      end if;
   end get_texture_name;

end libCLI;
