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
