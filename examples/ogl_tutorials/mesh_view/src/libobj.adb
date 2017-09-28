with Ada.Text_IO;   use Ada.Text_IO;
with GL.Types;      use GL.Types;
with Ada.Unchecked_Deallocation;

with libPNG;

package body libOBJ is

   procedure FreeVertices  is new Ada.Unchecked_Deallocation (vertices_array,  vertices_ptr);
   procedure FreeColours   is new Ada.Unchecked_Deallocation (colours_array,   colours_ptr);
   procedure FreeTextures  is new Ada.Unchecked_Deallocation (textures_array,  textures_ptr);
   procedure FreeNormals   is new Ada.Unchecked_Deallocation (normals_array,   normals_ptr);
   procedure FreeTriangles is new Ada.Unchecked_Deallocation (triangles_array, triangles_ptr);

   package Single_IO is new Ada.Text_IO.Float_IO (Single);   use Single_IO;
   package Int_IO    is new Ada.Text_IO.Integer_IO (Int);    use Int_IO;

   procedure Preview_Obj_File
     (num_vertices   : out GL.Types.Int;
      num_textures   : out GL.Types.Int;
      num_normals    : out GL.Types.Int;
      num_triangles  : out GL.Types.Int;
      mesh_file_name :     String)
   is

      Inp : File_Type;

   begin

      num_vertices  := 0;
      num_textures  := 0;
      num_normals   := 0;
      num_triangles := 0;

      Open (Inp, In_File, mesh_file_name);

      loop
         begin
            declare
               Line : String := Get_Line (Inp);
            begin
               if Line'Last > 1 then
                  declare
                     Word : String := Line (1 .. 2);
                  begin
                     if    Word = "v " then num_vertices  := num_vertices  + 1;
                     elsif Word = "vt" then num_textures  := num_textures  + 1;
                     elsif Word = "vn" then num_normals   := num_normals   + 1;
                     elsif Word = "f " then num_triangles := num_triangles + 1;
                     end if;
                  end;
               end if;
            end;
            exception when End_Error => exit;
         end;
      end loop;

      Close (Inp);

   exception
      when others =>
         Put_Line ("An exception occurred in Preview_Obj_File.");
         raise;
   end Preview_Obj_File;

   procedure Load_Obj_File
     (vertex_data       : out Singles.Vector3_Array;
      colour_data       : out Singles.Vector3_Array;
      normal_data       : out Singles.Vector3_Array;
      num_vertices      :     Int;
      num_textures      :     Int;
      num_normals       :     Int;
      num_triangles     :     Int;
      mesh_file_name    :     String;
      texture_file_name :     String)
   is

      Inp : File_Type;

      num_v  : Int := 0;
      num_vt : Int := 0;
      num_vn : Int := 0;
      num_f  : Int := 0;

      num    : Int := 0;

      vertices_acc  : vertices_ptr  := new vertices_array  (1 .. num_vertices);
      colours_acc   : colours_ptr   := new colours_array   (1 .. num_textures);  -- one colour per texture
      textures_acc  : textures_ptr  := new textures_array  (1 .. num_textures);
      normals_acc   : normals_ptr   := new normals_array   (1 .. num_normals);
      triangles_acc : triangles_ptr := new triangles_array (1 .. num_triangles);

      vertices      : vertices_array  renames vertices_acc.all;
      colours       : colours_array   renames colours_acc.all;
      textures      : textures_array  renames textures_acc.all;
      normals       : normals_array   renames normals_acc.all;
      triangles     : triangles_array renames triangles_acc.all;

      Counting_Error : exception;

      procedure add_vertex_coords (line : String) is
         x, y, z : Single;
         last    : Integer;
         start   : Integer := line'First + 1; -- + 1 to skip the leading "v"
         finish  : Integer := line'Last;
      begin

         Get (line (start .. finish), x, last);   start := last + 2;
         Get (line (start .. finish), y, last);   start := last + 2;
         Get (line (start .. finish), z, last);

         num_v := num_v + 1;

         vertices (num_v) := (x, y, z);

         -- put_line ("v: "&x'Img&' '&y'Img&' '&z'Img);

      end add_vertex_coords;

      procedure add_vertex_texture (line : String) is
         u, v   : Single;
         last   : Integer;
         start  : Integer := line'First + 2; -- + 2 to skip the leading "vt"
         finish : Integer := line'Last;
      begin

         Get (line (start .. finish), u, last);   start := last + 2;
         Get (line (start .. finish), v, last);

         num_vt := num_vt + 1;

         textures (num_vt) := (u, v);

         -- put_line ("vt: "&str(u)&' '&str(v));

      end add_vertex_texture;

      procedure add_vertex_normal (line : String) is
         p, q, r : Single;
         last    : Integer;
         start   : Integer := line'First + 2; -- + 2 to skip the leading "vn"
         finish  : Integer := line'Last;
      begin

         Get (line (start .. finish), p, last);   start := last + 2;
         Get (line (start .. finish), q, last);   start := last + 2;
         Get (line (start .. finish), r, last);

         num_vn := num_vn + 1;

         normals (num_vn) := (p, q, r);

         -- put_line ("vn: "&p'Img&' '&q'Img&' '&r'Img);

      end add_vertex_normal;

      procedure add_triangle (line : String) is
         v1, v2, v3 : Int;
         t1, t2, t3 : Int;
         n1, n2, n3 : Int;

         last   : Integer;
         start  : Integer := line'First + 1; -- + 1 to skip the leading "f"
         finish : Integer := line'Last;

      begin

         Get (line (start .. finish), v1, last);   start := last + 2;
         Get (line (start .. finish), t1, last);   start := last + 2;
         Get (line (start .. finish), n1, last);   start := last + 2;

         Get (line (start .. finish), v2, last);   start := last + 2;
         Get (line (start .. finish), t2, last);   start := last + 2;
         Get (line (start .. finish), n2, last);   start := last + 2;

         Get (line (start .. finish), v3, last);   start := last + 2;
         Get (line (start .. finish), t3, last);   start := last + 2;
         Get (line (start .. finish), n3, last);

         num_f := num_f + 1;

         triangles (num_f) := (v1, t1, n1, v2, t2, n2, v3, t3, n3);

         -- put_line ("f: "&
         --           v1'img&'/'&t1'img&'/'&n1'img&' '&
         --           v2'img&'/'&t2'img&'/'&n2'img&' '&
         --           v3'img&'/'&t3'img&'/'&n3'img);

      exception
         when others =>
            Put_Line ("An exception occurred in Add_Triangle.");
            raise;

      end add_triangle;

   begin

      Open (Inp, In_File, mesh_file_name);

      loop
         begin
            declare
               Line : String := Get_Line (Inp);
            begin
               if Line'Last > 1 then
                  declare
                     Word : String := Line (1 .. 2);
                  begin
                     if    Word = "v " then add_vertex_coords  (Line);
                     elsif Word = "vt" then add_vertex_texture (Line);
                     elsif Word = "vn" then add_vertex_normal  (Line);
                     elsif Word = "f " then add_triangle       (Line);
                     end if;
                  end;
               end if;
            end;
         exception
            when End_Error => exit;
         end;
      end loop;

      Close (Inp);

      -- probably overkill, but let's check anyway

      if (num_v  /= num_vertices)
      or (num_vt /= num_textures)
      or (num_vn /= num_normals)
      or (num_f  /= num_triangles)
      then
         raise Counting_Error;
      end if;

      declare
         use libPNG;
         num_rows, num_cols : Integer;
      begin

         -- read png file to get its dimensions
         libPNG.read_png_dimensions (num_rows, num_cols, texture_file_name);

         -- may need to move this table to the heap for large png files
         declare
            colour_table : colour_table_array (0 .. num_rows - 1, 0 .. num_cols - 1);
         begin

            -- read png once, saving rgb vlaues in colour_table
            libPNG.make_colour_table (colour_table, texture_file_name);

            -- convert all (u,v)'s to colours
            for i in Int range 1 .. num_textures loop
               colours (i) := libPNG.read_colour_table (textures (i), colour_table);
            end loop;

         end;

      end;

      num := 0;

      for i in Int range 1 .. num_triangles loop

         num := num + 1;

         vertex_data (num) := vertices (triangles (i) (1));
         colour_data (num) := colours  (triangles (i) (2));
         normal_data (num) := normals  (triangles (i) (3));

         num := num + 1;

         vertex_data (num) := vertices (triangles (i) (4));
         colour_data (num) := colours  (triangles (i) (5));
         normal_data (num) := normals  (triangles (i) (6));

         num := num + 1;

         vertex_data (num) := vertices (triangles (i) (7));
         colour_data (num) := colours  (triangles (i) (8));
         normal_data (num) := normals  (triangles (i) (9));

      end loop;

      FreeVertices  (vertices_acc);
      FreeColours   (colours_acc);
      FreeTextures  (textures_acc);
      FreeNormals   (normals_acc);
      FreeTriangles (triangles_acc);

   exception
      when others =>
         Put_Line ("An exception occurred in Load_Obj_File.");
         raise;
   end Load_Obj_File;

end libOBJ;
