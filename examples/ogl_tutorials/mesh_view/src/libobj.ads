with GL;
with GL.Types;

package libOBJ is

   type triangle_array  is array (1 .. 9) of GL.Types.Int;

   type vertices_array  is array (GL.Types.Int range <>) of GL.Types.Singles.Vector3;
   type colours_array   is array (GL.Types.Int range <>) of GL.Types.Singles.Vector3;  -- rgb colour
   -- type colours_array   is Array (GL.Types.Int range <>) of GL.Types.Colors.Color;  -- rgba colour
   type textures_array  is array (GL.Types.Int range <>) of GL.Types.Singles.Vector2;
   type normals_array   is array (GL.Types.Int range <>) of GL.Types.Singles.Vector3;
   type triangles_array is array (GL.Types.Int range <>) of triangle_array;

   type vertices_ptr    is access vertices_array;
   type colours_ptr     is access colours_array;
   type textures_ptr    is access textures_array;
   type normals_ptr     is access normals_array;
   type triangles_ptr   is access triangles_array;

   procedure Preview_Obj_File
     (num_vertices   : out GL.Types.Int;
      num_textures   : out GL.Types.Int;
      num_normals    : out GL.Types.Int;
      num_triangles  : out GL.Types.Int;
      mesh_file_name :     String);

   procedure Load_Obj_File
     (vertex_data       : out GL.Types.Singles.Vector3_Array;
      colour_data       : out GL.Types.Singles.Vector3_Array;
      normal_data       : out GL.Types.Singles.Vector3_Array;
      num_vertices      :     GL.Types.Int;
      num_textures      :     GL.Types.Int;
      num_normals       :     GL.Types.Int;
      num_triangles     :     GL.Types.Int;
      mesh_file_name    :     String;
      texture_file_name :     String);

end libOBJ;