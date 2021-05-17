--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Directories;
with Ada.Direct_IO;
with Ada.Unchecked_Deallocation;

with Interfaces.C.Strings;

with GL.API;
with GL.Types;

package body GL.Files is
   use GL.Types;

   procedure Load_Shader_Source_From_File (Object : Objects.Shaders.Shader;
                                           File_Name : String) is
      procedure Free is new Ada.Unchecked_Deallocation
        (C.char_array, C.Strings.char_array_access);

      File_Size : constant Int := Int (Ada.Directories.Size (File_Name));

      -- File string *without* null termination
      subtype File_String is C.char_array (1 .. C.size_t (File_Size));

      package File_String_IO is new Ada.Direct_IO (File_String);

      File         : File_String_IO.File_Type;
      Raw_Contents : C.Strings.char_array_access
        := new C.char_array (1 .. C.size_t (File_Size + 1));
   begin
      File_String_IO.Open (File, Mode => File_String_IO.In_File,
                           Name => File_Name);
      File_String_IO.Read (File,
                           Item => Raw_Contents.all (1 .. C.size_t (File_Size)));
      File_String_IO.Close (File);
      Raw_Contents.all (C.size_t (File_Size + 1)) := C.nul;
      API.Shader_Source (Object.Raw_Id, 1,
                         (1 => Raw_Contents (1)'Unchecked_Access),
                         (1 => File_Size));
      Free (Raw_Contents);
      Raise_Exception_On_OpenGL_Error;
   end Load_Shader_Source_From_File;

end GL.Files;
