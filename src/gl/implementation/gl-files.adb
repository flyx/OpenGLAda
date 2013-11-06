--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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

with Ada.Directories;
with Ada.Direct_IO;

with Interfaces.C.Strings;

with GL.API;
with GL.Low_Level;
with GL.Types;

package body GL.Files is
   use GL.Types;
   
   procedure Read_Whole_File (File_Name : String;
                              File_Size : out Int;
                              Contents  : out C.Strings.chars_ptr) is
   begin
      File_Size := Int (Ada.Directories.Size (File_Name));
      declare
         -- File string *without* null termination
         subtype File_String is C.char_array (1 .. C.size_t (File_Size));
         
         package File_String_IO is new Ada.Direct_IO (File_String);
         
         File         : File_String_IO.File_Type;
         Raw_Contents : constant C.Strings.char_array_access
           := new C.char_array (1 .. C.size_t (File_Size + 1));
      begin
         File_String_IO.Open (File, Mode => File_String_IO.In_File,
                                    Name => File_Name);
         File_String_IO.Read (File,
                              Item => Raw_Contents.all (1 .. C.size_t (File_Size)));
         File_String_IO.Close (File);
         
         Raw_Contents.all (C.size_t (File_Size + 1)) := C.nul;
         Contents := C.Strings.To_Chars_Ptr (Raw_Contents, False);
      end;
   end Read_Whole_File;

   procedure Load_Shader_Source_From_File (Object : Objects.Shaders.Shader;
                                           File_Name : String) is
      Sources : Low_Level.CharPtr_Array (1 .. 1);
      Sizes   : Low_Level.Int_Array (1 .. 1);
     
   begin
      Read_Whole_File (File_Name, Sizes (1), Sources (1));
      API.Shader_Source (Object.Raw_Id, 1, Sources, Sizes);
      Raise_Exception_On_OpenGL_Error;
   end Load_Shader_Source_From_File;

end GL.Files;
