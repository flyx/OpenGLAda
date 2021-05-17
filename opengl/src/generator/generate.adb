--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

with Specs;

procedure Generate is
   Proc : Specs.Processor;

   package Spec_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, String);

   Spec_Paths : Spec_Vectors.Vector;
   package Path_Sorting is new Spec_Vectors.Generic_Sorting;

   Source_Folder    : constant String := "src/specs";
   Target_Folder    : constant String := "src/generated";
   Interface_Folder : constant String := "src/interface";

   procedure Process_File (Directory_Entry : in Directory_Entry_Type) is
      Path : constant String := Full_Name (Directory_Entry);
   begin
      Ada.Text_IO.Put_Line ("Processing " & Path & " ...");
      Spec_Paths.Append (Path);
      Ada.Text_IO.Put_Line ("Done processing " & Path & " .");
   end Process_File;

begin
   Search (Source_Folder, "*.spec", (Ordinary_File => True, others => False),
           Process_File'Access);
   Path_Sorting.Sort (Spec_Paths);
   for Path of Spec_Paths loop
      Specs.Parse_File (Proc, Path);
   end loop;

   Create_Path (Target_Folder);
   declare
      use type Specs.Spec;
      Cur : Specs.Spec := Specs.First (Proc);
   begin
      while Cur /= Specs.No_Spec loop
         Specs.Write_API (Proc, Cur, Target_Folder);
         Cur := Specs.Next (Proc, Cur);
      end loop;
   end;
   Specs.Write_Init (Proc, Target_Folder);
   Specs.Write_Wrapper_Table (Proc, Target_Folder, Interface_Folder);
exception when Error : Specs.Parsing_Error =>
   Ada.Text_IO.Put_Line (Exception_Message (Error));
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Generate;
