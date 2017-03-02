with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

with Specs;

procedure Generate is
   Proc : Specs.Processor;

   Source_Folder    : constant String := "src/gl/specs";
   Target_Folder    : constant String := "src/gl/generated";
   Interface_Folder : constant String := "src/gl/interface";

   procedure Process_File (Directory_Entry : in Directory_Entry_Type) is
      Path : constant String := Full_Name (Directory_Entry);
   begin
      Ada.Text_IO.Put_Line ("Processing " & Path & " ...");
      Specs.Parse_File (Proc, Path);
      Ada.Text_IO.Put_Line ("Done processing " & Path & " .");
   end Process_File;

begin
   Search (Source_Folder, "*.spec", (Ordinary_File => True, others => False),
     Process_File'Access);
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