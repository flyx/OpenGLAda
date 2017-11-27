
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

package body Load_Object_File is

   type Face_Indices is array (1 .. 3) of GL.Types.Ints.Vector3;
   type Faces_Array is array (Integer range <>, GL.Types.Int range <>) of Face_Indices;
   type Usemtl_Array is array (Integer range <>) of Ada.Strings.Unbounded.Unbounded_String;

   procedure Parse (Face_String : Ada.Strings.Unbounded.Unbounded_String;
                    Face        : out Face_Indices);
    procedure Parse (UV_String : Ada.Strings.Unbounded.Unbounded_String;
                     UV : out GL.Types.Singles.Vector2);
    procedure Parse (Vertex_String : Ada.Strings.Unbounded.Unbounded_String;
                     Vertex : out GL.Types.Singles.Vector3);

   --  -------------------------------------------------------------------------

   procedure Get_Array_Sizes (File_Name  : String; Vertex_Count, UV_Count,
                              Normal_Count, Face_Count : out GL.Types.Int;
                              Mesh_Count, Usemtl_Count : out Integer) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      File_ID  : Ada.Text_IO.File_Type;
      Text     : Unbounded_String;
      Label    : String (1 .. 2);
   begin
      Vertex_Count := 0;
      UV_Count := 0;
      Normal_Count := 0;
      Mesh_Count := 0;
      Face_Count := 0;
      Usemtl_Count := 0;
      Open (File_ID, In_File, File_Name);
      while not End_Of_File (File_ID) loop
         Text := To_Unbounded_String (Get_Line (File_ID));
         Label := To_String (Text) (1 .. 2);
         case Label (1) is
            when 'v' =>
               case Label (2) is
                  when ' ' => Vertex_Count := Vertex_Count + 1;
                  when 't' => UV_Count := UV_Count + 1;
                  when 'n' => Normal_Count := Normal_Count + 1;
                  when others => null;
               end case;
            when 's' => Mesh_Count := Mesh_Count + 1;
            when 'u' => Usemtl_Count := Usemtl_Count + 1;
            when 'f' => Face_Count := Face_Count + 1;
            when others => null;
         end case;
      end loop;
      Close (File_ID);

   exception
      when Ada.IO_Exceptions.Name_Error  =>
         --  File not found
         Put_Line ("Get_Array_Sizes can't find the file " & File_Name & "!");
         raise;
      when others =>
         Put_Line ("An exception occurred in Get_Array_Sizes.");
         raise;
   end Get_Array_Sizes;

   --  -------------------------------------------------------------------------

   procedure Load_Data (File_ID  : Ada.Text_IO.File_Type;
                        Vertices : in out GL.Types.Singles.Vector3_Array;
                        UVs      : in out GL.Types.Singles.Vector2_Array;
                        Normals  : in out GL.Types.Singles.Vector3_Array;
                        Faces    : in out Faces_Array;
                        Usemtl   : in out Usemtl_Array) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      Line         : Unbounded_String;
      Data         : Unbounded_String;
      Label        : String (1 .. 2);
      Vertex_Index : Int := 0;
      UV_Index     : Int := 0;
      Normal_Index : Int := 0;
      Mesh_Index   : Integer;
      Face_String  : Unbounded_String;
      Usemtl_Index : Integer := 0;
      Face_Index   : Int := 0;
   begin
      while not End_Of_File (File_ID) loop
         Ada.Text_IO.Unbounded_IO.Get_Line (File_ID, Line);
         Label := To_String (Line) (1 .. 2);
         Data := Unbounded_Slice (Line, 2, To_String (Line)'Length);
         case Label (1) is
            when 'v' =>
               case Label (2) is
                  when ' ' => Vertex_Index := Vertex_Index + 1;
                     Parse (Line, Vertices (Vertex_Index));
                  when 't' =>  UV_Index := UV_Index + 1;
                     Parse (Line, UVs (UV_Index));
                  when 'n' => Normal_Index := Normal_Index + 1;
                     Parse (Line, Normals (Normal_Index));
                  when others => null;
               end case;
            when 's' =>  Mesh_Index := Integer'Value (To_String (Data));
            when 'u' => Usemtl_Index := Usemtl_Index + 1;
               Usemtl (Usemtl_Index) := Data;
            when 'f' => Face_Index := Face_Index + 1;
               Parse (Face_String, Faces (Mesh_Index, Face_Index));
            when others => null;
         end case;
      end loop;
   end Load_Data;

   --  -------------------------------------------------------------------------

   procedure Load_Object (File_Name  : String;
                          Vertices : out GL.Types.Singles.Vector3_Array;
                          UVs      : out GL.Types.Singles.Vector2_Array;
                          Normals  : out GL.Types.Singles.Vector3_Array) is
      Text_File_ID   : Ada.Text_IO.File_Type;
      Num_Vertices   : GL.Types.Int;
      UV_Count       : GL.Types.Int;
      Normal_Count   : GL.Types.Int;
      Mesh_Count     : Integer;
      Usemtl_Count   : Integer;
      Face_Count     : GL.Types.Int;
   begin
      Get_Array_Sizes (File_Name, Num_Vertices, UV_Count, Normal_Count,
                       Face_Count, Mesh_Count, Usemtl_Count);
      declare
         Faces    : Faces_Array (1 .. Mesh_Count, 1 .. Face_Count);
         Usemtl   : Usemtl_Array (1 .. Usemtl_Count);
      begin
         Open (Text_File_ID, In_File, File_Name);
         Load_Data (Text_File_ID, Vertices, UVs, Normals, Faces, Usemtl);
         Close (Text_File_ID);
      end;

   exception
      when Ada.IO_Exceptions.Name_Error  =>
         --  File not found
         Put_Line ("Load_Object can't find the file " & File_Name & "!");
         raise;
      when others =>
         Put_Line ("An exception occurred in Load_Object.");
         raise;
   end Load_Object;

   --  -------------------------------------------------------------------------

   procedure Read_Vertex_Indices (Face_String : Ada.Strings.Unbounded.Unbounded_String;
                    String_Index : in out Positive;
                    Indices : in out GL.Types.Ints.Vector3) is
        use Ada.Strings.Unbounded;
        Size      : constant Natural := Length (Face_String);

        procedure Read_Index (Face_String  : Unbounded_String;
                              GL_Value     : out GL.Types.Int) is
            Value : Integer;
        begin
            Ada.Integer_Text_IO.Get (To_String (Face_String)
                                     (String_Index .. Size), Value, String_Index);
            GL_Value := GL.Types.Int (Value);
            String_Index := String_Index + 2; --  skip /
        end Read_Index;

   begin
       Read_Index (Face_String, Indices (GL.X));
       Read_Index (Face_String, Indices (GL.Y));
       Read_Index (Face_String, Indices (GL.Z));
   end Read_Vertex_Indices;

   --  -------------------------------------------------------------------------

--    type Face_Indices is array (1 .. 3) of GL.Types.Ints.Vector3;
   procedure Parse (Face_String : Ada.Strings.Unbounded.Unbounded_String;
                    Face : out Face_Indices) is
        use Ada.Strings.Unbounded;
        Text_Index : Positive := 1;
   begin
      Read_Vertex_Indices (Face_String, Text_Index, Face (1));
      Read_Vertex_Indices (Face_String, Text_Index, Face (2));
      Read_Vertex_Indices (Face_String, Text_Index, Face (3));
   end Parse;

   --  -------------------------------------------------------------------------

   procedure Parse (UV_String : Ada.Strings.Unbounded.Unbounded_String;
                    UV : out GL.Types.Singles.Vector2) is
        use Ada.Strings.Unbounded;
        Last     : Natural;
        Size     : constant Natural := Length (UV_String);
        Value    : Float;
   begin
      Ada.Float_Text_IO.Get (To_String (UV_String) (1 .. Size), Value, Last);
      UV (GL.X) := GL.Types.Single (Value);
      Ada.Float_Text_IO.Get (To_String (UV_String)(Last .. Size), Value, Last);
      UV (GL.Y) := GL.Types.Single (Value);
   end Parse;

   --  -------------------------------------------------------------------------

   procedure Parse (Vertex_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex : out GL.Types.Singles.Vector3) is
        use Ada.Strings.Unbounded;
        Last     : Natural;
        Size     : constant Natural := Length (Vertex_String);
        Value    : Float;
   begin

        Ada.Float_Text_IO.Get (To_String (Vertex_String)(1 .. Size), Value, Last);
        Vertex (GL.X) := GL.Types.Single (Value);
        Ada.Float_Text_IO.Get (To_String (Vertex_String)(Last .. Size), Value, Last);
        Vertex (GL.Y) := GL.Types.Single (Value);
        Ada.Float_Text_IO.Get (To_String (Vertex_String)(Last .. Size), Value, Last);
        Vertex (GL.Z) := GL.Types.Single (Value);
   end Parse;

   --  -------------------------------------------------------------------------

end Load_Object_File;
