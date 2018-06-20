
with Ada.IO_Exceptions;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

package body Load_Object_File is

   procedure Parse (Mesh_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex_Index, UV_Index, Normal_Index : out GL.Types.Ints.Vector3);
   procedure Parse (UV_String : Ada.Strings.Unbounded.Unbounded_String;
                    UV : out GL.Types.Singles.Vector2; DDS_Format : Boolean := True);
   procedure Parse (Vertex_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex : out GL.Types.Singles.Vector3);
   procedure Read_Index (Data : Ada.Strings.Unbounded.Unbounded_String;
                         Start : in out Positive; Index : out GL.Types.Int);

   --  -------------------------------------------------------------------------

   procedure Data_From_Faces (Raw_Vertices  : GL.Types.Singles.Vector3_Array;
                              Raw_UVs       : GL.Types.Singles.Vector2_Array;
                              Raw_Normals   : GL.Types.Singles.Vector3_Array;
                              Vertex_Indices, UV_Indices, Normal_Indices :
                                             GL.Types.Ints.Vector3_Array;
                              Mesh_Vertices : out GL.Types.Singles.Vector3_Array;
                              Mesh_UVs      : out GL.Types.Singles.Vector2_Array;
                              Mesh_Normals  : out GL.Types.Singles.Vector3_Array) is
      use GL;
      use GL.Types;
      --  The three elements of a Vertex_Index refer to the three vertices
      --  of a triangle
      Raw_Vertex_Indices  : Ints.Vector3;
      Raw_UVs_Indices     : Ints.Vector3;
      Raw_Normals_Indices : Ints.Vector3;
      Raw_Vertex_Index    : Int;
      Raw_UVs_Index       : Int;
      Raw_Normal_Index    : Int;
      Mesh_Index          : Int := 0;
   begin
      for Index in UV_Indices'Range loop
         -- Get vector of three indices, one for each vertex of a triangle
         Raw_Vertex_Indices :=  Vertex_Indices (Index);
         Raw_UVs_Indices :=  UV_Indices (Index);
         Raw_Normals_Indices :=  Normal_Indices (Index);
         for elem in Index_3D'Range loop
            Mesh_Index := Mesh_Index + 1;
            --  for each vertex of a triangle, get the vertex index
            Raw_Vertex_Index := Raw_Vertex_Indices (elem);
            Raw_UVs_Index := Raw_UVs_Indices (elem);
            Raw_Normal_Index := Raw_Normals_Indices (elem);
            -- for each vertex of a triangle, get the vertex components (x, y, z)
            Mesh_Vertices (Mesh_Index) := Raw_Vertices (Raw_Vertex_Index);
            Mesh_UVs (Mesh_Index) := Raw_UVs (Raw_UVs_Index);
            Mesh_Normals (Mesh_Index) := Raw_Normals (Raw_Normal_Index);
         end loop;
      end loop;
   end Data_From_Faces;

   --  -------------------------------------------------------------------------

   procedure Get_Array_Sizes (File_Name  : String; Vertex_Count, UV_Count,
                              Normal_Count, Indices_Count : out GL.Types.Int;
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
      Indices_Count := 0;
      Mesh_Count := 0;
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
            when 's' => Mesh_Count := Mesh_Count + 3;
            when 'u' => Usemtl_Count := Usemtl_Count + 1;
            when 'f' => Indices_Count := Indices_Count + 1;
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
                        Vertex_Indicies, UV_Indicies, Normal_Indicies :
                                   in out GL.Types.Ints.Vector3_Array) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      Line               : Unbounded_String;
      Data               : Unbounded_String;
      Label              : String (1 .. 2);
      Vertex_Index       : Int := 0;
      UV_Index           : Int := 0;
      Normal_Index       : Int := 0;
      Mesh_Vertice_Index : Int := 0;
   begin
      while not End_Of_File (File_ID) loop
         Ada.Text_IO.Unbounded_IO.Get_Line (File_ID, Line);
         Label := To_String (Line) (1 .. 2);
         Data := Unbounded_Slice (Line, 2, To_String (Line)'Length);
         case Label (1) is
            when 'v' =>
               case Label (2) is
                  when ' ' => Vertex_Index := Vertex_Index + 1;
                     Parse (Data, Vertices (Vertex_Index));
                  when 't' =>  UV_Index := UV_Index + 1;
                     Data := Unbounded_Slice (Line, 3, To_String (Line)'Length);
                     Parse (Data, UVs (UV_Index));
                  when 'n' => Normal_Index := Normal_Index + 1;
                     Data := Unbounded_Slice (Line, 3, To_String (Line)'Length);
                     Parse (Data, Normals (Normal_Index));
                  when others => null;
               end case;
            when 's' => null;
            when 'u' => null;
            when 'f' =>  Mesh_Vertice_Index := Mesh_Vertice_Index + 1;
               Parse (Data, Vertex_Indicies (Mesh_Vertice_Index),
                      UV_Indicies (Mesh_Vertice_Index),
                      Normal_Indicies (Mesh_Vertice_Index));
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
      Vertex_Count   : GL.Types.Int;
   begin
      Get_Array_Sizes (File_Name, Num_Vertices, UV_Count, Normal_Count,
                       Vertex_Count, Mesh_Count, Usemtl_Count);
      declare
         use GL.Types;
         Raw_Vertices   : Singles.Vector3_Array (1 .. Num_Vertices);
         Raw_UVs        : Singles.Vector2_Array (1 .. UV_Count);
         Raw_Normals    : Singles.Vector3_Array (1 .. Normal_Count);
         Vertex_Indices : Ints.Vector3_Array (1 .. Vertex_Count);
         UV_Indices     : Ints.Vector3_Array (1 .. Vertex_Count);
         Normal_Indices : Ints.Vector3_Array (1 .. Vertex_Count);
      begin
         Open (Text_File_ID, In_File, File_Name);
         Load_Data (Text_File_ID, Raw_Vertices, Raw_UVs, Raw_Normals,
                    Vertex_Indices, UV_Indices, Normal_Indices);
         Close (Text_File_ID);

         if Vertex_Count > 0 then
            Data_From_Faces (Raw_Vertices, Raw_UVs, Raw_Normals,
                             Vertex_Indices, UV_Indices, Normal_Indices,
                             Vertices, UVs, Normals);
         end if;
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

   procedure Load_Object (File_Name  : String;
                          Vertices : out GL.Types.Singles.Vector3_Array;
                          UVs      : out GL.Types.Singles.Vector2_Array) is
   begin
      declare
         Normals : GL.Types.Singles.Vector3_Array (1 .. Mesh_Size (File_Name));
      begin
         Load_Object (File_Name, Vertices, UVs, Normals);
      end;
   end Load_Object;

   --  -------------------------------------------------------------------------

   function Mesh_Size (File_Name  : String) return GL.Types.Int is
      use Ada.Strings.Unbounded;
      use GL.Types;
      File_ID           : Ada.Text_IO.File_Type;
      Mesh_Vertex_Count : Int := 0;
      Text              : Unbounded_String;
      Label             : String (1 .. 2);
   begin
      Open (File_ID, In_File, File_Name);
      while not End_Of_File (File_ID) loop
         Text := To_Unbounded_String (Get_Line (File_ID));
         Label := To_String (Text) (1 .. 2);
         if Label (1) = 'f' then
            Mesh_Vertex_Count := Mesh_Vertex_Count + 3;
         end if;
      end loop;
      Close (File_ID);
      return Mesh_Vertex_Count;

   exception
      when Ada.IO_Exceptions.Name_Error  =>
         --  File not found
         Put_Line ("Mesh_Size can't find the file " & File_Name & "!");
         raise;
      when others =>
         Put_Line ("An exception occurred in Mesh_Size.");
         raise;
   end Mesh_Size;

    --  -------------------------------------------------------------------------

   procedure Parse (Mesh_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex_Index, UV_Index, Normal_Index  : out GL.Types.Ints.Vector3) is
      Start : Positive := 1;
   begin
      for indice in GL.Index_3D loop
         Read_Index (Mesh_String, Start, Vertex_Index (indice));
         Read_Index (Mesh_String, Start, UV_Index (indice));
         Read_Index (Mesh_String, Start, Normal_Index (indice));
      end loop;
   end Parse;

   --  -------------------------------------------------------------------------

   procedure Parse (UV_String : Ada.Strings.Unbounded.Unbounded_String;
                    UV : out GL.Types.Singles.Vector2; DDS_Format : Boolean  := True) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      Next     : Natural := 1;
      Size     : constant Natural := Length (UV_String);
      Value    : Float;
   begin
      Ada.Float_Text_IO.Get (To_String (UV_String) (Next .. Size), Value, Next);
      UV (GL.X) := Single (Value);
      Next := Next + 1;
      Ada.Float_Text_IO.Get (To_String (UV_String)(Next .. Size), Value, Next);
      UV (GL.Y) := Single (Value);
      --  Invert V coordinate since we will only use DDS texture which are inverted.
      --  Remove if you want to use TGA or BMP loaders.
      if DDS_Format then
         UV (GL.Y) := -UV (GL.Y);
      end if;
   end Parse;

   --  -------------------------------------------------------------------------

   procedure Parse (Vertex_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex        : out GL.Types.Singles.Vector3) is
      use Ada.Strings.Unbounded;
      Next     : Natural := 1;
      Size     : constant Natural := Length (Vertex_String);
      Value    : float;
   begin
      Ada.Float_Text_IO.Get (To_String (Vertex_String) (Next .. Size), Value, Next);
      Vertex (GL.X) := GL.Types.Single (Value);
      Next := Next + 1;
      Ada.Float_Text_IO.Get (To_String (Vertex_String) (Next .. Size), Value, Next);
      Next := Next + 1;
      Vertex (GL.Y) := GL.Types.Single (Value);
      Ada.Float_Text_IO.Get (To_String (Vertex_String) (Next .. Size), Value, Next);
      Vertex (GL.Z) := GL.Types.Single (Value);
   end Parse;

   --  -------------------------------------------------------------------------

   Procedure Read_Index (Data : Ada.Strings.Unbounded.Unbounded_String;
                         Start : in out Positive; Index : out GL.Types.Int) is
      use Ada.Strings.Unbounded;
      Size     : constant Natural := Length (Data);
      Pos      : Positive := Start;
      Last_Pos : Positive;
      Value    : Integer;
   begin
      if Element (Data, Start) = '/' then
         Pos := Pos + 1;
      end if;

      Ada.Integer_Text_IO.Get (To_String (Data)(Pos .. Size), Value, Last_Pos);
      Index := GL.Types.Int (Value);
      Start := Last_Pos + 1;
   end Read_Index;

   --  -------------------------------------------------------------------------

end Load_Object_File;
