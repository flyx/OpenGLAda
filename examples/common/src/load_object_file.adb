
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

package body Load_Object_File is

--     type Usemtl_Array is array (Integer range <>) of Ada.Strings.Unbounded.Unbounded_String;

   procedure Parse (Face_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex_Index, UV_Index, Normal_Index : out GL.Types.Ints.Vector3);
   procedure Parse (UV_String : Ada.Strings.Unbounded.Unbounded_String;
                    UV : out GL.Types.Singles.Vector2; DDS_Format : Boolean := True);
   procedure Parse (Vertex_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex : out GL.Types.Singles.Vector3);
   procedure Read_Index (Data : Ada.Strings.Unbounded.Unbounded_String;
                         Start : in out Positive; Index : out GL.Types.Int);

   --  -------------------------------------------------------------------------

   procedure Data_From_Faces (Raw_Vertices : GL.Types.Singles.Vector3_Array;
                              Raw_UVs      : GL.Types.Singles.Vector2_Array;
                              Raw_Normals  : GL.Types.Singles.Vector3_Array;
                              Vertex_Indices, UV_Indices, Normal_Indices :
                                             GL.Types.Ints.Vector3_Array;
                              Vertices : out GL.Types.Singles.Vector3_Array;
                              UVs      : out GL.Types.Singles.Vector2_Array;
                              Normals  : out GL.Types.Singles.Vector3_Array) is
      use GL;
      --  The three elements of a Vertex_Index refer to the three vertices
      --  of a triangle
   begin
      for Index in Vertex_Indices'Range loop
         Put_Line ("Data_From_Faces Index: " & Types.Int'Image (Index));
         for elem in GL.Index_3D'Range loop
            Put_Line ("Data_From_Faces Vertex elem: " & Index_3D'Image (elem) &
               " " & GL.Types.Int'Image (Vertex_Indices (Index) (elem)));
            Vertices (Index) := Raw_Vertices (Vertex_Indices (Index) (elem));
            Put_Line ("Data_From_Faces UVs elem: " & Index_3D'Image (elem) &
               " " & GL.Types.Int'Image (UV_Indices (Index) (elem)));
               UVs (Index) := Raw_UVs (UV_Indices (Index) (elem));
            Normals (Index) := Raw_Normals (Normal_Indices (Index) (elem));
            Put_Line ("Data_From_Faces Normals elem: " & Index_3D'Image (elem) &
               " " & GL.Types.Int'Image (Normal_Indices (Index) (elem)));
         end loop;
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Data_From_Faces.");
         raise;
   end Data_From_Faces;

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

   procedure Get_Array_Sizes (File_Name : String; Vertex_Count, UV_Count,
                              Normal_Count : out GL.Types.Int) is
      Face_Count   : GL.Types.Int;
      Mesh_Count   : Integer;
      Usemtl_Count : Integer;
   begin
      Get_Array_Sizes (File_Name, Vertex_Count, UV_Count, Normal_Count,
                       Face_Count, Mesh_Count, Usemtl_Count);
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
      Line         : Unbounded_String;
      Data         : Unbounded_String;
      Label        : String (1 .. 2);
      Vertex_Index : Int := 0;
      UV_Index     : Int := 0;
      Normal_Index : Int := 0;
--        Mesh_Index   : Integer;
--        Usemtl_Index : Integer := 0;
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
--                 Mesh_Index := Integer'Value (To_String (Data));
            when 'u' => null;
--                 Usemtl_Index := Usemtl_Index + 1;
--                 Usemtl (Usemtl_Index) := Data;
            when 'f' =>  Face_Index := Face_Index + 1;
               Parse (Data, Vertex_Indicies (Face_Index),
                      UV_Indicies (Face_Index), Normal_Indicies (Face_Index));
            when others => null;
         end case;
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_Data.");
         raise;
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
         use GL.Types;
         Raw_Vertices   : Singles.Vector3_Array (1 .. Num_Vertices);
         Raw_UVs        : Singles.Vector2_Array (1 .. UV_Count);
         Raw_Normals    : Singles.Vector3_Array (1 .. Normal_Count);
         Vertex_Indices : Ints.Vector3_Array (1 .. 3 * Face_Count);
         UV_Indices     : Ints.Vector3_Array (1 .. 3 * Face_Count);
         Normal_Indices : Ints.Vector3_Array (1 .. 3 * Face_Count);
      begin
         Open (Text_File_ID, In_File, File_Name);
         Load_Data (Text_File_ID, Raw_Vertices, Raw_UVs, Raw_Normals,
                    Vertex_Indices, UV_Indices, Normal_Indices);
         Close (Text_File_ID);
         Data_From_Faces (Raw_Vertices, Raw_UVs, Raw_Normals,
                          Vertex_Indices, UV_Indices, Normal_Indices,
                          Vertices, UVs, Normals);
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

   procedure Parse (Face_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex_Index, UV_Index, Normal_Index  : out GL.Types.Ints.Vector3) is
      use Ada.Strings.Unbounded;
      Start : Positive := 1;
   begin
      for indice in GL.Index_3D loop
         Read_Index (Face_String, Start, Vertex_Index (indice));
      end loop;
     for indice in GL.Index_3D loop
         Read_Index (Face_String, Start, UV_Index (indice));
     end loop;
     for indice in GL.Index_3D loop
         Read_Index (Face_String, Start, Normal_Index (indice));
     end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Parse Face_String.");
         raise;
   end Parse;

   --  -------------------------------------------------------------------------

   procedure Parse (UV_String : Ada.Strings.Unbounded.Unbounded_String;
                    UV : out GL.Types.Singles.Vector2; DDS_Format : Boolean  := True) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      Last     : Natural;
      Size     : constant Natural := Length (UV_String);
      Value    : Float;
   begin
      Ada.Float_Text_IO.Get (To_String (UV_String) (1 .. Size), Value, Last);
      UV (GL.X) := Single (Value);
      Ada.Float_Text_IO.Get (To_String (UV_String)(Last .. Size), Value, Last);
      UV (GL.Y) := Single (Value);
      --  Invert V coordinate since we will only use DDS texture which are inverted.
      --  Remove if you want to use TGA or BMP loaders.
      if DDS_Format then
         UV (GL.Y) := -UV (GL.Y);
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Parse UV_String.");
         raise;
   end Parse;

   --  -------------------------------------------------------------------------

   procedure Parse (Vertex_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertex : out GL.Types.Singles.Vector3) is
        use Ada.Strings.Unbounded;
        Last     : Natural := 1;
        Size     : constant Natural := Length (Vertex_String);
        Value    : float;
   begin
        Ada.Float_Text_IO.Get (To_String (Vertex_String)(Last .. Size), Value, Last);
        Vertex (GL.X) := GL.Types.Single (Value);
        Ada.Float_Text_IO.Get (To_String (Vertex_String)(Last .. Size), Value, Last);
        Vertex (GL.Y) := GL.Types.Single (Value);
        Ada.Float_Text_IO.Get (To_String (Vertex_String)(Last .. Size), Value, Last);
      Vertex (GL.Z) := GL.Types.Single (Value);

   exception
      when others =>
         Put_Line ("An exception occurred in Parse Vertex_String.");
         raise;
   end Parse;

   --  -------------------------------------------------------------------------

   Procedure Read_Index (Data : Ada.Strings.Unbounded.Unbounded_String;
                         Start : in out Positive; Index : out GL.Types.Int) is
      use Ada.Strings.Unbounded;
      Size : constant Natural := Length (Data);
      Pos  : Positive := Start;
      Last_Pos  : Positive;
      Value : Integer;
   begin
      if Element (Data, Start) = '/' then
         Pos := Pos + 1;
      end if;

      Ada.Integer_Text_IO.Get (To_String (Data)(Pos .. Size), Value, Last_Pos);
      Index := GL.Types.Int (Value);
      Start := Last_Pos + 1;

   exception
      when others =>
         Put_Line ("An exception occurred in Read_Index.");
         raise;
   end Read_Index;

   --  -------------------------------------------------------------------------

end Load_Object_File;
