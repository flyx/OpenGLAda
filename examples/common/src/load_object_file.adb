
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

package body Load_Object_File is

   type Vertex_ID is (Vertex_1, Vertex_2, Vertex_3);
   type Indices is array (Vertex_ID) of GL.Types.Ints.Vector3;
--     type Faces_Array is array (Integer range <>, GL.Types.Int range <>) of Face_Indices;
   type Usemtl_Array is array (Integer range <>) of Ada.Strings.Unbounded.Unbounded_String;

   procedure Parse (Face_String : Ada.Strings.Unbounded.Unbounded_String;
                    V_Index : out GL.Types.Ints.Vector3);
    procedure Parse (UV_String : Ada.Strings.Unbounded.Unbounded_String;
                     UV : out GL.Types.Singles.Vector2);
    procedure Parse (Vertex_String : Ada.Strings.Unbounded.Unbounded_String;
                     Vertex : out GL.Types.Singles.Vector3);

   --  -------------------------------------------------------------------------

   procedure Data_From_Faces (Raw_Vertices : GL.Types.Singles.Vector3_Array;
                              Raw_UVs      : GL.Types.Singles.Vector2_Array;
                              Raw_Normals  : GL.Types.Singles.Vector3_Array;
                              Vertex_Indices, UV_Indices, Normal_Indices :
                                             GL.Types.Ints.Vector3_Array;
                              Vertices : out GL.Types.Singles.Vector3_Array;
                              UVs      : out GL.Types.Singles.Vector2_Array;
                              Normals  : out GL.Types.Singles.Vector3_Array) is
      Normal_Index   : Positive;
      UV_Index       : Positive;
      Vertex_Index   : Positive;
   begin
      for Vert in Vertices'Range loop
         Vertices (Vert) := Raw_Vertices (Vertex_Indices (Vert));
      end loop;
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
         Face_Count := Face_Count / 3;
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
                        Vertex_Indicies, UV_Indicies, Normal_Indicies : in out GL.Types.Ints.Vector3_Array;
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
      Indices_Index : Int := 0;
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
            when 'f' =>
               case Integer (Face_Index mod 3) is
                  when 0 =>
                     Indices_Index := Indices_Index + 1;
                     Parse (Face_String, Vertex_Indicies (Indices_Index));
                  when 1 =>
                     Parse (Face_String, UV_Indicies (Indices_Index));
                  when 2 =>
                     Parse (Face_String, Normal_Indicies (Indices_Index));
                  when others => null;
               end case;
               Face_Index := Face_Index + 1;
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
         Raw_Vertices   : GL.Types.Singles.Vector3_Array (1 .. Num_Vertices);
         Raw_UVs        : GL.Types.Singles.Vector2_Array (1 .. UV_Count);
         Raw_Normals    : GL.Types.Singles.Vector3_Array (1 .. Normal_Count);
         Vertex_Indices : GL.Types.Ints.Vector3_Array (1 .. Face_Count);
         UV_Indices     : GL.Types.Ints.Vector3_Array (1 .. Face_Count);
         Normal_Indices : GL.Types.Ints.Vector3_Array (1 .. Face_Count);
         Usemtl         : Usemtl_Array (1 .. Usemtl_Count);
         Mesh_Index     : constant Integer := 1;
      begin
         Open (Text_File_ID, In_File, File_Name);
         Load_Data (Text_File_ID, Raw_Vertices, Raw_UVs, Raw_Normals,
                    Vertex_Indices, UV_Indices, Normal_Indices, Usemtl);
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

   procedure Read_Vertex_Indices (Face_String : Ada.Strings.Unbounded.Unbounded_String;
                    String_Index : in out Positive;
                    Vertex_Indices : in out GL.Types.Ints.Vector3) is
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
       Read_Index (Face_String, Vertex_Indices (GL.X));
       Read_Index (Face_String, Vertex_Indices (GL.Y));
       Read_Index (Face_String, Vertex_Indices (GL.Z));
   end Read_Vertex_Indices;

   --  -------------------------------------------------------------------------

   procedure Parse (Face_String : Ada.Strings.Unbounded.Unbounded_String;
                    V_Index : out GL.Types.Ints.Vector3) is
      use Ada.Strings.Unbounded;
      Last : Positive := 1;
      Size : constant Natural := Length (Face_String);

      function Read_Index return GL.Types.Int is
         Value : Integer;
      begin
         Ada.Integer_Text_IO.Get (To_String (Face_String)(Last .. Size), Value, Last);
         return GL.Types.Int (Value);
      end Read_Index;

   begin
     for indice in GL.Index_3D loop
         V_Index (indice) := Read_Index;
     end loop;
   end Parse;

   --  -------------------------------------------------------------------------

   procedure Parse (UV_String : Ada.Strings.Unbounded.Unbounded_String;
                    UV : out GL.Types.Singles.Vector2) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      Last     : Natural;
      Size     : constant Natural := Length (UV_String);
      Value    : Float;
   begin
      Ada.Float_Text_IO.Get (To_String (UV_String) (1 .. Size), Value, Last);
      UV (GL.X) := Single (Value);
      Ada.Float_Text_IO.Get (To_String (UV_String)(Last .. Size), Value, Last);
      --  Invert V coordinate since we will only use DDS texture which are inverted.
      --  Remove if you want to use TGA or BMP loaders.
      UV (GL.Y) := -Single (Value);
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
   end Parse;

   --  -------------------------------------------------------------------------

end Load_Object_File;
