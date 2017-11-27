

with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

package body Load_Object_File is

   type Vertex_String is array (GL.Types.Int range <>, GL.Types.Int range <>) of
     Ada.Strings.Unbounded.Unbounded_String;
   type Vertex_Indices is array (Integer range <>, GL.Types.Int range <>) of
     GL.Types.Ints.Vector3;
   type Usemtl_Array is array (Integer range <>) of Ada.Strings.Unbounded.Unbounded_String;

   procedure Parse (Face_String : Ada.Strings.Unbounded.Unbounded_String;
                    S_Index  : GL.Types.Int;
                    V1_Indices, V2_Indices, V3_Indices : out Vertex_Indices);
    procedure Parse (UV_String : Ada.Strings.Unbounded.Unbounded_String;
                     UV : out GL.Types.Singles.Vector2);
    procedure Parse (Vertices_String : Ada.Strings.Unbounded.Unbounded_String;
                     Vertices : out GL.Types.Singles.Vector3);

   --  -------------------------------------------------------------------------

   procedure Get_Array_Sizes (File_Name  : String; Vertex_Count, UV_Count,
                              Normal_Count, Face_Count : out GL.Types.Int;
                              S_Count, Usemtl_Count : out Integer) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      File_ID  : Ada.Text_IO.File_Type;
      Text     : Unbounded_String;
      Label    : String (1 .. 2);
   begin
      Vertex_Count := 0;
      UV_Count := 0;
      Normal_Count := 0;
      S_Count := 0;
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
            when 's' => S_Count := S_Count + 1;
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
                        V1_Indices, V2_Indices, V3_Indices : in out Vertex_Indices;
                        Usemtl   : in out Usemtl_Array) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      Line         : Unbounded_String;
      Data         : Unbounded_String;
      Label        : String (1 .. 2);
      Vertex_Index : Int := 0;
      UV_Index     : Int := 0;
      Normal_Index : Int := 0;
      S_Index      : Int;
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
            when 's' =>  S_Index := Int'Value (To_String (Data));
            when 'u' => Usemtl_Index := Usemtl_Index + 1;
               Usemtl (Usemtl_Index) := Data;
            when 'f' => Face_Index := Face_Index + 1;
               Parse (Face_String, S_Index, V1_Indices, V2_Indices, V3_Indices);
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
      S_Count        : Integer;
      Usemtl_Count   : Integer;
      Face_Count     : GL.Types.Int;
   begin
      Get_Array_Sizes (File_Name, Num_Vertices, UV_Count, Normal_Count,
                       Face_Count, S_Count, Usemtl_Count);
      declare
--           Vertices  : GL.Types.Singles.Vector3_Array (1 .. Num_Vertices);
--           UVs       : GL.Types.Singles.Vector2_Array (1 .. UV_Count);
--           Normals   : GL.Types.Singles.Vector3_Array (1 .. Normal_Count);
         V1_Indices : Vertex_Indices (1 .. S_Count, 1 .. Face_Count);
         V2_Indices : Vertex_Indices (1 .. S_Count, 1 .. Face_Count);
         V3_Indices : Vertex_Indices (1 .. S_Count, 1 .. Face_Count);
         Usemtl    : Usemtl_Array (1 .. Usemtl_Count);
      begin
         Open (Text_File_ID, In_File, File_Name);
         Load_Data (Text_File_ID, Vertices, UVs, Normals,
                    V1_Indices, V2_Indices, V3_Indices, Usemtl);
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

   procedure Parse (Face_String : Ada.Strings.Unbounded.Unbounded_String;
                    S_Index  : GL.Types.Int;
                    V1_Indices, V2_Indices, V3_Indices : out Vertex_Indices) is
--        V1_String, V2_String, V3_String : Vertex_String;
   begin
      null;
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
      Ada.Float_Text_IO.Get (To_String (UV_String)(1 .. Size), Value, Last);
      UV (GL.Y) := GL.Types.Single (Value);
   end Parse;

   --  -------------------------------------------------------------------------

   procedure Parse (Vertices_String : Ada.Strings.Unbounded.Unbounded_String;
                    Vertices : out GL.Types.Singles.Vector3) is
   begin
      null;
   end Parse;

   --  -------------------------------------------------------------------------

end Load_Object_File;
