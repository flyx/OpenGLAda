

with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;

with GL.Types;
--  with GL.Objects.Textures.Targets;
--  with GL.Pixels;

package body Load_Object_File is

   type V_String is array (GL.Types.Int range <>, GL.Types.Int range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

    procedure Parse (F_String : Ada.Strings.Unbounded.Unbounded_String;
                     V1_String, V2_String, V3_String : out V_String);

   --  -------------------------------------------------------------------------

   procedure Get_Num_Vertices (File_ID : Ada.Text_IO.File_Type;
                               V_Count, VT_Count, VN_Count, S_Count,
                               Usemtl_Count, F_Count : out GL.Types.Int) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      Text  : Unbounded_String;
      Label : String (1 .. 2);
   begin
      V_Count := 0;
      VT_Count := 0;
      VN_Count := 0;
      S_Count := 0;
      Usemtl_Count := 0;
      F_Count := 0;
      while not End_Of_File (File_ID) loop
         Text := To_Unbounded_String (Get_Line (File_ID));
         Label := To_String (Text) (1 .. 2);
         case Label (1) is
            when 'v' =>
               case Label (2) is
                  when ' ' => V_Count := V_Count + 1;
                  when 't' => VT_Count := VT_Count + 1;
                  when 'n' => VN_Count := VN_Count + 1;
                  when others => null;
               end case;
            when 's' => S_Count := S_Count + 1;
            when 'u' => Usemtl_Count := Usemtl_Count + 1;
            when 'f' => F_Count := F_Count + 1;
            when others => null;
         end case;
      end loop;
   end Get_Num_Vertices;

   --  -------------------------------------------------------------------------

   procedure Load_Data (File_ID  : Ada.Text_IO.File_Type;
                        Vertices : in out GL.Types.Singles.Vector3_Array;
                        Verts    : in out GL.Types.Singles.Vector2_Array;
                        Normals  : in out GL.Types.Singles.Vector3_Array;
                        V1_String, V2_String, V3_String : out V_String) is
      use Ada.Strings.Unbounded;
      use GL.Types;
      Data_Stream  : constant Stream_Access := Stream (File_ID);
      Label        : String (1 .. 2);
      V_Index      : Int := 0;
      VT_Index     : Int := 0;
      VN_Index     : Int := 0;
      S_Index      : Int := 1;
      F_String     : Unbounded_String;
--        Usemtl_Index : Int := 0;
      F_Index      : Int := 0;
   begin
      while not End_Of_File (File_ID) loop
         String'Read (Data_Stream, Label);
         case Label (1) is
            when 'v' =>
               case Label (2) is
                  when ' ' => V_Index := V_Index + 1;
                     Singles.Vector3'Read (Data_Stream, Vertices (V_Index));
                  when 't' =>  VT_Index := VT_Index + 1;
                     Singles.Vector2'Read (Data_Stream, Verts (VT_Index));
                  when 'n' => VN_Index := VN_Index + 1;
                     Singles.Vector3'Read (Data_Stream, Normals (VN_Index));
                  when others => null;
               end case;
            when 's' => Int'Read (Data_Stream, S_Index);
--              when 'u' => Usemtl_Count := Usemtl_Count + 1;
            when 'f' => F_Index := F_Index + 1;
               Unbounded_String'Read (Data_Stream, F_String);
               Parse (F_String, V1_String, V2_String, V3_String);
            when others => null;
         end case;
      end loop;
   end Load_Data;

   --  -------------------------------------------------------------------------

   procedure Load_Object (File_Name  : String;
                          theTexture : out GL.Objects.Textures.Texture) is
      Text_File_ID   : Ada.Text_IO.File_Type;
--        Stream_File_ID : Ada.Text_IO.Text_Streams.File_Type;
      Num_Vertices   : GL.Types.Int;
      VT_Count       : GL.Types.Int;
      VN_Count       : GL.Types.Int;
      S_Count        : GL.Types.Int;
      Usemtl_Count   : GL.Types.Int;
      F_Count        : GL.Types.Int;
   begin
      Open (Text_File_ID, In_File, File_Name);
      Get_Num_Vertices (Text_File_ID, Num_Vertices, VT_Count, VN_Count, S_Count,
                        Usemtl_Count, F_Count);
      declare
         Vertices  : GL.Types.Singles.Vector3_Array (1 .. Num_Vertices);
         Verts     : GL.Types.Singles.Vector2_Array (1 .. VT_Count);
         Normals   : GL.Types.Singles.Vector3_Array (1 .. VN_Count);
         V1_String : V_String (1 .. S_Count, 1 .. F_Count);
         V2_String : V_String (1 .. S_Count, 1 .. F_Count);
         V3_String : V_String (1 .. S_Count, 1 .. F_Count);
      begin
         Reset (Text_File_ID);
         Load_Data (Text_File_ID, Vertices, Verts, Normals,
                    V1_String, V2_String, V3_String);
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

   procedure Parse (F_String : Ada.Strings.Unbounded.Unbounded_String;
                    V1_String, V2_String, V3_String : out V_String) is
   begin
      null;
   end Parse;

   --  -------------------------------------------------------------------------

end Load_Object_File;
