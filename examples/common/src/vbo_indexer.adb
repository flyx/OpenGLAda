
with Ada.Text_IO; use Ada.Text_IO;

package body VBO_Indexer is

   function Is_Near (V1, V2 : GL.Types.Single) return Boolean;

   --  -------------------------------------------------------------------------

   procedure Get_Similar_Vertex_Index
     (Vertices_In    : GL.Types.Singles.Vector3;
      UVs_In         : GL.Types.Singles.Vector2;
      Normals_In     : GL.Types.Singles.Vector3;
      Vertices_Out   : GL.Types.Singles.Vector3_Array;
      UVs_Out        : GL.Types.Singles.Vector2_Array;
      Normals_Out    : GL.Types.Singles.Vector3_Array;
      Result         : out GL.Types.UInt; Found : out Boolean) is
      use GL.Types;
   begin
      Found := False;
      Result := 0;

      for Index in Vertices_Out'Range loop
         Found := Is_Near (Vertices_In (GL.X), Vertices_Out (Index) (GL.X)) and then
           Is_Near (Vertices_In (GL.Y), Vertices_Out (Index) (GL.Y)) and then
           Is_Near (Vertices_In (GL.Z), Vertices_Out (Index) (GL.Z)) and then
           Is_Near (UVs_In (GL.X), UVs_Out (Index) (GL.X)) and then
           Is_Near (UVs_In (GL.Y), UVs_Out (Index) (GL.Y)) and then
           Is_Near (Normals_In (GL.X), Normals_Out (Index) (GL.X)) and then
           Is_Near (Normals_In (GL.Y), Normals_Out (Index) (GL.Y)) and then
           Is_Near (Normals_In (GL.Z), Normals_Out (Index) (GL.Z));
         if Found then
            Result := GL.Types.UInt (Index - 1);
            exit;
         end if;
      end loop;

   end Get_Similar_Vertex_Index;

   --  -------------------------------------------------------------------------

   procedure Get_Similar_Vertex_Index
     (Vertices_In    : GL.Types.Singles.Vector3;
      UVs_In         : GL.Types.Singles.Vector2;
      Vertices_Out   : GL.Types.Singles.Vector3_Array;
      UVs_Out        : GL.Types.Singles.Vector2_Array;
      Result         : out GL.Types.UInt; Found : out Boolean) is
      use GL.Types;
   begin
      Found := False;
      Result := 0;

      for Index in Vertices_Out'Range loop
         Found := Is_Near (Vertices_In (GL.X), Vertices_Out (Index) (GL.X)) and then
           Is_Near (Vertices_In (GL.Y), Vertices_Out (Index) (GL.Y)) and then
           Is_Near (Vertices_In (GL.Z), Vertices_Out (Index) (GL.Z)) and then
           Is_Near (UVs_In (GL.X), UVs_Out (Index) (GL.X)) and then
           Is_Near (UVs_In (GL.Y), UVs_Out (Index) (GL.Y));
         if Found then
            Result := GL.Types.UInt (Index - 1);
            exit;
         end if;
      end loop;

   end Get_Similar_Vertex_Index;

   --  -------------------------------------------------------------------------

   procedure Index_VBO (Vertices_In    : GL.Types.Singles.Vector3_Array;
                        UVs_In         : GL.Types.Singles.Vector2_Array;
                        Normals_In     : GL.Types.Singles.Vector3_Array;
                        Vertices_Out   : out GL.Types.Singles.Vector3_Array;
                        UVs_Out        : out GL.Types.Singles.Vector2_Array;
                        Normals_Out    : out GL.Types.Singles.Vector3_Array;
                        VBO_Indices    : out GL.Types.UInt_Array;
                        Last_VBO_Index : out GL.Types.Size;
                        Last_Vertex    : out GL.Types.Int) is
      use GL;
      use GL.Types;
      In_Size            : constant Int := Vertices_In'Length;
      Index              : UInt := 0;
      VBO_Indices_Index  : Size := 0;
      Out_Index          : UInt := 0;
      Found              : Boolean;
   begin
      for Vert in 1 .. Int (Vertices_Out'Length) loop
         for elem in Singles.Vector3'Range loop
            Vertices_Out (Vert) (elem) := 0.0;
            if elem /= GL.Z then
               UVs_Out (Vert) (elem) := 0.0;
            end if;
            Normals_Out (Vert) (elem) := 0.0;
         end loop;
      end loop;

      for Vert in 1 .. In_Size loop
         Get_Similar_Vertex_Index (Vertices_In (Vert), UVs_In (Vert), Normals_In (Vert),
                                   Vertices_Out, UVs_Out, Normals_Out,
                                   Index, Found);
         VBO_Indices_Index := VBO_Indices_Index + 1;
         if Found then
            -- A similar vertex is already in the VBO so use it instead
            VBO_Indices (VBO_Indices_Index) := Index;
         else
            --  No other vertex can be used instead so add it to the VBO.
            Out_Index := Out_Index + 1;
            Vertices_Out (Int (Out_Index)) := Vertices_In (Vert);
            UVs_Out (Int (Out_Index)) := UVs_In (Vert);
            Normals_Out (Int (Out_Index)) := Normals_In (Vert);
            VBO_Indices (VBO_Indices_Index) := Out_Index - 1;
         end if;
      end loop;
      Last_Vertex := Int (Out_Index);
      Last_VBO_Index  := VBO_Indices_Index;

   exception
      when others =>
         Put_Line ("An exception occurred in Index_VBO.");
         raise;
   end Index_VBO;

   --  -------------------------------------------------------------------------

   procedure Index_VBO (Vertices_In    : GL.Types.Singles.Vector3_Array;
                        UVs_In         : GL.Types.Singles.Vector2_Array;
                        Vertices_Out   : out GL.Types.Singles.Vector3_Array;
                        UVs_Out        : out GL.Types.Singles.Vector2_Array;
                        VBO_Indices    : out GL.Types.UInt_Array;
                        Last_VBO_Index : out GL.Types.Size;
                        Last_Vertex    : out GL.Types.Int) is
      use GL;
      use GL.Types;
      In_Size            : constant Int := Vertices_In'Length;
      Index              : UInt := 0;
      VBO_Indices_Index  : Size := 0;
      Out_Index          : UInt := 0;
      Found              : Boolean;
   begin
      --  Initialize output arrays
      for Vert in 1 .. Int (Vertices_Out'Length) loop
         for elem in Singles.Vector3'Range loop
            Vertices_Out (Vert) (elem) := 0.0;
            if elem /= GL.Z then
               UVs_Out (Vert) (elem) := 0.0;
            end if;
         end loop;
      end loop;

      for Vert in 1 .. In_Size loop
         Get_Similar_Vertex_Index (Vertices_In (Vert), UVs_In (Vert),
                                   Vertices_Out, UVs_Out, Index, Found);
         VBO_Indices_Index := VBO_Indices_Index + 1;
         if Found then
            -- A similar vertex is already in the VBO so use it instead
            VBO_Indices (VBO_Indices_Index) := Index;
         else
            --  No other vertex can be used instead so add it to the VBO.
            Out_Index := Out_Index + 1;
            Vertices_Out (Int (Out_Index)) := Vertices_In (Vert);
            UVs_Out (Int (Out_Index)) := UVs_In (Vert);
            VBO_Indices (VBO_Indices_Index) := Out_Index - 1;
         end if;
      end loop;
      Last_Vertex := Int (Out_Index);
      Last_VBO_Index  := VBO_Indices_Index;

   exception
      when others =>
         Put_Line ("An exception occurred in Index_VBO.");
         raise;
   end Index_VBO;

   --  -------------------------------------------------------------------------

   function Is_Near (V1, V2 : GL.Types.Single) return Boolean is
      use GL.Types;
   begin
      return Abs (V2 - V1) < 0.01;
   end Is_Near;

   --  -------------------------------------------------------------------------

end VBO_Indexer;
