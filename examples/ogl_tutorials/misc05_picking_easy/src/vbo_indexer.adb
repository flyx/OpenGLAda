
--  with Ada.Containers.Indefinite_Ordered_Maps;

package body VBO_Indexer is

--     package VBO_Map is
--       new Ada.Containers.Indefinite_Ordered_Maps
--           (Key_Type => tElement, Element_Type => tEnergy_Level_Basis_Set);
--     type tBasis_Set_Map is new Basis_Set_Map.Map with null record;

--     type Packed_Vertex is record
--        Position : GL.Types.Singles.Vector3;
--        UV       : GL.Types.Singles.Vector2;
--        Normal   : GL.Types.Singles.Vector3;
--     end record;

   function Is_Near (V1, V2 : GL.Types.Single) return Boolean;

   --  -------------------------------------------------------------------------

   procedure Get_Similar_Vertex_Index_Slow
     (Vertex_In : GL.Types.Singles.Vector3;
      UV_In     : GL.Types.Singles.Vector2;
      Normal_In : GL.Types.Singles.Vector3;
      Vertices  : GL.Types.Singles.Vector3_Array;
      UVs       : GL.Types.Singles.Vector2_Array;
      Normals   : GL.Types.Singles.Vector3_Array;
      Result    : out GL.Types.Int; Found : out Boolean) is
   begin
      Found := False;
      for Index in Vertices'Range loop
         Found := Is_Near (Vertex_In (GL.X), Vertices (Index) (GL.X))
                  and then Is_Near (Vertex_In (GL.Y), Vertices (Index) (GL.Y))
                  and then Is_Near (Vertex_In (GL.Z), Vertices (Index) (GL.Z))
                  and then Is_Near (UV_In (GL.X), UVs (Index) (GL.X))
                  and then Is_Near (UV_In (GL.Y), UVs (Index) (GL.Y))
                  and then Is_Near (Normal_In (GL.X), Normals (Index) (GL.X))
                  and then Is_Near (Normal_In (GL.Y), Normals (Index) (GL.Y))
                  and then Is_Near (Normal_In (GL.Z), Normals (Index) (GL.Z));
         if Found then
            Result := Index;
            exit;
         end if;
      end loop;
   end Get_Similar_Vertex_Index_Slow;

   --  -------------------------------------------------------------------------

   procedure Index_VBO (Vertices_In : GL.Types.Singles.Vector3_Array;
                        UVs_In      : GL.Types.Singles.Vector2_Array;
                        Normals_In  : GL.Types.Singles.Vector3_Array;
                        Vertices    : in out GL.Types.Singles.Vector3_Array;
                        UVs         : in out GL.Types.Singles.Vector2_Array;
                        Normals     : in out GL.Types.Singles.Vector3_Array;
                        Indices     : out GL.Types.Int_Array;
                        Last_Index  : out GL.Types.Int;
                        Last_Vertex : out GL.Types.Int) is
      use GL.Types;
      Index          : Int;
      Indices_Index  : Int := 0;
      Out_Index      : Int := 0;
      Found          : Boolean := False;
   begin
      for Vert in Vertices_In'Range loop
         Get_Similar_Vertex_Index_Slow (Vertices_In (Vert), UVs_In (Vert), Normals_In (Vert),
                                        Vertices, UVs, Normals, Index, Found);
         Indices_Index := Indices_Index + 1;
         if Found then
            Indices (Indices_Index) := Index;
         else
            Out_Index := Out_Index + 1;
            Vertices (Out_Index) := Vertices_In (Index);
            UVs (Out_Index) := UVs (Index);
            Normals (Out_Index) := Normals_In (Index);
            Indices (Indices_Index) := Out_Index;
         end if;
      end loop;
      Last_Index  := Indices_Index;
      Last_Vertex := Out_Index;
   end Index_VBO;

   --  -------------------------------------------------------------------------

   function Is_Near (V1, V2 : GL.Types.Single) return Boolean is
      use GL.Types;
   begin
     return Abs (V2 - V1) < 0.01;
   end Is_Near;

  --  -------------------------------------------------------------------------

end VBO_Indexer;
