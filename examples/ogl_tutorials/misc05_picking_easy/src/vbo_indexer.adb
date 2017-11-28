
--  with Ada.Containers.Indefinite_Ordered_Maps;

package body VBO_Indexer is

--     package VBO_Map is
--       new Ada.Containers.Indefinite_Ordered_Maps
--           (Key_Type => tElement, Element_Type => tEnergy_Level_Basis_Set);
--     type tBasis_Set_Map is new Basis_Set_Map.Map with null record;

   type Packed_Vertex is record
      Position : GL.Types.Singles.Vector3;
      UV       : GL.Types.Singles.Vector2;
      Normal   : GL.Types.Singles.Vector3;
   end record;

   function Is_Near (V1, V2 : GL.Types.Single) return Boolean;

   --  -------------------------------------------------------------------------

  function Get_Similar_Index_Slow (Packed : Packed_Vertex) return Boolean is

   begin
      return False;
   end Get_Similar_Index_Slow;

   --  -------------------------------------------------------------------------

   procedure Index_VBO (Vertices_In : GL.Types.Singles.Vector3_Array;
                        UVs_In      : GL.Types.Singles.Vector2_Array;
                        Normals_In  : GL.Types.Singles.Vector3_Array;
                        Vertices    : GL.Types.Singles.Vector3_Array;
                        UVs         : GL.Types.Singles.Vector2_Array;
                        Normals     : GL.Types.Singles.Vector3_Array) is
      Packed : Packed_Vertex;
      Found  : Boolean := False;
   begin
      Found := Get_Similar_Index_Slow (Packed);
   end Index_VBO;

   --  -------------------------------------------------------------------------

   procedure Get_Similar_Vertex_index
     (Vertices_In : GL.Types.Singles.Vector3_Array;
      UVs_In      : GL.Types.Singles.Vector2_Array;
      Normals_In  : GL.Types.Singles.Vector3_Array;
      Vertices    : out GL.Types.Singles.Vector3_Array;
      UVs         : out GL.Types.Singles.Vector2_Array;
      Normals     : out GL.Types.Singles.Vector3_Array;
      Result      : out Integer; Found : out Boolean) is
      Packed : Packed_Vertex;
   begin
      Found := False;
      for Index in Vertices'Range loop
         Found := Is_Near (Vertices_In (Index) (GL.X),
                           Vertices (Index) (GL.X))
                  and then Is_Near (Vertices_In (Index) (GL.Y),
                           Vertices (Index) (GL.Y))
                  and then Is_Near (Vertices_In (Index) (GL.Z),
                                    Vertices (Index) (GL.Z))
                  and then Is_Near (UVs_In (Index) (GL.X),
                           UVs (Index) (GL.X))
                  and then Is_Near (UVs_In (Index) (GL.Y),
                                    UVs (Index) (GL.Y))
                  and then Is_Near (Normals_In (Index) (GL.X),
                            Normals (Index) (GL.X))
                  and then Is_Near (Normals_In (Index) (GL.Y),
                             Normals (Index) (GL.Y))
                  and then Is_Near (Normals_In (Index) (GL.Z),
                                    Normals (Index) (GL.Z));
         if Found then
            exit;
         end if;
      end loop;
   end Get_Similar_Vertex_index;

   --  -------------------------------------------------------------------------

   function Is_Near (V1, V2 : GL.Types.Single) return Boolean is
      use GL.Types;
   begin
     return Abs (V2 - V1) < 0.01;
   end Is_Near;

  --  -------------------------------------------------------------------------

end VBO_Indexer;
