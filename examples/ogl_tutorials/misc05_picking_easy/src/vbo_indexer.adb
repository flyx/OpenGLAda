
--  with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

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

   procedure Get_Similar_Index_Slow
     (Data_In : GL.Types.Singles.Vector2;
      Data    : GL.Types.Singles.Vector2_Array;
      Result  : out GL.Types.Int; Found : out Boolean) is
   begin
      Found := False;
      Result := 0;
      for Index in Data'Range loop
         Found := Is_Near (Data_In (GL.X), Data (Index) (GL.X))
                  and then Is_Near (Data_In (GL.Y), Data (Index) (GL.Y));
         if Found then
            Result := Index;
            exit;
         end if;
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Get_Similar_Vertex_Index_Slow.");
         raise;
   end Get_Similar_Index_Slow;

   --  -------------------------------------------------------------------------

   procedure Get_Similar_Index_Slow
     (Data_In : GL.Types.Singles.Vector3;
      Data    : GL.Types.Singles.Vector3_Array;
      Result  : out GL.Types.Int; Found : out Boolean) is
   begin
      Found := False;
      Result := 0;
      for Index in Data'Range loop
         Found := Is_Near (Data_In (GL.X), Data (Index) (GL.X))
                  and then Is_Near (Data_In (GL.Y), Data (Index) (GL.Y))
                  and then Is_Near (Data_In (GL.Z), Data (Index) (GL.Z));
         if Found then
            Result := Index;
            exit;
         end if;
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Get_Similar_Vertex_Index_Slow.");
         raise;
   end Get_Similar_Index_Slow;

   --  -------------------------------------------------------------------------

   procedure Index_VBO (Vertices_In : GL.Types.Singles.Vector3_Array;
                        UVs_In      : GL.Types.Singles.Vector2_Array;
                        Normals_In  : GL.Types.Singles.Vector3_Array;
                        Vertices    : in out GL.Types.Singles.Vector3_Array;
                        UVs         : in out GL.Types.Singles.Vector2_Array;
                        Normals     : in out GL.Types.Singles.Vector3_Array;
                        Indices     : out GL.Types.Int_Array;
                        Last_Index  : out GL.Types.Int;
                        Last_Vertex : out GL.Types.Int;
                        Last_UV     : out GL.Types.Int;
                        Last_Normal : out GL.Types.Int) is
      use GL.Types;
      Vert_In_Size   : constant Int := Vertices_In'Length;
      UVs_In_Size    : constant Int := UVs_In'Length;
      Norm_In_Size   : constant Int := Normals_In'Length;
      Max_Size       : Int := Maths.Maximum (Vert_In_Size, UVs_In_Size);
      Index          : Int := 0;
      Indices_Index  : Int := 0;
      Out_Index      : Int := 0;
      Found          : Boolean;
   begin
      Max_Size := Maths.Maximum (Max_Size, Norm_In_Size);
      for Vert in 1 .. Max_Size loop
         Found := False;
         if Vert <= Vert_In_Size then
            Get_Similar_Index_Slow (Vertices_In (Vert), Vertices, Index, Found);
         end if;
         if not Found and then Vert <= UVs_In_Size then
            Get_Similar_Index_Slow (UVs_In (Vert), UVs, Index, Found);
         end if;
         if not Found and then Vert <= Norm_In_Size then
            Get_Similar_Index_Slow (Normals_In (Vert), Normals, Index, Found);
         end if;
         Indices_Index := Indices_Index + 1;

         if not Found then
            Put_Line ("Index_VBO found, Index : " & Int'Image (Index));
            Indices (Indices_Index) := Index;
         else
            Out_Index := Out_Index + 1;
            if Vert <= Vert_In_Size then
               Vertices (Out_Index) := Vertices_In (Vert);
               Last_Vertex := Out_Index;
            end if;
            if Vert <= UVs_In_Size then
               UVs (Out_Index) := UVs (Vert);
               Last_UV := Out_Index;
            end if;
            if Vert <= Norm_In_Size then
               Normals (Out_Index) := Normals_In (Vert);
               Indices (Indices_Index) := Out_Index;
               Last_Normal := Out_Index;
            end if;
         end if;
      end loop;
      Last_Index  := Indices_Index;

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
