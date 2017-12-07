
--  with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO; use Ada.Text_IO;

package body VBO_Indexer is

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
   end Get_Similar_Index_Slow;

   --  -------------------------------------------------------------------------

   procedure Index_VBO (Vertices_In    : GL.Types.Singles.Vector3_Array;
                        UVs_In         : GL.Types.Singles.Vector2_Array;
                        Normals_In     : GL.Types.Singles.Vector3_Array;
                        Vertices_Out   : out GL.Types.Singles.Vector3_Array;
                        UVs_Out        : out GL.Types.Singles.Vector2_Array;
                        Normals_Out    : out GL.Types.Singles.Vector3_Array;
                        VBO_Indices    : out GL.Types.Int_Array;
                        Last_VBO_Index : out GL.Types.Int;
                        Last_Vertex    : out GL.Types.Int) is
      use GL;
      use GL.Types;
      Vert_In_Size       : constant Int := Vertices_In'Length;
      Index              : Int := 0;
      Check_Index        : Int := 0;
      VBO_Indices_Index  : Int := 0;
      Out_Index          : Int := 0;
      Found              : Boolean;
   begin
      for Vert in 1 .. Vert_In_Size loop
         for elem in Singles.Vector3'Range loop
            Vertices_Out (Vert) (elem) := 0.0;
            if elem /= GL.Z then
               UVs_Out (Vert) (elem) := 0.0;
            end if;
            Normals_Out (Vert) (elem) := 0.0;
         end loop;
      end loop;

      for Vert in 1 .. Vert_In_Size loop
         Found := False;
         Get_Similar_Index_Slow (Vertices_In (Vert), Vertices_Out, Check_Index, Found);
         if Found then
            Get_Similar_Index_Slow (UVs_In (Vert), UVs_Out, Index, Found);
         end if;
         if Found and then Index = Check_Index then
            Check_Index := Index;
            Get_Similar_Index_Slow (Normals_In (Vert), Normals_Out, Index, Found);
         end if;

         VBO_Indices_Index := VBO_Indices_Index + 1;
         if Found and then Index = Check_Index then
            -- A similar vertex is already in the VBO so use it instead
            VBO_Indices (VBO_Indices_Index) := Check_Index;
         else
            --  No other vertex can be used instead so add it to the VBO.
            Out_Index := Out_Index + 1;
            Vertices_Out (Out_Index) := Vertices_In (Vert);
            Last_Vertex := Out_Index;
            UVs_Out (Out_Index) := UVs_In (Vert);
            Normals_Out (Out_Index) := Normals_In (Vert);
            VBO_Indices (VBO_Indices_Index) := Out_Index;
         end if;
      end loop;
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
