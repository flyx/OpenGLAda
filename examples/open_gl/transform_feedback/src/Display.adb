

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Queries;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;

with Program_Loader;
with Utilities;

procedure Display is

   use GL.Objects.Buffers;
   use GL.Objects.Programs;
   use GL.Objects.Shaders;
   use GL.Types;
   use GL.Toggles;
   use Program_Loader;

   Data              : constant GL.Types.Single_Array (1 .. 5) :=
                         (1.0, 2.0, 3.0, 4.0, 5.0);
   Data_Bytes        : constant GL.Types.Long := Data'Size / 8;
   Shader_Program    : GL.Objects.Programs.Program;
   Vertex_Array      : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer     : GL.Objects.Buffers.Buffer;
   Feedback_Buffer   : GL.Objects.Buffers.Buffer;
   Input_Attribute   : GL.Attributes.Attribute;
   Query             : GL.Objects.Queries.Query_Object;
   Out_Varying       : constant String := "outValue";
   Primitives        : UInt;
   Feedback          : GL.Types.Single_Array  (1 .. 15) := (others => 99.0);
   OK                : Boolean;
begin
   Vertex_Array.Initialize_Id;
   Vertex_Array.Bind;
   Shader_Program := Program_From
     ((Src ("src/shaders/feedback_vertex_shader.glsl", Vertex_Shader),
      Src ("src/shaders/feedback_geometry_shader.glsl", Geometry_Shader)));

   Shader_Program.Use_Program;
   --  Interleaved_Attribs means that the varyings are recorded
   --  consecutively into a single buffer.
   Transform_Feedback_Varyings (Shader_Program, Out_Varying, Interleaved_Attribs);
   Shader_Program.Link;
   OK := GL.Objects.Programs.Link_Status (Shader_Program);
   if not OK then
      Put_Line ("Main_Loop.Setup, Shader_Program Link for Out_Varying failed");
      Put_Line (GL.Objects.Programs.Info_Log (Shader_Program));
   end if;

   Vertex_Buffer.Initialize_Id;
   Array_Buffer.Bind (Vertex_Buffer);
   Utilities.Load_Singles_Buffer (Array_Buffer, Data, Static_Draw);

   Shader_Program.Use_Program;
   Input_Attribute := GL.Objects.Programs.Attrib_Location
     (Shader_Program, "inValue");
   GL.Attributes.Enable_Vertex_Attrib_Array (Input_Attribute);
   GL.Attributes.Set_Vertex_Attrib_Pointer
     (Input_Attribute, 1, GL.Types.Single_Type, False, 0, 0);

   Feedback_Buffer.Initialize_Id;
   Array_Buffer.Bind (Feedback_Buffer);
   Array_Buffer.Allocate (3 * Data_Bytes, Static_Read);

   Query.Initialize_Id;
   Enable (Rasterizer_Discard);
   GL.Objects.Buffers.Transform_Feedback_Buffer.Bind_Buffer_Base
     (0, Feedback_Buffer);

   GL.Objects.Queries.Begin_Query
     (GL.Low_Level.Enums.Transform_Feedback_Primitives_Written, Query);
   GL.Objects.Programs.Begin_Transform_Feedback (Triangles);
   GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 5);
   GL.Objects.Programs.End_Transform_Feedback;
   GL.Objects.Queries.End_Query
     (GL.Low_Level.Enums.Transform_Feedback_Primitives_Written);
   Disable (Rasterizer_Discard);
   Flush;

   GL.Objects.Queries.Get_Query_Object
     (Query, GL.Low_Level.Enums.Query_Result, Primitives);
   Put_Line (UInt'Image (Primitives) & " primitives written!");

   Get_Sub_Data (Transform_Feedback_Buffer, 0, Feedback);
   Put_Line ("Feedback values: ");
   for count in Feedback'Range loop
      Put (Single'Image (Feedback (count)) & "  ");
      if count mod 3 = 0 then
         New_Line;
      end if;
   end loop;
   New_Line;

exception
   when others =>
      Put_Line ("An exception occurred in Display.");
      raise;
end Display;
