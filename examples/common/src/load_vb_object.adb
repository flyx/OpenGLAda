
with Interfaces.C.Pointers;

with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;

with Utilities;

package body Load_VB_Object is

   type Image_Data is array (UInt range <>) of aliased UByte;
   package Image_Data_Pointers is new
     Interfaces.C.Pointers (UInt, UByte, Image_Data, UByte'Last);

   procedure Load_Data is new
     GL.Objects.Buffers.Load_To_Buffer (Image_Data_Pointers);

   type Vertex_Data is array (UInt range <>) of aliased Single;
   package Vertex_Data_Pointers is new
     Interfaces.C.Pointers (UInt, Single, Vertex_Data, Single'Last);

   procedure Load_Data is new
     GL.Objects.Buffers.Load_To_Buffer (Vertex_Data_Pointers);

   UInt_Size     : constant UInt := UInt'Size / 8;
   Single_Size   : constant UInt := Single'Size / 8;  --  GLfloat
   UShort_Size   : constant UInt := UShort'Size / 8;

   procedure Load_Raw_Data (File_ID     : Ada.Streams.Stream_IO.File_Type;
                            Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
                            Raw_Data : out Image_Data; Byte_Count : in out UInt);
   procedure Load_Indices (Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
                           Header : VBM_Header;
                           Object : in out VB_Object);
   procedure Load_Materials (Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
                             Header : VBM_Header;
                             Object : in out VB_Object);
   procedure Load_Textures (Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
                             Header : VBM_Header;
                             Object : in out VB_Object);
   procedure Load_VBM_Header (Header_Stream : Ada.Streams.Stream_IO.Stream_Access;
                              Header        : out VBM_Header;
                              Byte_Count    : in out UInt);
  procedure Set_Attributes (VBM_Object : VB_Object;
                             Vertex_Index, Normal_Index, Tex_Coord0_Index : Int);

   --  ------------------------------------------------------------------------

   function Get_Vertex_Count (Object : VB_Object; Frame_Index : UInt := 1) return Int is
      Count : UInt := 0;
      Frame : VBM_Frame_Header;
   begin
      if Object.Header.Num_Frames > 0 then
         if Frame_Index > 0 and then
           Frame_Index <= Object.Header.Num_Frames then
            Frame := Object.Frame_Headers.Element (Integer (Frame_Index));
            Count := Frame.Num_Vertices;
         else
            Put_Line ("Load_VB_Object.Get_Vertex_Count, invalid frame index: " &
                   UInt'Image (Frame_Index));
         end if;
      else
         Put_Line ("Load_VB_Object.Get_Vertex_Count, no frames are available.");
      end if;
      return Int (Count);

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Get_Vertex_Count.");
         raise;
   end Get_Vertex_Count;

   --  ------------------------------------------------------------------------

   procedure Load_Attribute_Header (Header_Stream : Ada.Streams.Stream_IO.Stream_Access;
                                    Header        : out VBM_Attributes_Header;
                                    Byte_Count    : in out UInt) is
      use Ada.Streams.Stream_IO;
      Numeric : UInt;
   begin
      String'Read (Header_Stream, Header.Name);
      Byte_Count := Byte_Count + 64;
      UInt'Read (Header_Stream, Numeric);
      Header.Attribute_Type := Numeric_Type'Enum_Val (Numeric);
      Byte_Count := Byte_Count + UInt_Size;
      UInt'Read (Header_Stream, Header.Components);
      Byte_Count := Byte_Count + UInt_Size;
      UInt'Read (Header_Stream, Header.Flags);
      Byte_Count := Byte_Count + UInt_Size;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Load_Attribute_Header.");
         raise;
   end Load_Attribute_Header;

   --  ------------------------------------------------------------------------

   procedure Load_Frame_Header (Header_Stream : Ada.Streams.Stream_IO.Stream_Access;
                                Header        : out VBM_Frame_Header;
                                Byte_Count    : in out UInt) is
      use Ada.Streams.Stream_IO;
   begin
      UInt'Read (Header_Stream, Header.First);
      Byte_Count := Byte_Count + UInt_Size;
      UInt'Read (Header_Stream, Header.Num_Vertices);
      Byte_Count := Byte_Count + UInt_Size;
      UInt'Read (Header_Stream, Header.Flags);
      Byte_Count := Byte_Count + UInt_Size;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Load_Frame_Header.");
         raise;
   end Load_Frame_Header;

   --  ------------------------------------------------------------------------

   procedure Load_From_VBM (File_Name : String; VBM_Object : out VB_Object;
                            Vertex_Index, Normal_Index, Tex_Coord0_Index : Int;
                            Result : out Boolean) is
      use Ada.Streams.Stream_IO;

      File_ID             : Ada.Streams.Stream_IO.File_Type;
      Data_Stream         : Ada.Streams.Stream_IO.Stream_Access;
      Header              : VBM_Header;
      Attributes_Header   : VBM_Attributes_Header;
      Frame_Header        : VBM_Frame_Header;
      Total_Data_Size     : UInt := 0;
      Byte_Count          : UInt := 0;
   begin
      if Vertex_Index = 0 and Normal_Index = 0 and Tex_Coord0_Index = 0 then
         Put_Line ("Load_From_VBM; all indices are 0");
      end if;
      Result := False;

      Open (File_ID, In_File, File_Name);
      Data_Stream := Stream (File_ID);

      Load_VBM_Header (Data_Stream, Header, Byte_Count);
      declare
         use GL.Objects;
         use GL.Objects.Buffers;
         Object : VB_Object (Positive (Header.Num_Frames));
      begin
         Object.Vertex_Array_Object.Initialize_Id;
         Object.Vertex_Array_Object.Bind;

         Object.Header := Header;
         --  Load attribute headers
         for count in 1 .. Header.Num_Attributes loop
            Load_Attribute_Header (Data_Stream, Attributes_Header, Byte_Count);
            Object.Attribute_Headers.Append (Attributes_Header);
            Total_Data_Size := Total_Data_Size +
              Attributes_Header.Components * Header.Num_Vertices * Single_Size;
         end loop;

         --  Load frame headers
         for count in 1 .. Header.Num_Frames loop
            Load_Frame_Header (Data_Stream, Frame_Header, Byte_Count);
            Object.Frame_Headers.Append (Frame_Header);
         end loop;

         Object.Attribute_Buffer.Initialize_Id;
         Array_Buffer.Bind (Object.Attribute_Buffer);

         declare
            Raw_Data    : Image_Data (1 .. Total_Data_Size);
            --  Image_Data is an array of bytes
--              Vertex_Data : array (1 .. Header.Num_Vertices) of Single :=
--                (others => 0.0);
            Vertices    : Vertex_Data (1 .. Header.Num_Vertices);
            Raw_Index   : UInt := 1;
         begin
            --  Read rest of file.
            Load_Raw_Data (File_ID, Data_Stream, Raw_Data, Byte_Count);
            Utilities.Print_Byte_Array ("Raw Data Array",
                                        Utilities.Byte_Array (Raw_Data), 1, 50);
            for index in Vertices'Range loop
               Vertices (index) := 0.0;
               for i in 1 .. Single_Size loop
                  Vertices (index) := Vertices (index) +
                    Single (Raw_Data (Raw_Index) * 8 ** Natural (i - 1));
               Raw_Index := Raw_Index + 1;
               end loop;
               Vertices (index) := Vertices (index) / 1000.0;
            end loop;
            Utilities.Print_Singles_Array
              ("Vertex_Data", Utilities.Singles_Array (Vertices), 1, 40);
            --  glBufferData(GL_ARRAY_BUFFER, total_data_size, raw_data,
            --               GL_STATIC_DRAW);
--              Load_Data (Array_Buffer, Raw_Data, Static_Draw);
            Load_Data (Array_Buffer, Vertices, Static_Draw);
            Set_Attributes (Object, Vertex_Index, Normal_Index,
                            Tex_Coord0_Index);
            Load_Indices (Data_Stream, Header, Object);

            -- unbind the current array object
            Vertex_Arrays.Bind (GL.Objects.Vertex_Arrays.Null_Array_Object);

            Load_Materials (Data_Stream, Header, Object);
         end;  --  declare block
         Close (File_ID);

         VBM_Object := Object;
      end;  --  outer declare block
      Result := True;

   exception
      when Ada.IO_Exceptions.Name_Error  =>
         --  File not found
         Put_Line ("Load_From_VBM can't find the file " & File_Name & "!");
         raise;
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Load_From_VBM.");
         raise;
   end Load_From_VBM;

   --  ------------------------------------------------------------------------

   procedure Load_Indices (Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
                           Header : VBM_Header;
                           Object : in out VB_Object) is
      use GL.Objects.Buffers;
      Element_Size      : UInt;
      Element_Data_Size : UInt;
   begin
      if Header.Num_Indices > 0 then
         case Header.Index_Type is
         when UShort_Type => Element_Size := UShort_Size;
         when UInt_Type => Element_Size := UInt_Size;
         when others =>
            Put_Line ("Load_VB_Object.Load_Indices, invalid Index_Type.");
         end case;

         Element_Data_Size := Header.Num_Indices * Element_Size;
         Object.Index_Buffer.Initialize_Id;
         Element_Array_Buffer.Bind (Object.Index_Buffer);
         declare
            Indices_Array : Image_Data (1 .. Element_Data_Size);
            Data_Byte     : UByte;
            Byte_Count    : UInt := 0;
         begin
            while Byte_Count < Element_Data_Size loop
               Byte_Count := Byte_Count + 1;
               if not End_Of_File then
                  UByte'Read (Data_Stream, Data_Byte);
                  Indices_Array (Byte_Count) := Data_Byte;
               else
                  Indices_Array (Byte_Count) := 0;
                  Put_Line ("Load_Indices; EOF reached before Indices_Array filled.");
               end if;
            end loop;
            if not End_Of_File then
               Put_Line ("Load_Indices, Indices_Array filled before EOF.");
            end if;
            --  glBufferData(GL_ELEMENT_ARRAY_BUFFER,
            --               m_header.num_indices * element_size,
            --               raw_data + total_data_size, GL_STATIC_DRAW);
            Load_Data (Element_Array_Buffer, Indices_Array, Static_Draw);
         end;  --  declare block
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Load_Indices.");
         raise;
   end Load_Indices;

   --  ------------------------------------------------------------------------

   procedure Load_Materials (Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
                             Header : VBM_Header;
                             Object : in out VB_Object) is
      use GL.Objects.Buffers;
      Material_Record     : VBM_Material;
      Record_Count        : UInt := 0;
      Materials_Data_Size : UInt;
   begin
      if Header.Num_Materials > 0 then
         Materials_Data_Size := Header.Num_Materials * VBM_Material'Size / 8;
         while Record_Count < Materials_Data_Size loop
            Record_Count := Record_Count + 1;
            if not End_Of_File then
               VBM_Material'Read (Data_Stream, Material_Record);
               Object.Materials.Append (Material_Record);
            else
               Put_Line ("Load_Materials; EOF reached before Materials loading completed.");
            end if;
         end loop;
         Load_Textures (Data_Stream, Header, Object);
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Load_Materials.");
         raise;
   end Load_Materials;

   --  ------------------------------------------------------------------------

   procedure Load_Raw_Data (File_ID     : Ada.Streams.Stream_IO.File_Type;
                            Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
                            Raw_Data : out Image_Data; Byte_Count : in out UInt) is
   begin
      for Count in UInt range 1 .. Raw_Data'Length loop
         Byte_Count := Byte_Count + 1;
         if not Ada.Streams.Stream_IO.End_Of_File (File_ID) then
            UByte'Read (Data_Stream, Raw_Data (Count));
         else
            Raw_Data (Count) := 0;
            Put_Line ("Load_Raw_Data, EOF reached before raw data list filled.");
         end if;
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Load_Raw_Data.");
         raise;
   end Load_Raw_Data;

   --  ------------------------------------------------------------------------

   procedure Load_Textures (Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
                            Header : VBM_Header;
                            Object : in out VB_Object) is
      use GL.Objects.Buffers;
      Texture_Record     : Material_Texture;
      Record_Count       : UInt := 0;
      Textures_Data_Size : UInt;
   begin
         Textures_Data_Size := Header.Num_Materials * Material_Texture'Size / 8;
         while Record_Count < Textures_Data_Size loop
            Record_Count := Record_Count + 1;
            if not End_Of_File then
               Material_Texture'Read (Data_Stream, Texture_Record);
               Object.Material_Textures.Append (Texture_Record);
            else
               Put_Line ("Load_Textures; EOF reached before Materials completed.");
            end if;
         end loop;

         if not End_Of_File then
            Put_Line ("Load_Textures, Materials filled before EOF.");
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Load_Textures.");
         raise;
   end Load_Textures;

   --  ------------------------------------------------------------------------

   procedure Load_VBM_Header (Header_Stream : Ada.Streams.Stream_IO.Stream_Access;
                              Header        : out VBM_Header;
                              Byte_Count    : in out UInt) is
      use Ada.Streams.Stream_IO;
      Magic         : UInt;
      Index_Type    : UInt := 0;
   begin
      UInt'Read (Header_Stream, Magic);
      Byte_Count := Byte_Count + UInt_Size;
      declare
         Head : VBM_Header (Magic);
      begin
         UInt'Read (Header_Stream, Head.Size);
         Byte_Count := Byte_Count + UInt_Size;
         String'Read (Header_Stream, Head.Name);
         Byte_Count := Byte_Count + 64;
         UInt'Read (Header_Stream, Head.Num_Attributes);
         Byte_Count := Byte_Count + UInt_Size;
         UInt'Read (Header_Stream, Head.Num_Frames);
         Byte_Count := Byte_Count + UInt_Size;
         if Header.Magic /= New_Header_Magic then
            UInt'Read (Header_Stream, Head.Num_Chunks);
            Byte_Count := Byte_Count + UInt_Size;
         end if;
         UInt'Read (Header_Stream, Head.Num_Vertices);
         Byte_Count := Byte_Count + UInt_Size;
         UInt'Read (Header_Stream, Head.Num_Indices);
         Byte_Count := Byte_Count + UInt_Size;
         UInt'Read (Header_Stream, Index_Type);
         if Index_Type /= 0 then
            Head.Index_Type := Numeric_Type'Enum_Val (Index_Type);
         end if;
         Byte_Count := Byte_Count + UInt_Size;
         if Byte_Count < Head.Size then
            UInt'Read (Header_Stream, Head.Num_Materials);
            Byte_Count := Byte_Count + UInt_Size;
            if Byte_Count < Head.Size then
               UInt'Read (Header_Stream, Head.Flags);
               Byte_Count := Byte_Count + UInt_Size;
            end if;
         end if;
         Header := Head;
      end;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Load_VBM_Header.");
         raise;
   end Load_VBM_Header;

   --  ------------------------------------------------------------------------

   procedure Print_Attributes_Header (Message : String; Object : VB_Object;
                                      Attributes_Index : UInt) is
      Attributes : VBM_Attributes_Header;
   begin
      if Attributes_Index < Object.Header.Num_Attributes then
         Attributes := Object.Attribute_Headers.Element (Natural (Attributes_Index));
         Put_Line (Message);
         Put_Line ("Name: " & Attributes.Name);
         Put_Line ("Type: " & Numeric_Type'Image (Attributes.Attribute_Type));
         Put_Line ("Components: " & UInt'Image (Attributes.Components));
         Put_Line ("Flags: " & UInt'Image (Attributes.Flags));
      else
         Put_Line ("Load_VB_Object.Print_Attributes_Header, invalid attribute index: " &
                  UInt'Image (Attributes_Index));
      end if;
      New_Line;
   end Print_Attributes_Header;

   --  ------------------------------------------------------------------------

   procedure Print_VBM_Frame_Data (Message : String; Object : VB_Object;
                                   Frame_Index : UInt) is
      Frame : VBM_Frame_Header;
   begin
      if Frame_Index > 0 and then Frame_Index <= Object.Header.Num_Frames then
         Frame := Object.Frame_Headers.Element (Natural (Frame_Index));
         Put_Line (Message);
         Put_Line ("First: " & UInt'Image (Frame.First));
         Put_Line ("Vertices: " & UInt'Image (Frame.Num_Vertices));
         Put_Line ("Flags: " & UInt'Image (Frame.Flags));
      else
         Put_Line ("Load_VB_Object.Print_VBM_Frame_Data, invalid frame index: " &
                  UInt'Image (Frame_Index));
      end if;
      New_Line;
   end Print_VBM_Frame_Data;

   --  ------------------------------------------------------------------------

   procedure Print_VBM_Object_Data (Message : String; Object : VB_Object) is
   begin
      Put_Line (Message);

      Put_Line ("Header.Size: " & UInt'Image (Object.Header.Size));
      Put_Line ("Number of attributes: " & UInt'Image (Object.Header.Num_Attributes));
      Put_Line ("Number of frames: " & UInt'Image (Object.Header.Num_Frames));
      Put_Line ("Number of vertices: " & UInt'Image (Object.Header.Num_Vertices));
      Put_Line ("Number of indices: " & UInt'Image (Object.Header.Num_Indices));
      Put_Line ("Index type: " & Numeric_Type'Image (Object.Header.Index_Type));
      Put_Line ("Number of Materials: " & UInt'Image (Object.Header.Num_Materials));
      if Object.Header.Magic /= New_Header_Magic then
         Put_Line ("Number of chunks: " & UInt'Image (Object.Header.Num_Chunks));
      end if;
      Put_Line ("Vertex_Array_Object size: " & Integer'Image (Object.Vertex_Array_Object'Size));
      Put_Line ("Attribute_Buffer size: " & Integer'Image (Object.Attribute_Buffer'Size));
      Put_Line ("Index_Buffer size: " & Integer'Image (Object.Index_Buffer'Size));
      New_Line;

   end Print_VBM_Object_Data;

   --  ------------------------------------------------------------------------

   procedure Render (VBM_Object : VB_Object;
                     Frame_Index : UInt := 1; Instances : UInt := 0) is
      use GL.Objects.Buffers;
      use GL.Objects;
      Frame : VBM_Frame_Header;
   begin
      if Frame_Index > 0 and then Frame_Index <= VBM_Object.Header.Num_Frames then
         Vertex_Arrays.Bind (VBM_Object.Vertex_Array_Object);
         Array_Buffer.Bind (VBM_Object.Attribute_Buffer);
--           Set_Attributes (VBM_Object, 0, 1, 2);
         Frame := VBM_Object.Frame_Headers.Element (Natural (Frame_Index));
         if Instances > 0 then
            if VBM_Object.Header.Num_Indices > 0 then
               Draw_Elements_Instanced (Triangles, Frame.Num_Vertices,
                                        GL.Types.UInt_Type, Frame.First);
            else
               Vertex_Arrays.Draw_Arrays_Instanced
                 (Triangles, Int (Frame.First), Int (Frame.Num_Vertices),
                  Int (Instances));
            end if;
         else
            if VBM_Object.Header.Num_Indices > 0 then
               Put_Line ("Load_VB_Object.Render Num_Indices > 0");
               null;
            else
               Vertex_Arrays.Draw_Arrays (Triangles, Int (Frame.First),
                                          Int (Frame.Num_Vertices));
            end if;
         end if;
         Vertex_Arrays.Null_Array_Object.Bind;
      else
         Put_Line ("Load_VB_Object.Render, invalid frame index: " &
                  UInt'Image (Frame_Index));
      end if;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Render.");
         raise;
   end Render;

   --  ------------------------------------------------------------------------

   procedure Set_Attributes (VBM_Object : VB_Object;
                             Vertex_Index, Normal_Index, Tex_Coord0_Index : Int) is
      use GL.Attributes;
      Attribute_Index : Attribute;  --  UInt
      Attr_Header     : VBM_Attributes_Header;
      Data_Offset     : Int := 0;  --  Offset into buffer
   begin
--        GL.Objects.Vertex_Arrays.Bind (VBM_Object.Vertex_Array_Object);
      for Index in 0 .. VBM_Object.Header.Num_Attributes - 1 loop
         Attr_Header := VBM_Object.Attribute_Headers.Element (Natural (Index));
         case Index is
         when 0 => Attribute_Index := Attribute (Vertex_Index);  --  Vertices
         when 1 => Attribute_Index := Attribute (Normal_Index);  --  Indices
         when 2 => Attribute_Index := Attribute (Tex_Coord0_Index);  --  Texture coordinates
         when others =>
            Put_Line ("Load_VB_Object.Set_Attributes, invalid attribute index.");
         end case;

         GL.Attributes.Set_Vertex_Attrib_Pointer
           (Index  => Attribute_Index,
            Count  => Int (Attr_Header.Components),
            Kind   => Attr_Header.Attribute_Type,
            Stride => 0, Offset => Data_Offset);
         GL.Attributes.Enable_Vertex_Attrib_Array (Attribute_Index);
         Data_Offset := Data_Offset +
           Int (Attr_Header.Components * VBM_Object.Header.Num_Vertices * Single_Size);
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VB_Object.Set_Attributes.");
         raise;
   end Set_Attributes;

   --  ------------------------------------------------------------------------

end Load_VB_Object;
