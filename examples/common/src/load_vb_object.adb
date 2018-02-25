
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Load_VB_Object is

   --     type VBM_Data is array (GL.Types.UInt range <>) of GL.Types.UByte;

   UInt_Size      : constant UInt := UInt'Size / 8;
   Byte_Count     : UInt := 0;


   procedure Load_Attribute_Header (Header_Stream : Ada.Streams.Stream_IO.Stream_Access;
                              Header              : out VBM_Attributes_Header);
   procedure Load_VBM_Header (Header_Stream : Ada.Streams.Stream_IO.Stream_Access;
                              Header        : out VBM_Header);

   --  ------------------------------------------------------------------------

   procedure Load_From_VBM (File_Name : String;
                            Vertex_Index, Normal_Index, Tex_Coord0_Index : Int;
                            Result : out Boolean) is
      use Ada.Streams.Stream_IO;

      File_ID           : Ada.Streams.Stream_IO.File_Type;
      Data_Stream       : Ada.Streams.Stream_IO.Stream_Access;
      Header            : VBM_Header;
      Attributes_Header : VBM_Attributes_Header;
   begin
      if Vertex_Index = 0 and Normal_Index = 0 and Tex_Coord0_Index = 0 then
         Put_Line ("Load_From_VBM; all indices are 0");
      end if;
      Result := False;
      Open (File_ID, In_File, File_Name);
      Data_Stream := Stream (File_ID);

      Load_VBM_Header (Data_Stream, Header);
      Load_Attribute_Header (Data_Stream, Attributes_Header);
         Put_Line ("Load_VBM_Header Head.Name: " & Header.Name);
      Put_Line ("Load_Attribute_Header Header.Name: " & Attributes_Header.Name);

      Close (File_ID);

   exception
      when Ada.IO_Exceptions.Name_Error  =>
         --  File not found
         Put_Line ("Load_From_VBM can't find the file " & File_Name & "!");
         raise;
      when others =>
         Put_Line ("An exception occurred in Load_From_VBM.");
         raise;
   end Load_From_VBM;

   --  ------------------------------------------------------------------------

   procedure Load_Attribute_Header (Header_Stream : Ada.Streams.Stream_IO.Stream_Access;
                                    Header        : out VBM_Attributes_Header) is
      use Ada.Streams.Stream_IO;
   begin
      String'Read (Header_Stream, Header.Name);
      Byte_Count := Byte_Count + 64;
      Put_Line ("Load_Attribute_Header Header.Name: " & Header.Name);
      UInt'Read (Header_Stream, Header.Attribute_Type);
      Byte_Count := Byte_Count + UInt_Size;
      UInt'Read (Header_Stream, Header.Components);
      Byte_Count := Byte_Count + UInt_Size;
      UInt'Read (Header_Stream, Header.Flags);
      Byte_Count := Byte_Count + UInt_Size;
      Put_Line ("Load_Attribute_Header Byte_Count: " & UInt'Image (Byte_Count));

   exception
      when others =>
         Put_Line ("An exception occurred in Load_Attribute_Header.");
         raise;
   end Load_Attribute_Header;

   --  ------------------------------------------------------------------------

   procedure Load_VBM_Header (Header_Stream : Ada.Streams.Stream_IO.Stream_Access;
                              Header        : out VBM_Header) is
      use Ada.Streams.Stream_IO;
      Magic          : UInt;
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
         Put_Line ("Load_VBM_Header Head.Name: " & Head.Name);
         UInt'Read (Header_Stream, Head.Num_Attributes);
         Byte_Count := Byte_Count + UInt_Size;
         UInt'Read (Header_Stream, Head.Num_Frames);
         Byte_Count := Byte_Count + UInt_Size;
         Put_Line ("Load_VBM_Header New_Header_Magic, Magic: " &
                    UInt'Image (New_Header_Magic) & "  " & UInt'Image (Magic));
         Put_Line ("Load_VBM_Header Head.Size: " & UInt'Image (Head.Size));
         Put_Line ("Load_VBM_Header Head.Num_Attributes: " & UInt'Image (Head.Num_Attributes));
         Put_Line ("Load_VBM_Header Head.Num_Frames: " & UInt'Image (Head.Num_Frames));
         if Header.Magic /= New_Header_Magic then
            UInt'Read (Header_Stream, Head.Num_Chunks);
            Byte_Count := Byte_Count + UInt_Size;
            Put_Line ("Load_VBM_Header Head.Num_Chunks: " & UInt'Image (Head.Num_Chunks));
         end if;
         UInt'Read (Header_Stream, Head.Num_Vertices);
         Byte_Count := Byte_Count + UInt_Size;
         UInt'Read (Header_Stream, Head.Num_Indices);
         Byte_Count := Byte_Count + UInt_Size;
         UInt'Read (Header_Stream, Head.Index_Type);
         Byte_Count := Byte_Count + UInt_Size;
         if Byte_Count < Head.Size then
            UInt'Read (Header_Stream, Head.Num_Materials);
            Byte_Count := Byte_Count + UInt_Size;
            if Byte_Count < Head.Size then
               UInt'Read (Header_Stream, Head.Flags);
               Byte_Count := Byte_Count + UInt_Size;
            end if;
         end if;
         Put_Line ("Load_VBM_Header Head.Num_Vertices: " & UInt'Image (Head.Num_Vertices));
         Put_Line ("Load_VBM_Header Head.Num_Indices: " & UInt'Image (Head.Num_Indices));
         Put_Line ("Load_VBM_Header Head.Index_Type: " & UInt'Image (Head.Index_Type));
         Put_Line ("Load_VBM_Header Head.Num_Materials: " & UInt'Image (Head.Num_Materials));
         Put_Line ("Load_VBM_Header Byte_Count: " & UInt'Image (Byte_Count));
         Header := Head;
      end;

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VBM_Header.");
         raise;
   end Load_VBM_Header;

   --  ------------------------------------------------------------------------

end Load_VB_Object;
