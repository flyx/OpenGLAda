
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Load_VB_Object is

   type VBM_Data is array (GL.Types.UInt range <>) of GL.Types.UByte;

   procedure Load_VBM_Header (File_ID : Ada.Streams.Stream_IO.File_Type;
                              Header  : out VBM_Header);

   --  ------------------------------------------------------------------------

   procedure Load_From_VBM (File_Name : String;
                            Vertex_Index, Normal_Index,
                            Tex_Coord0_Index : out Int; Result : out Boolean) is
      use Ada.Streams.Stream_IO;

      File_ID      : Ada.Streams.Stream_IO.File_Type;
      Data_Stream  : Ada.Streams.Stream_IO.Stream_Access;
      Header       : VBM_Header;
      Old_Header   : VBM_Old_Header;
   begin
      Result := False;
      Open (File_ID, In_File, File_Name);

   exception
      when Ada.IO_Exceptions.Name_Error  =>
         --  File not found
         Put_Line ("Load_DDS can't find the file " & File_Name & "!");
         raise;
      when others =>
         Put_Line ("An exception occurred in Load_DDS.");
         raise;
   end Load_From_VBM;

   --  ------------------------------------------------------------------------

   procedure Load_VBM_Header (File_ID : Ada.Streams.Stream_IO.File_Type;
                              Header  : out VBM_Header) is
      use Ada.Streams.Stream_IO;

      Header_Stream   : constant Stream_Access := Stream (File_ID);
   begin
      UInt'Read (Header_Stream, Header.Magic);
      UInt'Read (Header_Stream, Header.Size);
      String'Read (Header_Stream, Header.Name);
      UInt'Read (Header_Stream, Header.Num_Attributes);
      UInt'Read (Header_Stream, Header.Num_Frames);
      if Header.Magic /= New_Header_Magic then
         UInt'Read (Header_Stream, Header.Num_Chunks);
      end if;
      UInt'Read (Header_Stream, Header.Num_Vertices);
      UInt'Read (Header_Stream, Header.Num_Indices);
      UInt'Read (Header_Stream, Header.Index_Type);
      UInt'Read (Header_Stream, Header.Num_Materials);
      UInt'Read (Header_Stream, Header.Flags);

   exception
      when others =>
         Put_Line ("An exception occurred in Load_VBM_Header.");
         raise;
   end Load_VBM_Header;

   --  ------------------------------------------------------------------------

end Load_VB_Object;
