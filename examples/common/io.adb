with Ada.Sequential_IO; Use Ada;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

with Interfaces.C;

with GL.Types; use GL.Types;

package body IO is

    function To_GL_UByte is new
      Ada.Unchecked_Conversion (Character, UByte);

    --  ------------------------------------------------------------------------

    procedure Read_UByte (File_ID : Byte_IO.File_Type; theByte : out Ubyte) is
        byte_char     : Character;
    begin
        Byte_IO.Read (File_ID, byte_char);
        theByte := To_GL_UByte (byte_char);
    end read_UByte;

    --  ------------------------------------------------------------------------

    procedure Seek (File_ID : Byte_IO.File_Type; Bytes : int) is
        dump : Character;
    begin
        For index in int range 1 .. Bytes loop
            Byte_IO.Read (File_ID, dump);
        end loop;
    end Seek;

    --  ------------------------------------------------------------------------

end IO;
