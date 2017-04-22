
with Ada.Sequential_IO; Use Ada;

with GL.Types; use GL.Types;

package IO is
    package Byte_IO is new Sequential_IO (Character);
    package Integer_IO is new Sequential_IO (Integer);

    procedure Seek (File_ID : Byte_IO.File_Type; Bytes : int);
    procedure Read_UByte (File_ID : Byte_IO.File_Type; theByte : out Ubyte);

    IO_Exception : exception;

end IO;
