
with Ada.Sequential_IO; Use Ada;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;

with GL.Types; use GL.Types;

package IO is
    package Byte_IO is new Sequential_IO (Character);
    package Integer_IO is new Sequential_IO (Integer);

    procedure Seek (File_ID : Byte_IO.File_Type; Bytes : int);
    procedure Read_String (File_ID  : Byte_IO.File_Type; theString : out unbounded_String;
                           Bytes_Read : out int);
    procedure Read_String (File_ID    : Byte_IO.File_Type; Length : Positive;
                           theString  : out String);
    procedure Read_String0 (File_ID  : Byte_IO.File_Type; theString : out unbounded_String;
                            Bytes_Read : out int);
    procedure Read_UByte (File_ID : Byte_IO.File_Type; theByte : out Ubyte);

    IO_Exception : exception;

end IO;
