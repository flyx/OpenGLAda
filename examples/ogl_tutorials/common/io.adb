with Ada.Sequential_IO; Use Ada;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

with Interfaces.C;

with GL.Types; use GL.Types;

package body IO is

    type tCharacter2_Array is array (1 .. Ushort'size / 8) of character;
    type tCharacter4_Array is array (1 .. Integer'size / 8) of character;

    function To_GL_UByte is new
      Ada.Unchecked_Conversion (Character, UByte);
    function To_GL_Integer is new
      Ada.Unchecked_Conversion (Source => tCharacter4_Array,  Target => Int);
    function To_GL_Float is new
      Ada.Unchecked_Conversion (tCharacter4_Array, Single);
    function To_GL_Short is new
      Ada.Unchecked_Conversion (tCharacter2_Array, Ushort);
    function To_GL_Unsigned_Integer is new
      Ada.Unchecked_Conversion (Source => tCharacter4_Array,  Target => uint);

    --  ------------------------------------------------------------------------

    procedure read_string0 (File_ID    : Byte_IO.File_Type; theString : out unbounded_String;
                            Bytes_Read : out int) is
        theChar       : character;
        Finished      : Boolean := false;
    begin
        Bytes_Read := 0;
        theString := To_Unbounded_String ("");
        while not Finished loop
            IO.Byte_IO.Read (File_ID, theChar);
            Bytes_Read := Bytes_Read + 1;
            Finished := theChar = Character (Interfaces.C.nul);
            if not finished then
                theString := theString & theChar;
            end if;
        end loop;
        if bytes_read mod 2 /= 0 then -- read one more byte
            IO.seek (File_ID, 1);
            bytes_read := bytes_read + 1;
        end if;
    end read_string0;

    --  ------------------------------------------------------------------------

    procedure read_string (File_ID    : Byte_IO.File_Type; theString : out unbounded_String;
                           Bytes_Read : out int) is
        theChar                 : character;
        Finished                : Boolean := false;
    begin
        Bytes_Read := 0;
        theString := To_Unbounded_String ("");
        while not Finished loop
            IO.Byte_IO.Read (File_ID, theChar);
            Bytes_Read := Bytes_Read + 1;
            Finished := theChar = Character (Interfaces.C.nul);
            if not finished then
                theString := theString & theChar;
            end if;
        end loop;

        --  if length of string (including \0) is odd skip another byte
        if  Integer (Bytes_Read) mod 2 /= 0 then
            IO.Byte_IO.Read (File_ID, theChar);
            Bytes_Read := Bytes_Read + 1;
        end if;
    end read_string;

    --  ------------------------------------------------------------------------

    procedure Read_String (File_ID    : Byte_IO.File_Type; Length : Positive;
                           theString  : out String) is
        New_String : String (1 .. Length);
    begin
        for Count in 1 .. Length loop
            IO.Byte_IO.Read (File_ID, New_String (Count));
        end loop;
        theString := New_String;
    end Read_String;

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
