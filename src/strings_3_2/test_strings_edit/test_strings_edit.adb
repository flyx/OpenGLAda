--                                                                    --
--  procedure Test_Strings_Edit     Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Strings_Edit;                 use Strings_Edit;
with Strings_Edit.Fields;          use Strings_Edit.Fields;
with Strings_Edit.UTF8;            use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Blocks;     use Strings_Edit.UTF8.Blocks;
with Strings_Edit.UTF8.Handling;   use Strings_Edit.UTF8.Handling;
with Strings_Edit.UTF8.Mapping;    use Strings_Edit.UTF8.Mapping;
with Strings_Edit.UTF8.Maps;       use Strings_Edit.UTF8.Maps;
with Strings_Edit.UTF8.Wildcards;  use Strings_Edit.UTF8.Wildcards;
with Strings_Edit.Roman_Edit;      use Strings_Edit.Roman_Edit;

with Strings_Edit.Integers.Superscript;
with Strings_Edit.Integers.Subscript;
with Strings_Edit.Integer_Edit;
with Strings_Edit.Float_Edit;
with Strings_Edit.UTF8.Maps.Constants;

with Strings_Edit.UTF8.Categorization;
use  Strings_Edit.UTF8.Categorization;

with Strings_Edit.Lexicographical_Order;
use  Strings_Edit.Lexicographical_Order;

procedure Test_Strings_Edit is
   package Float_Edit is new Strings_Edit.Float_Edit (Float);
   package Integer_Edit is new Strings_Edit.Integer_Edit (Integer);
   use Float_Edit;
   use Integer_Edit;

   function Image (Set : Unicode_Set) return String is
      Ranges : constant Code_Points_Ranges := To_Ranges (Set);
      Result : Unbounded_String;
   begin
      if Ranges'Length = 0 then
         return "{Ø}";
      else
         Append (Result, "{");
         for Index in Ranges'Range loop
            if Length (Result) > 1 then
               Append (Result, ", ");
            end if;
            Append
            (  Result,
               (  Image (Integer (Ranges (Index).Low), 16)
               &  ".."
               &  Image (Integer (Ranges (Index).High), 16)
            )  );
         end loop;
         Append (Result, "}");
         return To_String (Result);
      end if;
   end Image;

   procedure Put (Text : String; Pointer : Integer) is
      Line  : String (1..80);
      Index : Integer := Line'First;
   begin
      Put (Text);
      New_Line;
      Put
      (  Line,
         Index,
         "",
         Field => Pointer - Text'First,
         Justify => Right
      );
      Put (Line, Index, "^ Pointer ");
      Integer_Edit.Put (Line, Index, Pointer);
      Put (Line (Line'First..Index - 1));
      New_Line;
   end Put;

   procedure Test_Justify
             (  Value   : String;
                Field   : Natural;
                Justify : Alignment;
                Result  : String
             )  is
      Text    : String (1..68);
      Pointer : Integer;
   begin
      Pointer := 1;
      Put (Text, Pointer, Value, Field => Field, Justify => Justify);
      if Text (1..Pointer-1) /= Result then
         raise Data_Error;
      end if;
   end Test_Justify;

   procedure Test_Float (Value : Float; Base : NumberBase := 10) is
      Eps     : constant :=
         Float'Compose (1.0, 1 - Float'Machine_Mantissa) * 7.0;
      Text    : String (1..68);
      Temp    : Float;
      Diff    : Float;
      Pointer : Integer;
   begin
      Pointer := 1;
      Put (Text, Pointer, Value, Base => Base);
      Temp :=
         Float_Edit.Value
         (  Text (Text'First..Pointer - 1),
            Base    => Base,
            ToFirst => True,
            ToLast  => True
         );
      if Temp /= Value then
         Diff := abs ((Temp - Value) / Temp);
         if Diff > Eps then
            Put
            (  "Base:" & NumberBase'Image (Base)
            &  " Diff:" & Float'Image (Diff)
            &  ">" & Float'Image (Eps)
            );
            New_Line;
            Put (Text (Text'First..Pointer - 1));
            New_Line;
            Text (Pointer..Text'Last) := (Pointer..Text'Last => ' ');
            Put (Float_Edit.Image (Temp, Base => Base));
            New_Line;
            raise Data_Error;
         end if;
      end if;
   end Test_Float;

   procedure Test_Rel_Precision
             (  Value  : Float;
                Small  : Integer;
                Result : String
             )  is
      Text    : String (1..68);
      Pointer : Integer;
   begin
      Pointer := 1;
      Put (Text, Pointer, Value, RelSmall => Small);
      if Text (1..Pointer-1) /= Result then
         raise Data_Error;
      end if;
   end Test_Rel_Precision;

   procedure Test_Abs_Precision
             (  Value  : Float;
                Small  : Integer;
                Result : String
             )  is
      Text    : String (1..68);
      Pointer : Integer;
   begin
      Pointer := 1;
      Put (Text, Pointer, Value, AbsSmall => Small);
      if Text (1..Pointer-1) /= Result then
         raise Data_Error;
      end if;
   end Test_Abs_Precision;

   procedure Test_Integer (Value : Integer; Base : NumberBase := 10) is
      Text    : String (1..68);
      Temp    : Integer;
      Pointer : Integer;
   begin
      Pointer := 1;
      Put (Text, Pointer, Value, Base => Base);
      Temp :=
         Integer_Edit.Value
         (  Text (Text'First..Pointer - 1),
            Base => Base
         );
      if (  Temp /= Value
         or else
            (  Integer_Edit.Image (Temp, Base => Base)
            /= Text (Text'First..Pointer - 1)
         )  )
      then
         Put (Text (Text'First..Pointer - 1));
         New_Line;
         Text (Pointer..Text'Last) := (Pointer..Text'Last => ' ');
         Pointer := 1;
         Get (Text, Pointer, Temp, Base => Base);
         Put (Text, Pointer);
	 raise Data_Error;
      end if;
   end Test_Integer;

   procedure Put_UTF8 (Text : String) is
      Line  : String (1..180);
      Index : Integer := Line'First;
   begin
      for I in Text'Range loop
         Put (Line, Index, Integer'Image (Character'Pos (Text (I))));
      end loop;
      Put_Line (Line (1..Index - 1));
   end Put_UTF8;

   procedure Test_Match
             (  Text, Pattern : String;
                Wide, Result  : Boolean
             )  is
      use Strings_Edit.UTF8.Maps.Constants;
   begin
      if Match (Text, Pattern, Lower_Case_Map, Wide) /= Result then
         if Wide then
            Put_Line
            (  "[match wide] '" & Text & "'"
            &  " vs "
            &  "'" & Pattern & "'"
            &  " = "
            &  Boolean'Image
               (  Match (Text, Pattern, Lower_Case_Map, Wide)
               )
            &  ", expected "
            &  Boolean'Image (Result)
            );
         else
            Put_Line
            (  "[match] '" & Text & "'"
            &  " vs "
            &  "'" & Pattern & "'"
            &  " = "
            &  Boolean'Image
               (  Match (Text, Pattern, Lower_Case_Map, Wide)
               )
            &  ", expected "
            &  Boolean'Image (Result)
            );
         end if;
         raise Data_Error;
      end if;
   end Test_Match;

   procedure Test_Match_Sensitive
             (  Text, Pattern : String;
                Wide, Result  : Boolean
             )  is
   begin
      if Match (Text, Pattern, Wide) /= Result then
         if Wide then
            Put_Line
            (  "[match wide] '" & Text & "'"
            &  " vs "
            &  "'" & Pattern & "'"
            &  " = "
            &  Boolean'Image (Match (Text, Pattern, Wide))
            &  ", expected "
            &  Boolean'Image (Result)
            );
         else
            Put_Line
            (  "[match] '" & Text & "'"
            &  " vs "
            &  "'" & Pattern & "'"
            &  " = "
            &  Boolean'Image (Match (Text, Pattern, Wide))
            &  ", expected "
            &  Boolean'Image (Result)
            );
         end if;
         raise Data_Error;
      end if;
   end Test_Match_Sensitive;

   procedure Test_Compare
             (  Left, Right       : String;
                As_Text           : Precedence;
                Lexicographically : Precedence
             )  is
   begin
      if Compare_Textually (Left, Right) /= As_Text then
         Put_Line
         (  "[as texts] '" & Left & "'"
         &  " vs "
         &  "'" & Right & "'"
         &  " = "
         &  Precedence'Image (Compare_Textually (Left, Right))
         &  ", expected "
         &  Precedence'Image (As_Text)
         );
         raise Data_Error;
      elsif Compare_Lexicographically (Left, Right) /= Lexicographically
      then
         Put_Line
         (  "[lexicographically] '" & Left & "'"
         &  " vs "
         &  "'" & Right & "'"
         &  " = "
         &  Precedence'Image (Compare_Lexicographically (Left, Right))
         &  ", expected "
         &  Precedence'Image (Lexicographically)
         );
         raise Data_Error;
      end if;
   end Test_Compare;

   procedure Test_UTF8_Superscript_Integer
             (  Value : Integer;
                Plus  : Boolean := False
             )  is
      use Integers;
      Text    : String (1..256);
      Temp    : Integer;
      Pointer : Integer;
   begin
      Pointer := 1;
      Superscript.Put (Text, Pointer, Value, PutPlus=>Plus);
      Temp := Superscript.Value (Text (Text'First..Pointer - 1));
      if (  Temp /= Value
         or else
            (  Superscript.Image (Temp, PutPlus=>Plus)
            /= Text (Text'First..Pointer - 1)
         )  )
      then
         Put_Line ("Number: " & Integers.Image (Value));
         Put_UTF8 (Text (Text'First..Pointer - 1));
         Put_Line ("Got: " & Integers.Image (Temp));
	 raise Data_Error;
      end if;
   end Test_UTF8_Superscript_Integer;

   procedure Test_UTF8_Subscript_Integer
             (  Value : Integer;
                Plus  : Boolean := False
             )  is
      use Integers;
      Text    : String (1..256);
      Temp    : Integer;
      Pointer : Integer;
   begin
      Pointer := 1;
      Subscript.Put (Text, Pointer, Value, PutPlus=>Plus);
      Temp := Subscript.Value (Text (Text'First..Pointer - 1));
      if (  Temp /= Value
         or else
            (  Subscript.Image (Temp, PutPlus=>Plus)
            /= Text (Text'First..Pointer - 1)
         )  )
      then
         Put_Line ("Number: " & Integers.Image (Value));
         Put_UTF8 (Text (Text'First..Pointer - 1));
         Put_Line ("Got: " & Integers.Image (Temp));
	 raise Data_Error;
      end if;
   end Test_UTF8_Subscript_Integer;

   procedure TestFields
             (  Destination : String;
                Value       : String;
                Field       : Natural;
                Justify     : Alignment;
                Fill        : Character;
                Expected    : String
             )  is
      procedure Put_String
                (  Destination : in out String;
                   Pointer     : in out Integer;
                   Value       : String;
                   Field       : Natural := 0;
                   Justify     : Alignment := Left;
                   Fill        : Character := ' '
                )  is
         Out_Field : constant Natural :=
                        Get_Output_Field (Destination, Pointer, Field);
         subtype Output is String (Pointer..Pointer + Out_Field - 1);
         Text  : Output renames
                    Destination (Pointer..Pointer + Out_Field - 1);
         Index : Integer := Pointer;
      begin
         Put (Text, Index, Value);
         Adjust_Output_Field
         (  Destination,
            Pointer,
            Index,
            Out_Field,
            Field,
            Justify,
            Fill
         );
      end Put_String;
      Text    : String  := Destination;
      Pointer : Integer := Text'First;
   begin
      Put_String (Text, Pointer, Value, Field, Justify, Fill);
      if Text /= Expected then
	   Raise_Exception
         (  Data_Error'Identity,
            "Got '" & Text & "', expected '" & Expected & "'"
         );
      end if;
   end TestFields;

   type Char_Positions is array (Positive range <>) of Integer;

   function By_Pos (S : Char_Positions) return String is
      R : String (S'Range);
   begin
      for I in S'Range loop
         R (I) := Character'Val (S (I));
      end loop;
      return R;
   end By_Pos;

   Text    : String (1..68);
   Pointer : Integer;
begin
   Put_Line ("Testing strings edit ...");

   for I in Character'Range loop
      if (  To_String (To_UTF8 (I)) /= (1=>I)
         or else
            To_String (To_UTF8 (String'(1=>I))) /= (1=>I)
         )
      then
	   Raise_Exception
         (  Data_Error'Identity,
            "UTF-8 <-> Latin-1 conversion error"
         );
      end if;
   end loop;

   for I in Wide_Character'Range loop
      if (  To_Wide_String (To_UTF8 (I)) /= (1=>I)
         or else
            To_Wide_String (To_UTF8 (Wide_String'(1=>I))) /= (1=>I)
         )
      then
	   Raise_Exception
         (  Data_Error'Identity,
            "UTF-8 <-> UCS-2 conversion error"
         );
      end if;
   end loop;

   for I in Strings_Edit.UTF8.UTF8_Code_Point'Range loop
      declare
         Is_Lower : constant Boolean := Is_Lowercase (I);
         Is_Upper : constant Boolean := Is_Uppercase (I);
         Upper    : constant UTF8_Code_Point := To_Uppercase (I);
         Lower    : constant UTF8_Code_Point := To_Lowercase (I);
      begin
         if (  (Upper /= I and then Is_Upper)
            or else
               (Lower /= I and then Is_Lower)
            )
         then
	    Raise_Exception
            (  Data_Error'Identity,
               (  "UTF-8 Case conversion of"
               &  Integer'Image (Integer (I))
               &  " /"
               &  Integer'Image (Integer (Upper))
               &  " \"
               &  Integer'Image (Integer (Lower))
            )  );
         end if;
         case Category (I) is
            when Ll =>
               if not Is_Lower or else Lower /= I then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "UTF-8 categorization error of"
                     &  Integer'Image (Integer (I))
                     &  " /"
                     &  Integer'Image (Integer (Upper))
                     &  " \"
                     &  Integer'Image (Integer (Lower))
                     &  ", is not a lower case letter: "
                     &  General_Category'Image (Category (I))
                  )  );
               end if;
            when Lu =>
               if not Is_Upper or else Upper /= I then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "UTF-8 categorization error of"
                     &  Integer'Image (Integer (I))
                     &  " /"
                     &  Integer'Image (Integer (Upper))
                     &  " \"
                     &  Integer'Image (Integer (Lower))
                     &  ", is not an upper case letter: "
                     &  General_Category'Image (Category (I))
                  )  );
               end if;
            when others =>
               null;
         end case;
      end;
   end loop;

   declare
      Buffer     : String (1..4);
      That, This : UTF8_Code_Point;
      I1, I2     : Integer;
   begin
      for I in UTF8_Code_Point'Range loop
         I1   := Buffer'First;
         I2   := Buffer'First;
         This := I;
         Put (Buffer, I1, This);
         Get (Buffer, I2, That);
         if I1 /= I2 or This /= That then
	      Raise_Exception
            (  Use_Error'Identity,
               "UTF-8 I/O error"
            );
         end if;
      end loop;
   exception
      when Data_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Error converting" & UTF8_Code_Point'Image (This)
         );
   end;

   declare
      procedure Show (Text : String; Code : UTF8_Code_Point) is
         UTF : constant String := Image (Code);
      begin
         Put (Text & ":" & UTF8_Code_Point'Image (Code) & " =");
         for I in UTF'Range loop
            Put (Integer'Image (Character'Pos (UTF (I))));
         end loop;
         New_Line;
      end Show;
   begin
      Put_Line ("Some UTF-8 code positions");
      Show ("DEGREE CELSIUS",    16#2103#);
      Show ("DEGREE FAHRENHEIT", 16#2109#);
      Show ("OHM SIGN",          16#2126#);
      Show ("KELVIN SIGN",       16#212A#);
      Show ("ANGSTROM SIGN",     16#212B#);
      Show ("SUPERSCRIPT FOUR",  16#2074#);
      Show ("SUPERSCRIPT FIVE",  16#2075#);
      Show ("SUPERSCRIPT SIX",   16#2076#);
      Show ("SUPERSCRIPT SEVEN", 16#2077#);
      Show ("SUPERSCRIPT EIGHT", 16#2078#);
      Show ("SUPERSCRIPT NINE",  16#2079#);
      Show ("SUPERSCRIPT PLUS",  16#207A#);
      Show ("SUPERSCRIPT MINUS", 16#207B#);
      Show ("SUBSCRIPT ZERO",    16#2080#);
      Show ("SUBSCRIPT ONE",     16#2081#);
      Show ("SUBSCRIPT TWO",     16#2082#);
      Show ("SUBSCRIPT THREE",   16#2083#);
      Show ("SUBSCRIPT FOUR",    16#2084#);
      Show ("SUBSCRIPT FIVE",    16#2085#);
      Show ("SUBSCRIPT SIX",     16#2086#);
      Show ("SUBSCRIPT SEVEN",   16#2087#);
      Show ("SUBSCRIPT EIGHT",   16#2088#);
      Show ("SUBSCRIPT NINE",    16#2089#);
      Show ("SUBSCRIPT PLUS",    16#208A#);
      Show ("SUBSCRIPT MINUS",   16#208B#);
      declare
         Test  : constant String      := "kg·m²/A²·s³";
         UTF   : constant String      := To_UTF8 (Test);
         Wide  : constant Wide_String := To_Wide_String (UTF);
      begin
         if To_UTF8 (Wide) /= UTF then
            Raise_Exception
            (  Data_Error'Identity,
               "Error converting to wide-string"
            );
         end if;
      end;
   end;

   Test_UTF8_Superscript_Integer (-6);
   Test_UTF8_Superscript_Integer (-987);
   Test_UTF8_Superscript_Integer (0);
   Test_UTF8_Superscript_Integer (12345);

   Test_UTF8_Superscript_Integer (-6,    True);
   Test_UTF8_Superscript_Integer (-987,  True);
   Test_UTF8_Superscript_Integer (0,     True);
   Test_UTF8_Superscript_Integer (12345, True);

   Test_UTF8_Subscript_Integer (-6);
   Test_UTF8_Subscript_Integer (-987);
   Test_UTF8_Subscript_Integer (0);
   Test_UTF8_Subscript_Integer (12345);

   Test_UTF8_Subscript_Integer (-6,    True);
   Test_UTF8_Subscript_Integer (-987,  True);
   Test_UTF8_Subscript_Integer (0,     True);
   Test_UTF8_Subscript_Integer (12345, True);

   Test_Justify ("#", 10, Left,   "#         ");
   Test_Justify ("#", 10, Right,  "         #");
   Test_Justify ("#", 10, Center, "    #     ");

   Test_Justify ("##########", 10, Left,   "##########");
   Test_Justify ("##########", 10, Right,  "##########");
   Test_Justify ("##########", 10, Center, "##########");

   Test_Rel_Precision (2.345678, 1, "2");
   Test_Rel_Precision (2.345678, 2, "2.3");
   Test_Rel_Precision (2.345678, 3, "2.35");
   Test_Rel_Precision (2.345678, 4, "2.346");
   Test_Rel_Precision (2.345678, 5, "2.3457");

   Test_Abs_Precision (2.345678, 1, "0");
   Test_Abs_Precision (2.345678, 0, "2");
   Test_Abs_Precision (2.345678,-1, "2.3");
   Test_Abs_Precision (2.345678,-2, "2.35");
   Test_Abs_Precision (2.345678,-3, "2.346");
   Test_Abs_Precision (2.345678,-4, "2.3457");

   for Base in NumberBase'Range loop
      Test_Float (Float'Last,  Base => Base);
      Test_Float (Float'First, Base => Base);
   end loop;

   Test_Float (1.2e37, Base => 2);
   Test_Float (1.2e37, Base => 16);
   Test_Float (1.2e37);
   Test_Float (123.45);
   Test_Float (0.00000012345);
   Test_Float (0.000000000000012345);

   for Number in Roman'Range loop
      Pointer := 1;
      Put (Text, Pointer, Number);
      if (  Roman_Edit.Value (Text (Text'First..Pointer - 1))
         /= Number
         )
      then
         Put
         (  Text (Text'First..Pointer - 1)
         &  "/="
         &  Roman_Edit.Image (Number)
         );
         New_Line;
	 raise Data_Error;
      end if;
   end loop;

   for Base in NumberBase'Range loop
      Test_Integer (Integer'Last, Base => Base);
      Test_Integer (Integer'First, Base => Base);
   end loop;

   Test_Integer (0);
   Test_Integer (-6);
   Test_Integer (12345);
   Test_Integer (Integer'First);
   Test_Integer (Integer'Last);

   begin
      if 0.0 = Value ("e+15") then null; end if;
      raise Data_Error;
   exception
      when End_Error => null;
   end;
   begin
      if 0 = Integer'(Value ("+")) then null; end if;
      raise Data_Error;
   exception
      when Data_Error => null;
   end;
   begin
      if 0.0 = Value ("+") then null; end if;
      raise Data_Error;
   exception
      when Data_Error => null;
   end;
   begin
      if 0.0 = Value (" + .") then null; end if;
      raise Data_Error;
   exception
      when Data_Error => null;
   end;
   begin
      if 0.0 = Value ("  xx") then null; end if;
      raise Data_Error;
   exception
      when End_Error => null;
   end;
   begin
      Pointer := 0;
      Put (Text, Pointer, "");
      raise Data_Error;
   exception
      when Layout_Error => null;
   end;
   begin
      Pointer := Text'Last;
      Put (Text, Pointer, "AA");
      raise Data_Error;
   exception
      when Layout_Error => null;
   end;
   Pointer := Text'Last;
   Put (Text, Pointer, "A");
   if Pointer /= Text'Last + 1 or Text (Text'Last) /= 'A' then
      raise Data_Error;
   end if;
   Pointer := Text'Last + 1;
   Put (Text, Pointer, "");
   if Pointer /= Text'Last + 1 then
      raise Data_Error;
   end if;
   TestFields ("xxx",  "a", 0, Right,  ' ', "axx" );
   TestFields ("xxx",  "",  2, Right,  ' ', "  x" );
   TestFields ("xxx",  "s", 3, Right,  'h', "hhs" );
   TestFields ("xxxx", "t", 3, Center, ' ', " t x");
   TestFields ("xxxx", "t", 3, Left,   ' ', "t  x");
   TestFields ("xxxx", "t", 1, Left,   ' ', "txxx");
   begin
      TestFields ("xxxx", "zzzz", 3, Left,   ' ', "t  x");
      raise Data_Error;
   exception
      when Layout_Error => null;
   end;
   declare
      Text  : constant String :=
                 (  "123456789012345678901234567890"
                 &  "123456789012345678901234567890"
                 &  "123456789012345678901234567890"
                 &  "123456789012345678901234567890"
                 &  "123456789012345678901234567890"
                 &  "123456789012345678901234567890"
                 &  "E99"
                 );
      Value : Float;
   begin
      Pointer := Text'First;
      Get (Text, Pointer, Value, ToLast => True);
      if Value /= Float'Last then
         raise Constraint_Error;
      end if;
   end;
   declare
      Text  : constant String :=
                 (  "1E"
                 &  "123456789012345678901234567890"
                 &  "123456789012345678901234567890"
                 &  "123456789012345678901234567890"
                 &  "123456789012345678901234567890"
                 &  "123456789012345678901234567890"
                 &  "123456789012345678901234567890"
                 );
      Value : Float;
   begin
      Pointer := Text'First;
      Get (Text, Pointer, Value, ToLast => True);
      if Value /= Float'Last then
         raise Constraint_Error;
      end if;
   end;

   declare
      Latin : constant Unicode_Set := To_Set (Basic_Latin);
      Hex   : constant Unicode_Set := To_Set ("0123456789ABCDEF");
      Latin_Letters    : constant Unicode_Set :=
                         Choose (Latin, Is_Letter'Access);
      All_Letters      : constant Unicode_Set :=
                         Choose (Universal_Set, Is_Letter'Access);
      Cyrillic_Letters : constant Unicode_Set :=
                         To_Set (Is_Letter'Access) and Cyrillic;
   begin
      if Is_In
         (  Value ("T"),
            To_Set (16#09#) or Strings_Edit.UTF8.Maps.Constants.Space_Set
         )
      then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set Is_In (T,"
            &  Image
               (  To_Set (16#09#)
               or Strings_Edit.UTF8.Maps.Constants.Space_Set
         )  )  );
      end if;
      if (  Cyrillic_Letters
         /= Choose (To_Set (Cyrillic), Is_Letter'Access)
         )
      then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set "
            &  Image (Cyrillic_Letters)
            &  " /= "
            &  Image (Choose (To_Set (Cyrillic), Is_Letter'Access))
         )  );
      end if;
      if (To_Set ("ABCEF") or "D") /= "ABCDEF" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set not "
            &  Image (To_Set ("ABCEF") or "D")
            &  " /= "
            &  Image (To_Set ("ABCDEF"))
         )  );
      end if;
      if To_Set ("ABCDGFH") /= To_Set ("FGHABCD") then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set "
            &  Image (To_Set ("FGHABCD"))
            &  " /= "
            &  Image (To_Set ("ABCDGFH"))
         )  );
      end if;
      if (To_Set (65, 68) or To_Set (70, 72)) /= To_Set ("ABCDGFH") then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set "
            &  Image (To_Set (65, 68))
            &  " or "
            &  Image (To_Set (70, 72))
            &  " /= "
            &  Image (To_Set ("ABCDGFH"))
         )  );
      end if;
      if Latin /= not (not Latin) then
         Raise_Exception
         (  Data_Error'Identity,
            "Error in Unicode_Set 'not'"
         );
      end if;
      if To_Set (Code_Points_Range'(97, 97)) /= To_Set ("a") then
         Raise_Exception
         (  Data_Error'Identity,
            "Error in Unicode_Set To_Set"
         );
      end if;
      if Latin /= (Latin or "a") then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set 'or' "
            &  Image (Latin)
            &  " /= "
            &  Image (Latin or "a")
         )  );
      end if;
      if not (  Is_In (Character'('a'), Latin)
             and then
                Is_In (Wide_Character'('a'), Latin)
             and then
                Is_In (Wide_Character'('A'), Hex)
             and then
                not Is_In (Wide_Character'('a'), Hex)
             )
      then
         Raise_Exception
         (  Data_Error'Identity,
            "Error in Unicode_Set Is_In"
         );
      end if;
      if not Is_Subset ("abcd0a", Latin) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set not Is_Subset "
            &  Image (To_Set ("abcd0a"))
            &  ", "
            &  Image (Latin)
         )  );
      end if;
      if not Is_Subset (Latin, Latin) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set not Is_Subset "
            &  Image (Latin)
            &  ", "
            &  Image (Latin)
         )  );
      end if;
      if not Is_Subset (Basic_Latin, Latin) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set not Is_Subset "
            &  Image (To_Set (Basic_Latin))
            &  ", "
            &  Image (Latin)
         )  );
      end if;
      if not Hex > "AF" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set not "
            &  Image (Hex)
            &  " > "
            &  Image (To_Set ("AF"))
         )  );
      end if;
      if not Hex > "AF12345666" then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set not "
            &  Image (Hex)
            &  " > "
            &  Image (To_Set ("AF12345666"))
         )  );
      end if;
      if not Hex >= Hex then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set not "
            &  Image (Hex)
            &  " >= "
            &  Image (Hex)
         )  );
      end if;
      if Hex > Hex then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set "
            &  Image (Hex)
            &  " > "
            &  Image (Hex)
         )  );
      end if;
      if Choose (Hex, Is_Letter'Access) /= To_Set ("ABCDEF") then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set Choose "
            &  Image (Choose (Hex, Is_Letter'Access))
            &  " /= "
            &  Image (Hex)
         )  );
      end if;
      if Choose (Hex, Is_Letter'Access) /= To_Set ("ABCDEF") then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set Choose "
            &  Image (Choose (Hex, Is_Letter'Access))
            &  " /= "
            &  Image (Hex)
         )  );
      end if;
      if Latin_Letters /= (To_Set (65, 90) or To_Set (97, 122)) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set Choose "
            &  Image (Latin_Letters)
            &  " /= "
            &  Image (To_Set (65, 90) or To_Set (97, 122))
         )  );
      end if;
      if not (not Latin_Letters) /= Latin_Letters then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set "
            &  Image (not (not Latin_Letters))
            &  " /= "
            &  Image (Latin_Letters)
         )  );
      end if;
      if (not Latin_Letters) < To_Set (1, 20) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set not "
            &  Image (not Latin_Letters)
            &  " < "
            &  Image (To_Set (1, 20))
         )  );
      end if;
      if (Null_Set or "ABC") /= To_Set ("ABC") then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set "
            &  Image (Null_Set or "ABC")
            &  " /= "
            &  Image (To_Set ("ABC"))
         )  );
      end if;
      if ((Null_Set or "ABC") or "EF") /= To_Set ("ABCEF") then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set "
            &  Image ((Null_Set or "ABC") or "EF")
            &  " /= "
            &  Image (To_Set ("ABCEF"))
         )  );
      end if;
      if (not (Universal_Set - "A")) /= To_Set ("A") then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set "
            &  Image (not (Universal_Set - "A"))
            &  " /= "
            &  Image (To_Set ("A"))
         )  );
      end if;
      if (  (Universal_Set - "AFG" or "AB" or "F")
         /= Universal_Set - "G"
         )
      then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error in Unicode_Set "
            &  Image (Universal_Set - "AFG" or "AB" or "F")
            &  " /= "
            &  Image (Universal_Set - "G")
         )  );
      end if;
      if (  Value (To_Mapping ("ABCDEFG", "abcdefg"), Character'('B'))
         /= Character'Pos ('b')
         )
      then
         Raise_Exception
         (  Data_Error'Identity,
            "Error in Unicode_Mapping"
         );
      end if;
      if (  Value (To_Mapping ("ABCDEFG", "abcdefg"), Character'('1'))
         /= Character'Pos ('1')
         )
      then
         Raise_Exception
         (  Data_Error'Identity,
            "Error in Unicode_Mapping"
         );
      end if;

      Put_Line
      (  "Unicode letters "
      &  Image (Cardinality (All_Letters))
      &  " in"
      &  Integer'Image (To_Ranges (All_Letters)'Length)
      &  " ranges"
      );
      Put_Line
      (  "Cyrillic letters "
      &  Image (Cardinality (Cyrillic_Letters))
      &  " in"
      &  Integer'Image (To_Ranges (Cyrillic_Letters)'Length)
      &  " ranges"
      );
   end;
   Test_Compare ("abc-123.txt", "abc-0.txt", Equal, Greater);
   Test_Compare ("abc-123-d", "abc-d", Greater, Greater);
   Test_Compare ("123y", "0123y", Equal, Equal);
   Test_Compare ("123y", "0113y", Equal, Greater);
   Test_Compare ("123y", "0114y", Equal, Greater);
   Test_Compare ("abc", "ab1", Less, Less);
   Test_Compare ("ab44", "ab0123", Equal, Less);

   Test_Match ("AbC", "abc", True, True);
   Test_Match ("AbC", "a*c", True, True);
   Test_Match ("Ab   C", "aB c", True, True);
   Test_Match ("Ab   C", "aB c", False, False);

   Test_Match_Sensitive ("AbC", "AbC", True, True);
   Test_Match_Sensitive ("AbC", "A*C", True, True);
   Test_Match_Sensitive ("Ab   C", "Ab C", True, True);
   Test_Match_Sensitive ("Ab   C", "Ab C", False, False);

   Put_Line ("... Done");
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Test_Strings_Edit;
