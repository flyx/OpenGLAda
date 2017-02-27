with Ada.Characters.Handling;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Strings.Maps;

package body Tokenization is
   function Case_Insensitive_Hash (S : String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Ada.Characters.Handling.To_Lower (S));
   end Case_Insensitive_Hash;

   function Case_Insensitive_Equals (Left, Right : String) return Boolean is
      use Ada.Characters.Handling;
   begin
      return To_Lower (Left) = To_Lower (Right);
   end Case_Insensitive_Equals;

   use Ada.Strings.Maps;

   Inline_Separators : constant Character_Set :=
      To_Set (Character'Val (9)) or To_Set (' ');

   Newlines : constant Character_Set :=
      To_Set (Character'Val (10)) or To_Set (Character'Val (13));

   Separators : constant Character_Set :=
      Inline_Separators or Newlines;

   Single_Delimiters : constant Character_Set :=
      To_Set ("&'()*+,-./:;<=>|");

   Compound_Delimiters : constant array(1 .. 10) of String (1 .. 2) :=
      ("=>", "..", "**", ":=", "/=", ">=", "<=", "<<", ">>", "<>");

   function Tokenize (File_Path : String) return Tokenizer is
      File_Size : constant Natural := Natural (Ada.Directories.Size (File_Path));
      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);
      File : File_String_IO.File_Type;
   begin
      File_String_IO.Open (File, File_String_IO.In_File, File_Path);
      return Ret : Tokenizer :=
        Tokenizer'(Length => File_Size, Pos => 1, Depth => 0, Cur_Line => 1,
                   Cur_Column => 1, others => <>) do
         File_String_IO.Read (File, Ret.Input);
         File_String_IO.Close (File);
         Ret.Symbol_Table.Insert ("access", Keyword_Access);
         Ret.Symbol_Table.Insert ("constant", Keyword_Constant);
         Ret.Symbol_Table.Insert ("dynamic", Keyword_Dynamic);
         Ret.Symbol_Table.Insert ("end", Keyword_End);
         Ret.Symbol_Table.Insert ("function", Keyword_Function);
         Ret.Symbol_Table.Insert ("in", Keyword_In);
         Ret.Symbol_Table.Insert ("is", Keyword_Is);
         Ret.Symbol_Table.Insert ("out", Keyword_Out);
         Ret.Symbol_Table.Insert ("package", Keyword_Package);
         Ret.Symbol_Table.Insert ("pragma", Keyword_Pragma);
         Ret.Symbol_Table.Insert ("procedure", Keyword_Procedure);
         Ret.Symbol_Table.Insert ("return", Keyword_Return);
         Ret.Symbol_Table.Insert ("spec", Keyword_Spec);
         Ret.Symbol_Table.Insert ("static", Keyword_Static);
         Ret.Symbol_Table.Insert ("subtype", Keyword_Subtype);
         Ret.Symbol_Table.Insert ("type", Keyword_Type);
         Ret.Symbol_Table.Insert ("use", Keyword_Use);
         Ret.Symbol_Table.Insert ("with", Keyword_With);
         Ret.Symbol_Table.Insert ("wrapper", Keyword_Wrapper);
      end return;
   end Tokenize;

   function Id (Object : in out Tokenizer; Symbol : String) return Symbol_Id is
      use String_Indexer;
      use type Ada.Containers.Count_Type;
      Position: Cursor;
      Inserted: Boolean;
   begin
      Object.Symbol_Table.Insert
        (Symbol, Symbol_Id (Object.Symbol_Table.Length + 1), Position, Inserted);
      return Element (Position);
   end Id;

   function Next (Object : in out Tokenizer) return Token is
      I : Positive := Object.Pos;
      Cur : Character;
      New_Line : Positive := Object.Cur_Line;
      New_Column : Positive := Object.Cur_Column;

      function Next_Char return Character is
      begin
         return Ret : Character := Object.Input (I) do
            I := I + 1;
            if Ret = Character'Val (10) then -- Linefeed
               New_Line := New_Line + 1;
               New_Column := 1;
            else
               New_Column := New_Column + 1;
            end if;
         end return;
      end Next_Char;

      procedure Update_Pos is
      begin
         Object.Pos := I;
         Object.Cur_Line := New_Line;
         Object.Cur_Column := New_Column;
      end Update_Pos;

      procedure Skip (Set : Character_Set) is
      begin
         while I <= Object.Input'Length loop
            exit when not Is_In (Object.Input (I), Set);
            Cur := Next_Char;
         end loop;
      end Skip;
   begin
      Skip (Separators);
      Update_Pos;
      Object.Prev_Line := Object.Cur_Line;
      Object.Prev_Column := Object.Cur_Column;

      if I > Object.Input'Length then
         return Token'(Kind => Stream_End, Length => 0, Content => "");
      end if;

      Cur := Next_Char;
      if Is_In (Cur, Single_Delimiters) then
         if I <= Object.Input'Length then
            declare
               Next : constant Character := Object.Input (I);
            begin
               if Cur = '-' and Next = '-' then
                  Cur := Next_Char; -- properly advance I/line/column
                  Skip (not Newlines);
                  return Ret : Token :=
                     Token'(Kind => Comment, Length => I - Object.Pos - 2,
                           Content => Object.Input (Object.Pos + 2 .. I - 1)) do
                     Update_Pos;
                  end return;
               else
                  declare
                     Possible_Compound : String := Cur & Next;
                  begin
                     for J in 1 .. 10 loop
                        if Compound_Delimiters (J) = Possible_Compound then
                           Cur := Next_Char; -- properly advace I/line/column
                           Update_Pos;
                           return Token'(Kind => Delimiter, Length => 2,
                                         Content => Possible_Compound);
                        end if;
                     end loop;
                     Update_Pos;
                     if Cur = '(' then
                        Object.Depth := Object.Depth + 1;
                     elsif Cur = ')' then
                        if Object.Depth = 0 then
                           raise Tokenization_Error with "Extra `)`!";
                        end if;
                        Object.Depth := Object.Depth - 1;
                     end if;
                     return Token'(Kind => Delimiter, Length => 1,
                                    Content => (1 => Cur));
                  end;
               end if;
            end;
         else
            Update_Pos;
            return Token'(Kind => Delimiter, Length => 1, Content => (1 => Cur));
         end if;
      elsif Cur = '"' then
         while I <= Object.Input'Length loop
            <<continue>>
            Cur := Next_Char;
            if Cur = '"' then
               if I <= Object.Input'Length then
                  if Object.Input (I) = '"' then
                     Cur := Next_Char; -- properly advance I/line/column
                     goto continue;
                  end if;
               end if;
               return Ret : Token := Token'(Kind => String_Literal,
                 Length => I - Object.Pos - 2,
                 Content => Object.Input (Object.Pos + 1 .. I - 2)) do
                  Update_Pos;
               end return;
            end if;
         end loop;
         raise Tokenization_Error with "Unclosed string literal";
      else
         Skip (not (Separators or Single_Delimiters));
         return Ret : Token := Token'(Kind => Identifier,
           Length => I - Object.Pos,
           Content => Object.Input (Object.Pos .. I - 1),
           Id => <>) do
            Ret.Id := Id (Object, Ret.Content);
            Update_Pos;
         end return;
      end if;
   end Next;

   function Paren_Depth (Object : Tokenizer) return Natural is
   begin
      return Object.Depth;
   end Paren_Depth;

   function Line (Object : Tokenizer) return Positive is (Object.Prev_Line);
   function Column (Object : Tokenizer) return Positive is (Object.Prev_Column);

   function To_String (T : Token) return String is
   begin
      if T.Kind = Identifier and then Is_Keyword (T.Id) then
         return "KEYWORD'(""" & T.Content & """)";
      else
         return T.Kind'Img & "'(""" & T.Content & """)";
      end if;
   end To_String;

   function Is_Keyword (Id : Symbol_Id) return Boolean is
     (Id < Keyword_Wrapper);

   function Copy (T : Token) return Token is
   begin
      case T.Kind is
      when Identifier =>
         return Token'(Kind => Identifier, Length => T.Length,
           Content => T.Content, Id => T.Id);
      when Numeric_Literal =>
         return Token'(Kind => Numeric_Literal, Length => T.Length,
                       Content => T.Content);
      when String_Literal =>
         return Token'(Kind => String_Literal, Length => T.Length,
                       Content => T.Content);
      when Delimiter =>
         return Token'(Kind => Delimiter, Length => T.Length,
                       Content => T.Content);
      when Comment =>
         return Token'(Kind => Comment, Length => T.Length, Content => T.Content);
      when Stream_End =>
         return Token'(Kind => Stream_End, Length => 0, Content => "");
      end case;
   end Copy;

   procedure Register_Symbol (Object : in out Tokenizer; Symbol : String;
                              New_Id : out Symbol_Id) is
   begin
      New_Id := Id (Object, Symbol);
   end Register_Symbol;
end Tokenization;