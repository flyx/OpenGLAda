with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Tokenization is
   type Tokenizer (<>) is limited private;

   type Symbol_Id is new Positive;

   type Token_Kind is (Identifier, Numeric_Literal, String_Literal,
      Delimiter, Comment, Stream_End);

   type Token (Length : Natural; Kind : Token_Kind) is limited record
      Content : String (1 .. Length);
      case Kind is
      when Identifier =>
         Id : Symbol_Id;
      when others => null;
      end case;
   end record;

   Tokenization_Error : exception;

   function Tokenize (File_Path : String) return Tokenizer;

   function Next (Object : in out Tokenizer) return Token;

   function Paren_Depth (Object : Tokenizer) return Natural;

   function Line (Object : Tokenizer) return Positive;
   function Column (Object : Tokenizer) return Positive;

   function To_String (T : Token) return String;

   function Is_Keyword (Id : Symbol_Id) return Boolean;

   function Copy (T : Token) return Token;

   procedure Register_Symbol (Object : in out Tokenizer; Symbol : String;
                              New_Id : out Symbol_Id);

   Keyword_Access    : constant Symbol_Id := 1;
   Keyword_Constant  : constant Symbol_Id := 2;
   Keyword_Dynamic   : constant Symbol_Id := 3;
   Keyword_End       : constant Symbol_Id := 4;
   Keyword_Function  : constant Symbol_Id := 5;
   Keyword_In        : constant Symbol_Id := 6;
   Keyword_Is        : constant Symbol_Id := 7;
   Keyword_Out       : constant Symbol_Id := 8;
   Keyword_Package   : constant Symbol_Id := 9;
   Keyword_Pragma    : constant Symbol_Id := 10;
   Keyword_Procedure : constant Symbol_Id := 11;
   Keyword_Return    : constant Symbol_Id := 12;
   Keyword_Spec      : constant Symbol_Id := 13;
   Keyword_Static    : constant Symbol_Id := 14;
   Keyword_Subtype   : constant Symbol_Id := 15;
   Keyword_Type      : constant Symbol_Id := 16;
   Keyword_Use       : constant Symbol_Id := 17;
   Keyword_With      : constant Symbol_Id := 18;
   Keyword_Wrapper   : constant Symbol_Id := 19;
private
   function Case_Insensitive_Hash (S : String) return Ada.Containers.Hash_Type;
   function Case_Insensitive_Equals (Left, Right : String) return Boolean;

   package String_Indexer is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Symbol_Id, Case_Insensitive_Hash, Case_Insensitive_Equals);

   type Tokenizer (Length : Positive) is record
      Input : String (1 .. Length);
      Pos : Positive;
      Symbol_Table : String_Indexer.Map;
      Depth : Natural;
      Cur_Line, Cur_Column, Prev_Line, Prev_Column : Positive;
   end record;

end Tokenization;