with Ada.Containers.Hashed_Maps;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with Tokenization;

package body Specs is
   use Ada.Text_IO;
   use Ada.Containers;
   use Ada.Strings.Unbounded;

   procedure Wrong_Token (Cur : Tokenization.Token; Message : String) is
   begin
      raise Parsing_Error with Message & ": " & Tokenization.To_String (Cur);
   end Wrong_Token;

   function Indexed_Name (Prefix : Character; Index : Positive) return String is
      Img : constant String := Index'Img;
   begin
      return Prefix & Img (Img'First + 1 .. Img'Last);
   end Indexed_Name;

   procedure Put_Signature (File : in out File_Type; Sig: Signature;
                            Real_Names : Boolean) is
      Index : Positive := 1;
      First : Boolean;
   begin
      if Param_Lists.Length (Sig.Params) > 0 then
         Put (File, " (");
         for Param of Sig.Params loop
            if Index > 1 then
               Put (File, "; ");
            end if;
            First := True;
            for Name of Param.Names loop
               if First then
                  First := False;
               else
                  Put (File, ", ");
               end if;
               if Real_Names then
                  Put (File, Name);
               else
                  Put (File, Indexed_Name ('P', Index));
               end if;
               Index := Index + 1;
            end loop;
            Put (File, " : ");
            case Param.Mode is
            when Mode_In => null;
            when Mode_Out => Put (File, "out ");
            when Mode_In_Out => Put (File, "in out ");
            when Mode_Access => Put (File, "access ");
            when Mode_Access_Constant => Put (File, "access constant ");
            end case;
            Put (File, To_String (Param.Type_Name));
         end loop;
         Put (File, ')');
      end if;
      if Length (Sig.Return_Type) > 0 then
         Put (File, " return " & To_String (Sig.Return_Type));
      end if;
   end Put_Signature;

   procedure Parse_File (Proc : in out Processor; Path : String) is
      use Tokenization;
      use Ada.Exceptions;

      function Symbol_Hash (S : Symbol_Id) return Ada.Containers.Hash_Type is
        (Ada.Containers.Hash_Type (S));

      package Symbol_To_Index is new Ada.Containers.Hashed_Maps
        (Tokenization.Symbol_Id, Positive, Symbol_Hash, Tokenization."=");
      Subroutine_Defs : Symbol_To_Index.Map;

      T : Tokenizer := Tokenize (Path);
      Data : Spec_Data := Spec_Data'(File_Base_Name => To_Unbounded_String
        (Ada.Directories.Base_Name (Path)), others => <>);

      function Get_Name (Start : Token; Name : out Unbounded_String)
        return Token is
         -- returns the first token after the name
         Recent_Was_Identifier : Boolean := True;
      begin
         if Start.Kind /= Identifier then
            Wrong_Token (Start, "Unexpected token (expected identifier)");
         end if;
         Name := To_Unbounded_String (Start.Content);
         loop
            declare
               Cur : constant Token := Next (T);
            begin
               if Recent_Was_Identifier then
                  if Cur.Kind /= Delimiter or Cur.Content /= "." then
                     return Copy (Cur);
                  end if;
               elsif Cur.Kind /= Identifier or else Is_Keyword (Cur.Id) then
                  Wrong_Token (Cur, "Unexpected token (expected identifier))");
               end if;
               Recent_Was_Identifier := not Recent_Was_Identifier;
               Name := Name & Cur.Content;
            end;
         end loop;
      end Get_Name;

      procedure Process_With is
         Previous_Was_Id : Boolean := True;
         Content : Unbounded_String := To_Unbounded_String ("with");
      begin
         loop
            declare
               Cur : constant Token := Next (T);
            begin
               exit when Cur.Kind = Delimiter and Cur.Content = ";";
               if Previous_Was_Id and Cur.Kind = Identifier then
                  Content := Content & ' ';
               end if;
               Content := Content & Cur.Content;
               Previous_Was_Id := Cur.Kind = Identifier;
            end;
         end loop;
         Data.Withs.Append (To_String (Content) & ";");
      end Process_With;

      procedure Gen_Copy_Item (Start : Token) is
         Previous_Was_Id : Boolean := True;
         Content : Unbounded_String := To_Unbounded_String (Start.Content);
      begin
         loop
            declare
               Cur : constant Token := Next (T);
            begin
               exit when Paren_Depth (T) = 0 and Cur.Kind = Delimiter and
                 Cur.Content = ";";
               if Previous_Was_Id and Cur.Kind = Identifier then
                  Content := Content & ' ';
               end if;
               Content := Content & Cur.Content;
               Previous_Was_Id := Cur.Kind = Identifier;
            end;
         end loop;
         Data.Items.Append (Body_Item'(Kind => Copy,
           To_Copy => Content & ";"));
      end Gen_Copy_Item;

      procedure Gen_Subprogram_Item (Start : Token) is
         Sig : Signature;
         Read_Property : access function return String;

         function Read_Parenthesed_Name return String is
            Open_Paren  : constant Token := Next (T);
            Name_String : constant Token := Next (T);
            Close_Paren : constant Token := Next (T);
         begin
            if Open_Paren.Kind /= Delimiter or else
              Open_Paren.Content /= "(" then
               Wrong_Token (Open_Paren, "Unexpected token, expected `(`");
            elsif Name_String.Kind /= String_Literal then
               Wrong_Token (Name_String, "Unexpected token, expected string");
            elsif Close_Paren.Kind /= Delimiter or else
              Close_Paren.Content /= ")" then
               Wrong_Token (Close_Paren, "Unexpected token, expected `)`");
            end if;
            return Name_String.Content;
         end Read_Parenthesed_Name;

         function Read_Aspect_Value return String is
            Arrow : constant Token := Next (T);
            Value : constant Token := Next (T);
         begin
            if Arrow.Kind /= Delimiter or else Arrow.Content /= "=>" then
               Wrong_Token (Arrow, "Unexpected token, expected `=>`");
            elsif Value.Kind /= String_Literal then
               Wrong_Token (Value, "Unexpected token, expected string");
            end if;
            return Value.Content;
         end Read_Aspect_Value;

         procedure Finish_Sig (From : Token) is
            procedure Start_Properties (Starting : Token) is
            begin
               if Starting.Kind = Identifier then
                  case Starting.Id is
                  when Keyword_With =>
                     Read_Property := Read_Aspect_Value'Access;
                     return;
                  when Keyword_Is =>
                     Read_Property := Read_Parenthesed_Name'Access;
                     return;
                  when others => null;
                  end case;
               end if;
               Wrong_Token (Starting,
                 "Unexpected token (expected ""with"" or ""is"")");
            end Start_Properties;
         begin
            case Start.Id is
            when Keyword_Procedure => Start_Properties (From);
            when Keyword_Function =>
               if From.Kind /= Identifier or else From.Id /= Keyword_Return then
                  Wrong_Token (From, "Unexpected token (expected ""return"")");
               end if;
               declare
                  After : constant Token := Get_Name (Next (T), Sig.Return_Type);
               begin
                  Start_Properties (After);
               end;
            when others => null;
            end case;
         end Finish_Sig;

         function Param_Names return String_Lists.Vector is
         begin
            return Ret : String_Lists.Vector do
               loop
                  declare
                     Cur : constant Token := Next (T);
                  begin
                     if Cur.Kind /= Identifier then
                        Wrong_Token (Cur, "Unexpected token (expected identifier)");
                     end if;
                     Ret.Append (Cur.Content);
                  end;
                  declare
                     Cur : constant Token := Next (T);
                  begin
                     if Cur.Kind /= Delimiter then
                        Wrong_Token (Cur, "Unexpected token (expected delimiter)");
                     end if;
                     exit when Cur.Content = ":";
                     if Cur.Content /= "," then
                        Wrong_Token (Cur, "Unexpected token (expected `,`)");
                     end if;
                  end;
               end loop;
            end return;
         end Param_Names;

         function Param_Type (Mode : out Param_Mode;
                              Name : out Unbounded_String) return Token is
            -- returns the next token
            In_Seen, Out_Seen, Access_Seen, Constant_Seen : Boolean := False;
         begin
            loop
               declare
                  Cur : constant Token := Next (T);
               begin
                  if Cur.Kind /= Identifier then
                     Wrong_Token (Cur, "Unexpected token (expected param type)");
                  end if;
                  case Cur.Id is
                  when Keyword_In =>
                     if In_Seen then
                        raise Parsing_Error with "Duplicate `in`!";
                     end if;
                     In_Seen := True;
                  when Keyword_Out =>
                     if Out_Seen then
                        raise Parsing_Error with "Duplicate `in`!";
                     end if;
                     Out_Seen := True;
                  when Keyword_Access =>
                     if Access_Seen then
                        raise Parsing_Error with "Duplicate `in`!";
                     end if;
                     Access_Seen := True;
                  when Keyword_Constant =>
                     if Constant_Seen then
                        raise Parsing_Error with "Duplicate `constant`!";
                     elsif not Access_Seen then
                        raise Parsing_Error with "`constant` is illegal here!";
                     end if;
                     Constant_Seen := True;
                  when others =>
                     if Access_Seen then
                        if In_Seen or Out_Seen then
                           raise Parsing_Error with
                             "No `in` or `out` allowed together with `access`!";
                        end if;
                        if Constant_Seen then
                           Mode := Mode_Access_Constant;
                        else
                           Mode := Mode_Access;
                        end if;
                     elsif Out_Seen then
                        if In_Seen then
                           Mode := Mode_In_Out;
                        else
                           Mode := Mode_Out;
                        end if;
                     else
                        Mode := Mode_In;
                     end if;
                     return Get_Name (Cur, Name);
                  end case;
               end;
            end loop;
         end Param_Type;

         procedure Read_Param_List is
         begin
            loop
               declare
                  Param : Parameter :=
                    Parameter'(Names => Param_Names, others => <>);
                  After : constant Token :=
                    Param_Type (Param.Mode, Param.Type_Name);
               begin
                  Sig.Params.Append (Param);
                  if After.Kind /= Delimiter or
                    (After.Content /= ";" and After.Content /= ")") then
                     Wrong_Token (After,
                       "Unexpected token (expected `;` or `)`)");
                  end if;
                  exit when After.Content = ")";
               end;
            end loop;
         end Read_Param_List;

         function Singleton (Sig : Signature) return Sig_Lists.Vector is
         begin
            return Ret : Sig_Lists.Vector do
               Ret.Append (Sig);
            end return;
         end Singleton;

         function Expand (List : Sig_Lists.Vector; Sig : Signature)
           return Sig_Lists.Vector is
         begin
            return Ret : Sig_Lists.Vector := List do
               Ret.Append (Sig);
            end return;
         end Expand;

         Name_Token  : constant Token   := Next (T);
         Name        : constant String  := Name_Token.Content;
         Kind_Seen   : Boolean := False;
         Wrappers    : String_Lists.Vector;
         Wrapper_Pos : Natural := 0;
      begin
         if Name_Token.Kind /= Identifier then
            Wrong_Token (Name_Token, "Unexpected token (expected identifier)");
         end if;
         declare
            Cur : constant Token := Next (T);
         begin
            if Cur.Kind /= Delimiter or else Cur.Content /= "(" then
               Finish_Sig (Cur);
            else
               Read_Param_List;
               Finish_Sig (Next (T));
            end if;
         end;
         <<istoken>>
         declare
            Cur : constant Token := Next (T);
         begin
            if Cur.Kind /= Identifier then
               Wrong_Token (Cur, "Unexpected token (expected `Static` or `Dynamic`)");
            end if;
            case Cur.Id is
            when Keyword_Static =>
               if Kind_Seen then
                  Wrong_Token (Cur, "Duplicate kind; only one of (`Static`, `Dynamic`) allowed.");
               else
                  Kind_Seen := True;
               end if;
               declare
                  use type Symbol_To_Index.Cursor;
                  Pos : constant Symbol_To_Index.Cursor :=
                    Subroutine_Defs.Find (Name_Token.Id);
               begin
                  if Pos = Symbol_To_Index.No_Element then
                     Data.Items.Append (Body_Item'(
                       Kind => Static, S_Name => To_Unbounded_String (Name),
                       S_GL_Name => To_Unbounded_String (Read_Property.all),
                       Sigs => Singleton (Sig)
                     ));
                     Subroutine_Defs.Insert (Name_Token.Id,
                                             Positive (Data.Items.Length));
                  else
                     declare
                        Item_Pos : constant Positive :=
                          Symbol_To_Index.Element (Pos);
                        Old : constant Body_Item :=
                          Data.Items.Element (Item_Pos);
                        GL_Name : constant Unbounded_String :=
                          To_Unbounded_String (Read_Property.all);
                     begin
                        if Old.Kind /= Static then
                           raise Parsing_Error with "Name """ & Name &
                             """ has previous definition which is not Static!";
                        elsif Old.S_GL_Name /= GL_Name then
                           raise Parsing_Error with """" & Name &
                             """ has previous definition with OpenGL name """ &
                             To_String (Old.S_GL_Name) & """ which differs from """ &
                             To_String (GL_Name) & """!";
                        end if;

                        Data.Items.Replace_Element (Symbol_To_Index.Element (Pos),
                          Body_Item'(
                             Kind => Static,
                             S_Name => To_Unbounded_String (Name),
                             S_GL_Name => GL_Name,
                             Sigs => Expand (Old.Sigs, Sig)
                        ));
                        Wrapper_Pos := Item_Pos;
                     end;
                  end if;
               end;
            when Keyword_Dynamic =>
               if Kind_Seen then
                  Wrong_Token (Cur, "Duplicate kind; only one of (`Static`, `Dynamic`) allowed.");
               else
                  Kind_Seen := True;
               end if;
               declare
                  Sig_Id : Positive := 1;
               begin
                  while Natural (Sig_Id) <= Natural (Proc.Dynamic_Subprogram_Types.Length)
                    loop
                     exit when Proc.Dynamic_Subprogram_Types.Element (Sig_Id) =
                       Sig;
                     Sig_Id := Sig_Id + 1;
                  end loop;
                  if Natural (Sig_Id) > Natural (Proc.Dynamic_Subprogram_Types.Length) then
                     Proc.Dynamic_Subprogram_Types.Append (Sig);
                  end if;
                  Data.Items.Append (Body_Item'(
                    Kind => Dynamic,
                    D_Name => To_Unbounded_String (Name),
                    D_GL_Name => To_Unbounded_String (Read_Property.all),
                    Sig_Id => Sig_Id
                  ));
               end;
            when Keyword_Wrapper =>
               Wrappers.Append (Read_Property.all);
            when others =>
               Wrong_Token (Cur,
                 "Unexpected identifier (expected `Static` or `Dynamic`)");
            end case;
            declare
               Cur : constant Token := Next (T);
            begin
               if Cur.Kind /= Delimiter then
                  Wrong_Token (Cur, "Unexpected token (expected `;` or `,`)");
               elsif Cur.Content = "," then
                  goto istoken;
               elsif Cur.Content /= ";" then
                  Wrong_Token (Cur, "Unexpected token (expected `;` or `,`)");
               end if;
            end;
            if Wrapper_Pos = 0 then
               Data.Wrappers.Append (Wrappers);
            else
               Data.Wrappers.Replace_Element (Wrapper_Pos,
                 String_Lists."&" (Data.Wrappers.Element (Wrapper_Pos),
                                   Wrappers));
            end if;
         end;
      end Gen_Subprogram_Item;

      procedure Gen_Use is
         Tmp : Unbounded_String := To_Unbounded_String ("");
      begin
         loop
            declare
               Ident : constant Token := Next (T);
            begin
               if Ident.Kind /= Identifier or else Is_Keyword (Ident.Id) then
                  Wrong_Token (Ident, "Unexpected token, expected identifier");
               end if;
               Append (Tmp, Ident.Content);
            end;
            declare
               Delim : constant Token := Next (T);
            begin
               if Delim.Kind /= Delimiter then
                  Wrong_Token (Delim, "Unexpected token, expected `;` or `.`");
               elsif Delim.Content = ";" then
                  exit;
               elsif Delim.Content = "." then
                  Append (Tmp, '.');
               else
                  Wrong_Token (Delim, "Unexpected token, expected `;` or `.`");
               end if;
            end;
         end loop;
         Data.Uses.Append (To_String (Tmp));
      end Gen_Use;
   begin
      loop
         <<continue1>>
         declare
            Cur : constant Token := Next (T);
         begin
            if Cur.Kind = Identifier then
               case Cur.Id is
               when Keyword_With => Process_With;
               when Keyword_Spec => exit;
               when others =>
                  Wrong_Token (Cur,
                    "Unexpected identifier (expected `with` or `spec`)");
               end case;
            elsif Cur.Kind = Comment then
               goto continue1;
            else
               Wrong_Token (Cur, "Unexpected token (expected identifier)");
            end if;
         end;
      end loop;
      declare
         Cur : constant Token := Get_Name (Next (T), Data.Name);
      begin
         if Cur.Kind /= Identifier or else Cur.Id /= Keyword_Is then
            Wrong_Token(Cur, "Unexpected token (expected `is`)");
         end if;
      end;
      Put_Line ("Processing spec """ & To_String (Data.Name) & """");
      loop
         <<continue2>>
         declare
            Cur : constant Token := Next (T);
         begin
            if Cur.Kind = Comment then
               goto continue2;
            elsif Cur.Kind /= Identifier then
               Wrong_Token (Cur, "Unexpected token (expected identifier)");
            end if;
            case Cur.Id is
            when Keyword_Pragma | Keyword_Type | Keyword_Subtype =>
               Gen_Copy_Item (Cur);
            when Keyword_Function | Keyword_Procedure =>
               Gen_Subprogram_Item (Cur);
            when Keyword_Use => Gen_Use;
            when Keyword_End => exit;
            when others =>
               Wrong_Token (Cur, "Unexpected identifier (expected keyword)");
            end case;
         end;
      end loop;
      declare
         End_Name : Unbounded_String;
         Cur      : constant Token := Get_Name (Next (T), End_Name);
      begin
         if End_Name /= Data.Name then
            raise Parsing_Error with "Wrong spec name after `end` (expected """ &
               To_String (Data.Name) & """, got """ & To_String (End_Name) & """).";
         end if;
         if Cur.Kind /= Delimiter or else Cur.Content /= ";" then
            Wrong_Token (Cur, "Unexpected token (expected `;`)");
         end if;
      end;
      Proc.List.Append (Data);
   exception when Error : Parsing_Error | Tokenization.Tokenization_Error =>
      raise Parsing_Error with "Parsing error at line" & Line (T)'Img &
        ", column" & Column (T)'Img & ":" & Character'Val (10) &
        Exception_Message (Error);
   end Parse_File;

   function First (Proc : Processor) return Spec is
   begin
      if Proc.List.Length > 0 then
         return 1;
      else
         return No_Spec;
      end if;
   end First;

   function Next (Proc : Processor; Cur : Spec) return Spec is
   begin
      if Cur = No_Spec then
         raise Constraint_Error;
      end if;
      return Ret : Spec := Cur + 1 do
         if Count_Type (Ret) > Proc.List.Length then
            Ret := No_Spec;
         end if;
      end return;
   end Next;

   procedure Write_API (Proc : Processor; Cur : Spec;
                        Dir_Path : String) is
      use Ada.Directories;
      Target : File_Type;
      Data : constant Spec_Data := Proc.List.Element (Cur);
      Name : constant String := To_String (Data.Name);
      File_Name : constant String :=
        Compose (Dir_Path, To_String (Data.File_Base_Name), "ads");
      Is_Root : constant Boolean := Name = "GL.API";
   begin
      Put_Line ("Writing API file for spec """ &
        To_String (Data.Name) & """: " & File_Name);
      Create (Target, Out_File, File_Name);
      Put_Line (Target, "-- Autogenerated by Generate, do not edit");
      for With_Stmt of Data.Withs loop
         Put_Line (Target, With_Stmt);
      end loop;
      Put_Line (Target,
        (if Is_Root then "private " else "") & "package " & Name & " is");
      Put_Line (Target, "   pragma Preelaborate;");
      for Use_Item of Data.Uses loop
         Put_Line (Target, "   use " & Use_Item & ";");
      end loop;
      if Is_Root then
         declare
            Sig_Id : Positive := 1;
         begin
            for Sig of Proc.Dynamic_Subprogram_Types loop
               declare
                  Type_Name : constant String := Indexed_Name ('T', Sig_Id);
                  Is_Function : constant Boolean := Length (Sig.Return_Type) > 0;
               begin
                  Put (Target, "   type " & Type_Name & " is access ");
                  Put (Target, (if Is_Function then "function" else "procedure"));
                  Put_Signature (Target,
                     Proc.Dynamic_Subprogram_Types.Element (Sig_Id), False);
                  Put_Line (Target, ";");
                  Put_Line (Target, "   pragma Convention (StdCall, " & Type_Name & ");");
               end;
               Sig_Id := Sig_Id + 1;
            end loop;
         end;
      end if;
      for Item of Data.Items loop
         case Item.Kind is
         when Copy =>
            Put_Line (Target, "   " & To_String (Item.To_Copy));
         when Static =>
            declare
               Sub_Name : constant String := To_String (Item.S_Name);
            begin
               for Sig of Item.Sigs loop
                  if Length (Sig.Return_Type) = 0 then
                     Put (Target, "   procedure " & Sub_Name);
                  else
                     Put (Target, "   function " & Sub_Name);
                  end if;
                  Put_Signature (Target, Sig, True);
                  Put_Line (Target, ";");
               end loop;
               Put_Line (Target, "   pragma Import (StdCall, " & Sub_Name &
                 ", """ & To_String (Item.S_GL_Name) & """);");
            end;
         when Dynamic =>
            declare
               Sub_Name : constant String := To_String (Item.D_Name);
               Type_Name : constant String := Indexed_Name ('T', Item.Sig_Id);
            begin
               Put_Line (Target, "   " & Sub_Name & " : " & Type_Name & ";");
            end;
         end case;
      end loop;
      Put_Line (Target, "end " & Name & ";");
      Close (Target);
   end Write_API;

   procedure Write_Init (Proc : Processor; Dir_Path : String) is
      Target : File_Type;

      procedure Write_Header is
         use Ada.Text_IO;
      begin
         Put_Line (Target, "-- Autogenerated by Generate, do not edit");
         Put_Line (Target, "with System;");
         Put_Line (Target, "with Ada.Unchecked_Conversion;");
         Put_Line (Target, "private with GL.API.Subprogram_Reference;");
         for Spec_Data of Proc.List loop
            Put_Line (Target, "private with " & To_String (Spec_Data.Name) & ";");
         end loop;
         Put_Line (Target, "procedure GL.Load_Function_Pointers is");
         Put_Line (Target, "   pragma Preelaborate;");
         Put_Line (Target, "   use GL.API;");
         Put_Line (Target, "   generic");
         Put_Line (Target, "      type Function_Reference is private;");
         Put_Line (Target, "   function Load (Function_Name : String) return Function_Reference;");
         Put_Line (Target, "   pragma Inline (Load);");
         Put_Line (Target, "   function Load (Function_Name : String) return Function_Reference is");
         Put_Line (Target, "      function As_Function_Reference is new Ada.Unchecked_Conversion (");
         Put_Line (Target, "        Source => System.Address, Target => Function_Reference);");
         Put_Line (Target, "      use type System.Address;");
         Put_Line (Target, "      Raw : System.Address := Subprogram_Reference (Function_Name);");
         Put_Line (Target, "   begin");
         Put_Line (Target, "      if Raw = System.Null_Address then");
         Put_Line (Target, "         Raw := Subprogram_Reference (Function_Name & ""ARB"");");
         Put_Line (Target, "         if Raw = System.Null_Address then");
         Put_Line (Target, "            Raw := Subprogram_Reference (Function_Name & ""EXT"");");
         Put_Line (Target, "         end if;");
         Put_Line (Target, "      end if;");
         Put_Line (Target, "      return As_Function_Reference (Raw);");
         Put_Line (Target, "   end Load;");
      end Write_Header;

      Index : Positive := 1;
   begin
      Put_Line ("Writing procedure ""GL.Init""");
      Create (Target, Out_File, Ada.Directories.Compose (Dir_Path, "gl-load_function_pointers.adb"));
      Write_Header;
      for Sub_Type of Proc.Dynamic_Subprogram_Types loop
         Put_Line (Target, "   function Load_" & Indexed_Name ('T', Index) &
           " is new Load (" & Indexed_Name ('T', Index) & ");");
         Index := Index + 1;
      end loop;
      Put_Line (Target, "begin");
      for Data of Proc.List loop
         declare
            Spec_Name : constant String := To_String (Data.Name);
         begin
            for Item of Data.Items loop
               if Item.Kind = Dynamic then
                  Put_Line (Target, "   " & Spec_Name &
                    '.' & To_String (Item.D_Name) & " := Load_" &
                    Indexed_Name ('T', Item.Sig_Id) & "(""" &
                    To_String (Item.D_GL_Name) & """);");
               end if;
            end loop;
         end;
      end loop;
      Put_Line (Target, "end GL.Load_Function_Pointers;");
      Close (Target);
   end Write_Init;

   procedure Write_Wrapper_Table (Proc : Processor;
                                  Dir_Path, Interface_Folder : String) is
      Target : File_Type;

      procedure Write_Header is
         use Ada.Text_IO;
      begin
         Put_Line (Target, "---");
         Put_Line (Target, "layout: default");
         Put_Line (Target, "title: OpenGL Mapping");
         Put_Line (Target, "permalink: mapping.html");
         Put_Line (Target, "---");
         Put_Line (Target, "<!-- Autogenerated by generate tool, do not edit -->");
         New_Line (Target);
         Put_Line (Target, "| OpenGL Function Name | OpenGLAda Wrapper");
         Put_Line (Target, "|----------------------+------------------");
      end Write_Header;

      function Get_Wrapper_Link (Qualified_Name : String) return String is
         use Ada.Characters.Handling;
         Prefix : constant String := "https://github.com/flyx/OpenGLAda/blob/master/src/gl/interface/";
         File_Name : Unbounded_String := To_Unbounded_String (0);
         Subroutine_Name : Unbounded_String := To_Unbounded_String (0);
      begin
         for Index in Qualified_Name'Range loop
            case Qualified_Name (Index) is
            when '.' =>
               if Length (File_Name) > 0 then
                     Append (File_Name, "-");
               end if;
               Append (File_Name, Subroutine_Name);
               Subroutine_Name := To_Unbounded_String (0);
            when others =>
               Append (Subroutine_Name, To_Lower (Qualified_Name (Index)));
            end case;
         end loop;
         Append (File_Name, ".ads");
         declare
            use Tokenization;
            use Ada.Strings;
            T : Tokenizer := Tokenize
               (Ada.Directories.Compose (Interface_Folder, To_String (File_Name)));
            Requested_Id : Symbol_Id;
         begin
            Register_Symbol (T, To_String (Subroutine_Name), Requested_Id);
            loop
               declare
                  Cur : constant Token := Next (T);
               begin
                  if Cur.Kind = Identifier and then Cur.Id = Requested_Id
                     then
                     return Prefix & To_String (File_Name) & "#L" &
                        Fixed.Trim (Line (T)'Img, Left);
                  elsif Cur.Kind = Stream_End then
                     exit;
                  end if;
               end;
            end loop;
            raise Parsing_Error with "Cannot find symbol " &
               To_String (Subroutine_Name) & " in file " &
               To_String (File_Name);
         end;
      end Get_Wrapper_Link;

      Index : Positive := 1;
   begin
      Put_Line ("Writing wrapper list");
      Create (Target, Out_File, Ada.Directories.Compose (Dir_Path, "WrapperList.md"));
      Write_Header;
      for Data of Proc.List loop
         for Index in Integer (Positive'First) .. Integer (Data.Items.Length) loop
            declare
               Item : constant Body_Item := Data.Items.Element (Index);
               GL_Name : constant String := To_String (
                 if Item.Kind = Static then Item.S_GL_Name else Item.D_GL_Name);
               Wrappers : constant String_Lists.Vector :=
                 Data.Wrappers.Element (Index);
            begin
               if Wrappers.Length > 0 then
                  Put (Target, "| `" & GL_Name & "` | ");
                  declare
                     First : Boolean := True;
                  begin
                     for Wrapper of Wrappers loop
                        if First then
                           First := False;
                        else
                           Put (Target, " <br/> ");
                        end if;
                        Put (Target,
                          "[" & Wrapper & "](" & Get_Wrapper_Link (Wrapper) &
                          ")");
                     end loop;
                  end;
                  New_Line (Target);
               end if;
            end;
         end loop;
      end loop;
      Close (Target);
   end Write_Wrapper_Table;
end Specs;