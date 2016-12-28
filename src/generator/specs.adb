with Ada.Directories;
with Ada.Exceptions;

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
            Put (File, To_String (Param.Type_Name));
            Index := Index + 1;
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
      T : Tokenizer := Tokenize (Path);
      Data : Spec_Data := Spec_Data'(File_Base_Name => To_Unbounded_String
        (Ada.Directories.Base_Name (Path)), others => <>);
      In_Root_Package : Boolean := False;

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
         Sig : Signature :=
           Signature'(In_Root_Package => In_Root_Package, others => <>);

         procedure Finish_Sig (From : Token) is
         begin
            case Start.Id is
            when Keyword_Procedure =>
               if From.Kind /= Identifier or else From.Id /= Keyword_Is then
                  Wrong_Token (From, "Unexpected token (expected ""is"")");
               end if;
            when Keyword_Function =>
               if From.Kind /= Identifier or else From.Id /= Keyword_Return then
                  Wrong_Token (From, "Unexpected token (expected ""return"")");
               end if;
               declare
                  After : constant Token := Get_Name (Next (T), Sig.Return_Type);
               begin
                  if After.Kind /= Identifier or else After.Id /= Keyword_Is then
                     Wrong_Token (After, "Unexpected token (expected ""is"")");
                  end if;
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
            In_Seen, Out_Seen, Access_Seen : Boolean := False;
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
                  when others =>
                     if Access_Seen then
                        if In_Seen or Out_Seen then
                           raise Parsing_Error with
                             "No `in` or `out` allowed together with `access`!";
                        end if;
                        Mode := Mode_Access;
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

         function Read_GL_Name return Unbounded_String is
            Open_Paren  : constant Token := Next (T);
            Name_String : constant Token := Next (T);
            Close_Paren : constant Token := Next (T);
         begin
            if Open_Paren.Kind /= Delimiter or else Open_Paren.Content /= "(" then
               Wrong_Token (Open_Paren, "Unexpected token, expected `(`");
            elsif Name_String.Kind /= String_Literal then
               Wrong_Token (Name_String, "Unexpected token, expected string");
            elsif Close_Paren.Kind /= Delimiter or else Close_Paren.Content /= ")" then
               Wrong_Token (Close_Paren, "Unexpected token, expected `)`");
            end if;
            return To_Unbounded_String (Name_String.Content);
         end Read_GL_Name;

         Name_Token      : constant Token   := Next (T);
         Name            : constant String  := Name_Token.Content;
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
         declare
            Cur : constant Token := Next (T);
         begin
            if Cur.Kind /= Identifier then
               Wrong_Token (Cur, "Unexpected token (expected `Static` or `Dynamic`)");
            end if;
            case Cur.Id is
            when Keyword_Static =>
               Data.Items.Append (Body_Item'(
                 Kind => Static,
                 S_Name => To_Unbounded_String (Name),
                 S_GL_Name => Read_GL_Name,
                 Sig => Sig
               ));
            when Keyword_Dynamic =>
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
                  D_GL_Name => Read_GL_Name,
                  Sig_Id => Sig_Id
                  ));
               end;
            when others =>
               Wrong_Token (Cur,
                 "Unexpected identifier (expected `Static` or `Dynamic`)");
            end case;
            declare
               Cur : constant Token := Next (T);
            begin
               if Cur.Kind /= Delimiter or else Cur.Content /= ";" then
                  Wrong_Token (Cur, "Unexpected token (expected `;`)");
               end if;
            end;
         end;
      end Gen_Subprogram_Item;
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
      In_Root_Package := Data.Name = To_Unbounded_String ("GL.API");
      Ada.Text_IO.Put_Line ("Processing spec """ & To_String (Data.Name) & """");
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
            when Keyword_Pragma | Keyword_Use | Keyword_Type | Keyword_Subtype =>
               Gen_Copy_Item (Cur);
            when Keyword_Function | Keyword_Procedure =>
               Gen_Subprogram_Item (Cur);
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
   exception when Error : Parsing_Error =>
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
      use Ada.Text_IO;
      use Ada.Directories;
      Target : File_Type;
      Data : constant Spec_Data := Proc.List.Element (Cur);
      Name : constant String := To_String (Data.Name);
      File_Name : constant String :=
        Compose (Dir_Path, To_String (Data.File_Base_Name), "ads");
      Is_Root : constant Boolean := Name = "GL.API";
      Types_Declared : array (1 .. Positive (Proc.Dynamic_Subprogram_Types.Length)) of
                       Boolean := (others => False);
   begin
      Ada.Text_IO.Put_Line ("Writing API file for spec """ &
        To_String (Data.Name) & """: " & File_Name);
      Create (Target, Out_File, File_Name);
      for With_Stmt of Data.Withs loop
         Put_Line (Target, With_Stmt);
      end loop;
      Put_Line (Target, "package " & Name & " is");
      for Item of Data.Items loop
         case Item.Kind is
         when Copy =>
            Put_Line (Target, "   " & To_String (Item.To_Copy));
         when Static =>
            declare
               Is_Function : constant Boolean := Length (Item.Sig.Return_Type) = 0;
               Sub_Name : constant String := To_String (Item.S_Name);
            begin
               if Is_Function then
                  Put (Target, "   function " & Sub_Name);
               else
                  Put (Target, "   procedure " & Sub_Name);
               end if;
               Put_Signature (Target, Item.Sig, True);
               Put_Line (Target, ";");
               Put_Line (Target, "   pragma Import (StdCall, " & Sub_Name &
                 ", """ & To_String (Item.S_GL_Name) & """);");
            end;
         when Dynamic =>
            declare
               Sub_Name : constant String := To_String (Item.D_Name);
               Type_Name : constant String := Indexed_Name ('T', Item.Sig_Id);
               Sig : constant Signature :=
                 Proc.Dynamic_Subprogram_Types.Element (Item.Sig_Id);
               Is_Function : constant Boolean := Length (Sig.Return_Type) = 0;
            begin
               if not Types_Declared (Item.Sig_Id) and (
                 Is_Root or not Sig.In_Root_Package) then
                  Put (Target, "   type " & Type_Name & " is access ");
                  Put (Target, (if Is_Function then "function" else "procedure"));
                  Put_Signature (Target,
                    Proc.Dynamic_Subprogram_Types.Element (Item.Sig_Id), False);
                  Put_Line (Target, ";");
                  Put_Line (Target, "   pragma Convention (StdCall, " & Type_Name & ");");
                  Types_Declared (Item.Sig_Id) := True;
               end if;
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
         Put_Line (Target, "-- Autogenerated by Init_Generator, do not edit");
         Put_Line (Target, "with GL.API;");
         Put_Line (Target, "with System;");
         Put_Line (Target, "with Ada.Unchecked_Conversion;");
         Put_Line (Target, "private generic");
         Put_Line (Target, "   with function Raw_Subprogram_Reference (Name : String) return System.Address;");
         Put_Line (Target, "procedure GL.Load_Function_Pointers is");
         Put_Line (Target, "   use GL.API;");
         Put_Line (Target, "   generic");
         Put_Line (Target, "      type Function_Reference is private;");
         Put_Line (Target, "   function Load (Function_Name : String) return Function_Reference;");
         Put_Line (Target, "   pragma Inline (Load);");
         Put_Line (Target, "   function Load (Function_Name : String) return Function_Reference is");
         Put_Line (Target, "      function As_Function_Reference is new Ada.Unchecked_Conversion (");
         Put_Line (Target, "        Source => System.Address, Target => Function_Reference);");
         Put_Line (Target, "      Raw : System.Address := Raw_Subprogram_Reference (Function_Name);");
         Put_Line (Target, "   begin");
         Put_Line (Target, "      if Raw = System.Null_Address then");
         Put_Line (Target, "         Raw := Raw_Subprogram_Reference (Function_Name & ""ARB"");");
         Put_Line (Target, "         if Raw = System.Null_Address then");
         Put_Line (Target, "            Raw := Raw_Subprogram_Reference (Function_Name & ""EXT"");");
         Put_Line (Target, "         end if;");
         Put_Line (Target, "      end if;");
         Put_Line (Target, "   end Load;");
      end Write_Header;

      Index : Positive := 1;
   begin
      Ada.Text_IO.Put_Line ("Writing procedure ""GL.Init""");
      Create (Target, Out_File, Ada.Directories.Compose (Dir_Path, "gl-init.adb"));
      Write_Header;
      for Sub_Type of Proc.Dynamic_Subprogram_Types loop
         Put_Line (Target, "   function Load_" & Indexed_Name ('T', Index) &
           " is new Load (" & Indexed_Name ('T', Index) & ");");
      end loop;
      Put_Line (Target, "begin");
      for Data of Proc.List loop
         declare
            Spec_Name : constant String := To_String (Data.Name);
         begin
            for Item of Data.Items loop
               if Item.Kind = Dynamic then
                  Put_Line (Target, "   " & Spec_Name (3 .. Spec_Name'Last) &
                    '.' & To_String (Item.D_Name) & " := Load_" &
                    Indexed_Name ('T', Item.Sig_Id) & "(""" &
                    To_String (Item.D_GL_Name) & """);");
               end if;
            end loop;
         end;
      end loop;
      Close (Target);
   end Write_Init;
end Specs;