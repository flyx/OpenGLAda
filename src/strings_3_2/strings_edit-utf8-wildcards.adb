--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Wildcards                 Luebeck            --
--  Implementation                                 Winter, 2007       --
--                                                                    --
--                                Last revision :  22:44 07 Apr 2016  --
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

package body Strings_Edit.UTF8.Wildcards is

   type Outcome is (Success, Mismatch, Failure);
   subtype Tail is Character range
      Character'Val (2#1000_0000#)..Character'Val (2#1011_1111#);
   --
   -- Unchecked_Skip -- One UTF-8 character
   --
   --    Text    - The text
   --    Pointer - To start at
   --
   -- An UTF-8 encoded character has the tail of octets 2#10xx_xxxx#.
   -- Any other octet is the head of another character. The procedure
   -- skips one character (the head) and then loop over the tail.
   --
   -- Exceptions :
   --
   --    Constraint_Error - Pointer is out of range
   --
   procedure Unchecked_Skip
             (  Text    : String;
                Pointer : in out Integer
             )  is
      pragma Inline (Unchecked_Skip);
      Index : Integer := Pointer + 1;
   begin
      while Index <= Text'Last and then Text (Index) in Tail loop
         Index := Index + 1;
      end loop;
      Pointer := Index;
   end Unchecked_Skip;

   function Match
            (  Text       : String;
               Pattern    : String;
               Wide_Space : Boolean       := False;
               Blanks     : Character_Set := SpaceAndTab
            )  return Boolean is

      function Match (Text_Pointer, Pattern_Index : Integer)
         return Outcome is
         Index   : Integer := Pattern_Index;
         Pointer : Integer := Text_Pointer;
      begin
         while Index <= Pattern'Last loop
            if Pattern (Index) = '*' then
               --
               -- This is a wildcard. First we skip it. Then we match an
               -- empty  string  by  the  pattern,  and recursively call
               -- Match  in  order  to  match the rest of the name. When
               -- that fails we skip one UTF-8 character of the text and
               -- try again. When there is no more characters left, that
               -- means matching failure.
               --
               Index := Index + 1;
               loop
                  case Match (Pointer, Index) is
                     when Success =>
                        return Success;
                     when Failure =>
                        return Failure;
                     when Mismatch =>
                        if Pointer > Text'Last then
                           --
                           -- It  makes no sense to try anymore, because
                           -- all  alternatives  can be only longer than
                           -- the failed one.
                           --
                           return Failure;
                        else
                           --
                           -- Skip  one  character  as  matched  by  the
                           -- wildcard.
                           --
                           Unchecked_Skip (Text, Pointer);
                        end if;
                  end case;
               end loop;
            elsif Pointer > Text'Last then
               --
               -- Anything else will match a non-empty substring of  the
               -- text. Therefore if the text ends here it is a  failure
               -- because  any  possible  alternative  will require even
               -- more of text.
               --
               return Failure;
            elsif Wide_Space and then Pattern (Index) = ' ' then
               --
               -- This is an expanded space. It shall match at least one
               -- blank character and any number of following ones.
               --
               Index := Index + 1;
               if not Is_In (Text (Pointer), Blanks) then
                  return Mismatch;
               end if;
               Unchecked_Skip (Text, Pointer);
               while (  Pointer <= Text'Last
                     and then
                        Is_In (Text (Pointer), Blanks)
                     )
               loop
                  Unchecked_Skip (Text, Pointer);
               end loop;
            else
               --
               -- This  is  an ordinal character which is matched as-is.
               -- Here we match only its head and continue to match  its
               -- tail  upon  the  next  iteration  of  the  loop. It is
               -- possible  to  do because the octets of the tail of any
               -- UTF-8 encoded characted do not collide with either '*'
               -- or '.'.
               --
               if Text (Pointer) /= Pattern (Index) then
                  return Mismatch;
               end if;
               Pointer := Pointer + 1;
               Index   := Index + 1;
            end if;
         end loop;
         if Pointer > Text'Last then
            --
            -- Success due to fully matched text
            --
            return Success;
         else
            --
            -- Mismatch, which can be recovered from if there  are  some
            -- wildcards before.
            --
            return Mismatch;
         end if;
      end Match;
   begin
      return Match (Text'First, Pattern'First) = Success;
   end Match;

   function Match
            (  Text       : String;
               Pattern    : String;
               Map        : Unicode_Mapping;
               Wide_Space : Boolean       := False;
               Blanks     : Character_Set := SpaceAndTab
            )  return Boolean is

      function Match (Text_Pointer, Pattern_Index : Integer)
         return Outcome is
         Index   : Integer := Pattern_Index;
         Pointer : Integer := Text_Pointer;
      begin
         while Index <= Pattern'Last loop
            if Pattern (Index) = '*' then
               --
               -- This is a wildcard. First we skip it. Then we match an
               -- empty  string  by  the  pattern,  and recursively call
               -- Match  in  order  to  match the rest of the name. When
               -- that fails we skip one UTF-8 character of the text and
               -- try again. When there is no more characters left, that
               -- means matching failure.
               --
               Index := Index + 1;
               loop
                  case Match (Pointer, Index) is
                     when Success =>
                        return Success;
                     when Failure =>
                        return Failure;
                     when Mismatch =>
                        if Pointer > Text'Last then
                           --
                           -- It  makes no sense to try anymore, because
                           -- all  alternatives  can be only longer than
                           -- the failed one.
                           --
                           return Failure;
                        else
                           --
                           -- Skip  one  character  as  matched  by  the
                           -- wildcard.
                           --
                           Unchecked_Skip (Text, Pointer);
                        end if;
                  end case;
               end loop;
            elsif Pointer > Text'Last then
               --
               -- Anything else will match a non-empty substring of  the
               -- text. Therefore if the text ends here it is a  failure
               -- because  any  possible  alternative  will require even
               -- more of text.
               --
               return Failure;
            elsif Wide_Space and then Pattern (Index) = ' ' then
               --
               -- This is an expanded space. It shall match at least one
               -- blank character and any number of following ones.
               --
               Index := Index + 1;
               if not Is_In (Text (Pointer), Blanks) then
                  return Mismatch;
               end if;
               Unchecked_Skip (Text, Pointer);
               while (  Pointer <= Text'Last
                     and then
                        Is_In (Text (Pointer), Blanks)
                     )
               loop
                  Unchecked_Skip (Text, Pointer);
               end loop;
            else
               declare
                  Code_1 : UTF8_Code_Point;
                  Code_2 : UTF8_Code_Point;
               begin
                  Get (Text, Pointer,  Code_1);
                  Get (Pattern, Index, Code_2);
                  if Value (Map, Code_1) /= Value (Map, Code_2) then
                     return Mismatch;
                  end if;
               end;
            end if;
         end loop;
         if Pointer > Text'Last then
            --
            -- Success due to fully matched text
            --
            return Success;
         else
            --
            -- Mismatch, which can be recovered from if there  are  some
            -- wildcards before.
            --
            return Mismatch;
         end if;
      end Match;
   begin
      return Match (Text'First, Pattern'First) = Success;
   end Match;

end Strings_Edit.UTF8.Wildcards;
