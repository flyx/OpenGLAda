--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Maps                      Luebeck            --
--  Implementation                                 Spring, 2008       --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Strings;        use Ada.Strings;

with Ada.Unchecked_Deallocation;

package body Strings_Edit.UTF8.Maps is

   Default_Increment : constant := 512;

   procedure Adjust (Set : in out Unicode_Set) is
   begin
      if Set.Ptr /= null then
         Set.Ptr.Use_Count := Set.Ptr.Use_Count + 1;
      end if;
   end Adjust;

   procedure Adjust (Map : in out Unicode_Mapping) is
   begin
      if Map.Ptr /= null then
         Map.Ptr.Use_Count := Map.Ptr.Use_Count + 1;
      end if;
   end Adjust;

   function Cardinality (Set : Unicode_Set) return Natural is
   begin
      if Set.Ptr = null then
         return 0;
      end if;
      declare
         Ranges : Code_Points_Ranges renames Set.Ptr.Ranges;
         Length : Natural := 0;
      begin
         if Set.Ptr.Indicator = null then
            for Index in 1..Set.Ptr.Length loop
               Length :=
                  (  Length
                  +  Natural
                     (  Ranges (Index).High
                     -  Ranges (Index).Low
                     )
                  +  1
                  );
            end loop;
         else
            for Index in 1..Set.Ptr.Length loop
               for Code in Ranges (Index).Low..Ranges (Index).High loop
                  if Set.Ptr.Indicator (Code) then
                     Length := Length + 1;
                  end if;
               end loop;
            end loop;
         end if;
         return Length;
      end;
   end Cardinality;

   procedure Clone
             (  List      : in out Code_Points_List_Ptr;
                Use_Count : Natural;
                Increment : Natural
             )  is
   begin
      if List = null then
         if Increment > 0 then
            List := new Code_Points_List (Increment);
         end if;
      else
         if List.Use_Count > Use_Count then
            declare
               Ranges : Code_Points_Ranges renames List.Ranges;
               Length : constant Natural := List.Length;
               Old    : constant Code_Points_List_Ptr := List;
            begin
               List := new Code_Points_List (Length + Increment);
               List.Length := Length;
               List.Indicator := Old.Indicator;
               List.Ranges (1..Length) := Ranges (1..Length);
               Release (List);
            end;
         end if;
      end if;
   end Clone;

   function Choose
            (  Set       : Unicode_Set;
               Indicator : Unicode_Indicator_Function
            )  return Unicode_Set is
   begin
      if Set.Ptr = null or else Set.Ptr.Length = 0 then
         return Null_Set;
      elsif Indicator = null then
         return Set;
      elsif Set.Ptr.Indicator = null then
         declare
            function Selector is
               new Generic_From_Ranges (Indicator.all);
         begin
            return Selector (Set.Ptr.Ranges (1..Set.Ptr.Length));
         end;
      else
         declare
            function Both (Code : UTF8_Code_Point) return Boolean is
            begin
               return
               (  Indicator (Code)
               and then
                  Set.Ptr.Indicator (Code)
               );
            end Both;
            function Selector is new Generic_From_Ranges (Both);
         begin
            return Selector (Set.Ptr.Ranges (1..Set.Ptr.Length));
         end;
      end if;
   end Choose;

   function Complement
            (  Ranges    : Code_Points_Ranges;
               Indicator : Unicode_Indicator_function := null
            )  return Unicode_Set is
      Append  : Natural := 1;
      Prepend : Natural := 1;
      Length  : Natural := Ranges'Length;
   begin
      if Length = 0 then
         if Indicator = null then
            return Universal_Set;
         else
            return
            (  Ada.Finalization.Controlled
            with
               new Code_Points_List'
                   (  Ada.Finalization.Controlled
                   with
                      Use_Count => 1,
                      Size      => 1,
                      Length    => 1,
                      Indicator => Indicator,
                      Ranges    => (1 => Full_Range)
            )      );
         end if;
      end if;
      if Ranges (Ranges'First).Low = UTF8_Code_Point'First then
         Prepend := 0;
      end if;
      if Ranges (Ranges'Last).High = UTF8_Code_Point'Last then
         Append := 0;
      end if;
      Length := Length + Append + Prepend - 1;
      if Length = 0 then
         return Null_Set;
      end if;
      declare
         Result : constant Unicode_Set :=
                     (  Ada.Finalization.Controlled
                     with
                        new Code_Points_List (Length)
                     );
         Inverse : Code_Points_Ranges renames Result.Ptr.Ranges;
      begin
         Result.Ptr.Length := Length;
         if Prepend > 0 then
            Inverse (1) :=
               (  UTF8_Code_Point'First,
                  Ranges (1).Low - 1
               );
         end if;
         for Index in Ranges'First..Ranges'Last - 1 loop
            Prepend := Prepend + 1;
            Inverse (Prepend) :=
               (  Ranges (Index).High + 1,
                  Ranges (Index + 1).Low - 1
               );
         end loop;
         if Append > 0 then
            Inverse (Prepend + 1) :=
               (  Ranges (Ranges'Last).High + 1,
                  UTF8_Code_Point'Last
               );
         end if;
         Result.Ptr.Indicator := Indicator;
         return Result;
      end;
   end Complement;

   function Equivalent
            (  Ranges_1    : Code_Points_Ranges;
               Indicator_1 : Unicode_Indicator_function;
               Ranges_2    : Code_Points_Ranges;
               Indicator_2 : Unicode_Indicator_function
            )  return Boolean is
   begin
      if Indicator_1 = Indicator_2 then
         return Ranges_1 = Ranges_2;
      end if;
      declare
         Index_1 : Integer := Ranges_1'First;
         Index_2 : Integer := Ranges_2'First;
         Code    : Code_Point := UTF8_Code_Point'First;
         Low_1   : Code_Point := Code_Point'Last;
         Low_2   : Code_Point := Code_Point'Last;
         High_1  : Code_Point := Code_Point'Last;
         High_2  : Code_Point := Code_Point'Last;
      begin
         if Index_1 <= Ranges_1'Last then
            Low_1  := Ranges_1 (Index_1).Low;
            High_1 := Ranges_1 (Index_1).High;
         end if;
         if Index_2 <= Ranges_2'Last then
            Low_2  := Ranges_2 (Index_2).Low;
            High_2 := Ranges_2 (Index_2).High;
         end if;
         loop
            while Code > High_1 loop
               -- Go to the next range
               if Index_1 >= Ranges_1'Last then
                  Low_1  := Code_Point'Last;
                  High_1 := Code_Point'Last;
                  exit;
               end if;
               Index_1 := Index_1 + 1;
               Low_1   := Ranges_1 (Index_1).Low;
               High_1  := Ranges_1 (Index_1).High;
            end loop;
            while Code > High_2 loop
               -- Go to the next range
               if Index_2 >= Ranges_2'Last then
                  Low_2  := Code_Point'Last;
                  High_2 := Code_Point'Last;
                  exit;
               end if;
               Index_2 := Index_2 + 1;
               Low_2   := Ranges_2 (Index_2).Low;
               High_2  := Ranges_2 (Index_2).High;
            end loop;
            if Code >= Low_1 then
               if Code >= Low_2 then
                  --
                  --       Code
                  --    [//|////  1st
                  --  [////|////  2nd
                  --
                  if (  (Indicator_1 = null or else Indicator_1 (Code))
                     xor
                        (Indicator_2 = null or else Indicator_2 (Code))
                     )
                  then
                     return False;
                  end if;
               else
                  --     Code
                  --  [//|/////// 1st
                  --     |  [//// 2nd
                  --
                  if Indicator_1 = null or else Indicator_1 (Code) then
                     return False;
                  end if;
               end if;
               Code := Code + 1;
            elsif Code >= Low_2 then
                  --     Code
                  --     |  [//// 1st
                  --  [//|/////// 2nd
                  --
               if Indicator_2 = null or else Indicator_2 (Code) then
                  return False;
               end if;
               Code := Code + 1;
            else
               --     Code
               --     |  [//// 1st
               --     |    [// 2nd
               --
               Code := UTF8_Code_Point'Min (Low_1, Low_2);
            end if;
            if Code > UTF8_Code_Point'Last then
               return True;
            end if;
         end loop;
      end;
   end Equivalent;

   procedure Finalize (List : in out Code_Points_List) is
   begin
      if List.Use_Count /= 0 then
         raise Program_Error;
      end if;
   end Finalize;

   procedure Finalize (Map : in out Map_Implementation) is
   begin
      if Map.Use_Count /= 0 then
         raise Program_Error;
      end if;
   end Finalize;

   procedure Finalize (Set : in out Unicode_Set) is
   begin
      Release (Set.Ptr);
   end Finalize;

   procedure Finalize (Map : in out Unicode_Mapping) is
   begin
      Release (Map.Ptr);
   end Finalize;

   function Find
            (  List : Code_Points_List;
               Code : UTF8_Code_Point
            )  return Integer is
   begin
      if List.Length = 0 then
         return -1;
      else
         declare
            From    : Integer := 1;
            To      : Integer := List.Length;
            This    : Integer;
            Current : Code_Points_Range;
         begin
            loop
               This := (From + To) / 2;
               Current := List.Ranges (This);
               if Current.Low > Code then
                  if This = From then
                     return -This;
                  end if;
                  To := This - 1;
               elsif Current.High < Code then
                  if This = To then
                     return -1 - This;
                  end if;
                  From := This + 1;
               else
                  return This;
               end if;
           end loop;
         end;
      end if;
   end Find;

   function Find
            (  List : Mapping_Array;
               Code : UTF8_Code_Point
            )  return Integer is
   begin
      if List'Length = 0 then
         return -1;
      else
         declare
            From    : Integer := List'First;
            To      : Integer := List'Last;
            This    : Integer;
            Current : Code_Point;
         begin
            loop
               This := (From + To) / 2;
               Current := List (This).From;
               if Current = Code then
                  return This;
               elsif Current > Code then
                  if This = From then
                     return -This - 1;
                  end if;
                  To := This - 1;
               else
                  if This = To then
                     return -This;
                  end if;
                  From := This + 1;
               end if;
            end loop;
         end;
      end if;
   end Find;

   function Flatten (Set : Unicode_Set) return Unicode_Set is
   begin
      if Set.Ptr = null or else Set.Ptr.Length = 0 then
         return Null_Set;
      elsif Set.Ptr.Indicator = null then
         return Set;
      else
         declare
            function Selector is
               new Generic_From_Ranges (Set.Ptr.Indicator.all);
         begin
            return Selector (Set.Ptr.Ranges (1..Set.Ptr.Length));
         end;
      end if;
   end Flatten;

   function Generic_From_Ranges (Ranges : Code_Points_Ranges)
      return Unicode_Set is
      Span     : Code_Points_Range;
      Position : Positive := 1;
      Empty    : Boolean  := True;
      Result   : Code_Points_List_Ptr;
   begin
      for Index in Ranges'Range loop
         for Code in Ranges (Index).Low..Ranges (Index).High loop
            if Indicator (Code) then
               if Empty then
                  Span.Low := Code;
                  Empty    := False;
               end if;
               Span.High := Code;
            else
               if not Empty then
                  Unchecked_Insert
                  (  Result,
                     Position,
                     Span,
                     1,
                     Default_Increment
                  );
                  Position := Position + 1;
                  Empty    := True;
               end if;
            end if;
         end loop;
         if not Empty then
            Unchecked_Insert
            (  Result,
               Position,
               Span,
               1,
               Default_Increment
            );
         end if;
      end loop;
      return (Ada.Finalization.Controlled with Result);
   end Generic_From_Ranges;

   function Generic_Choose (Set : Unicode_Set) return Unicode_Set is
   begin
      if Set.Ptr = null or else Set.Ptr.Length = 0 then
         return Null_Set;
      elsif Set.Ptr.Indicator = null then
         declare
            function Selector is new Generic_From_Ranges (Indicator);
         begin
            return Selector (Set.Ptr.Ranges (1..Set.Ptr.Length));
         end;
      else
         declare
            function Both (Code : UTF8_Code_Point) return Boolean is
            begin
               return
               (  Indicator (Code)
               and then
                  Set.Ptr.Indicator (Code)
               );
            end Both;
            function Selector is new Generic_From_Ranges (Both);
         begin
            return Selector (Set.Ptr.Ranges (1..Set.Ptr.Length));
         end;
      end if;
   end Generic_Choose;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Blanks  : Unicode_Set
             )  is
      Index : Integer := Pointer;
      To    : Integer := Index;
      Code  : UTF8_Code_Point;
   begin
      if (  Index < Source'First
         or else
            (  Index > Source'Last
            and then
               Index > Source'Last + 1
         )  )
      then
         raise Layout_Error;
      end if;
      while Index <= Source'Last loop
         Get (Source, Index, Code);
         exit when not Is_In (Code, Blanks);
         To := Index;
      end loop;
      Pointer := To;
   exception
      when Data_Error =>
         Pointer := To;
   end Get;

   procedure Insert
             (  List      : in out Code_Points_List_Ptr;
                Span      : Code_Points_Range;
                Use_Count : Positive;
                Increment : Positive
             )  is
   begin
      if Span.Low > Span.High then
         return;
      elsif List = null then
         Unchecked_Insert (List, 1, Span, Use_Count, Increment);
         return;
      end if;
      Clone (List, 1, Increment);
      declare
         Ranges : Code_Points_Ranges renames List.Ranges;
         Low    : Integer := Find (List.all, Span.Low);
         High   : Integer := Find (List.all, Span.High);
      begin
         if Low > 0 and then Low = High then
            return;
         end if;
         if Low < -1 then
            -- Check if we can merge it with the predecessor
            if Ranges (-Low - 1).High + 1 = Span.Low then
               Low := -Low - 1;
            end if;
         end if;
         if High < 0 and then -High <= List.Length then
            -- Check if we can merge it with the successor
            if Ranges (abs High).Low - 1 = Span.High then
               High := -High;
            end if;
         end if;
         if Low > 0 then
            if High > 0 then
               --
               --      | Low              | High
               --   [//|////]  [////]   [/|//////]
               --              |<--- removed --->|
               --   [////////////////////////////]
               --
               Ranges (Low).High := Ranges (High).High;
               Remove (List.all, Low + 1, High);
            else
               High := -High;
               --
               --      | Low                   |  High
               --   [//|///]  [////]  [//////] |  [/////]
               --      |      |<-- removed ->| |
               --   [//////////////////////////]  [/////]
               --
               Ranges (Low).High := Span.High;
               Remove (List.all, Low + 1, High - 1);
            end if;
         else
            Low := -Low;
            if High > 0 then
               --
               --       |  Low                | High
               --  [//] |  [///]  [////]  [///|/////]
               --       |         |<--- removed --->|
               --  [//] [///////////////////////////]
               --
               Ranges (Low) := (Span.Low, Ranges (High).High);
               Remove (List.all, Low + 1, High);
            else
               High := -High;
               if Low = High then
                  --
                  --       |       |  Low  High
                  --  [//] |       |  [////////]
                  --       |       |
                  --  [//] [///////]  [////////]
                  --
                  Unchecked_Insert
                  (  List,
                     Low,
                     Span,
                     Use_Count,
                     Increment
                  );
               else
                  --
                  --       |  Low                     |  High
                  --  [//] |  [///]  [////]  [//////] |  [/////]
                  --       |         |<-- removed ->| |
                  --  [//] [//////////////////////////]  [/////]
                  --
                  Ranges (Low) := Span;
                  Remove (List.all, Low + 1, High - 1);
               end if;
            end if;
         end if;
      end;
   end Insert;

   function Intersect
            (  Left, Right : Code_Points_Ranges;
               Indicator   : Unicode_Indicator_Function := null
            )  return Unicode_Set is
      Increment : constant Positive :=
                     Positive'Base'Min
                        (Left'Length + Right'Length, Default_Increment);
      Result  : Code_Points_List_Ptr;
      Index_1 : Positive := Left'First;
      Index_2 : Positive := Right'First;
      Index_3 : Positive := 1;
      Span_1  : Code_Points_Range := Left  (Index_1);
      Span_2  : Code_Points_Range := Right (Index_2);
   begin
      loop
         if Span_1.High < Span_2.Low then
            -- First precedes the second
            exit when Index_1 >= Left'Last;
            Index_1 := Index_1 + 1;
            Span_1  := Left (Index_1);
         elsif Span_1.Low > Span_2.High then
            -- Second precedes the first
            exit when Index_2 >= Right'Last;
            Index_2 := Index_2 + 1;
            Span_2  := Right (Index_2);
         else
            --
            -- First and second overlap, the intersection is inserted
            -- into   result   and  then  one  or  both  indices  are
            -- advanced.
            --
            Unchecked_Insert
            (  Result,
               Index_3,
               (  UTF8_Code_Point'Max (Span_1.Low,  Span_2.Low),
                  UTF8_Code_Point'Min (Span_1.High, Span_2.High)
               ),
               1,
               Increment
            );
            Index_3 := Index_3 + 1;
            if Span_1.High = Span_2.High then
               exit when Index_1 >= Left'Last
                  or else Index_2 >= Right'Last;
               Index_1 := Index_1 + 1;
               Span_1  := Left (Index_1);
               Index_2 := Index_2 + 1;
               Span_2  := Right (Index_2);
            elsif Span_1.High < Span_2.High then
               exit when Index_1 >= Left'Last;
               Index_1 := Index_1 + 1;
               Span_1  := Left (Index_1);
            else
               exit when Index_2 >= Right'Last;
               Index_2 := Index_2 + 1;
               Span_2  := Right (Index_2);
            end if;
         end if;
      end loop;
      Result.Indicator := Indicator;
      return (Ada.Finalization.Controlled with Result);
   end Intersect;

   function Is_Empty (Set : Unicode_Set) return Boolean is
   begin
      if Set.Ptr = null or else Set.Ptr.Length = 0 then
         return True;
      elsif Set.Ptr.Indicator = null then
         return False;
      end if;
      declare
         Ranges : Code_Points_Ranges renames Set.Ptr.Ranges;
      begin
         for Index in 1..Set.Ptr.Length loop
            for Code in Ranges (Index).Low..Ranges (Index).High loop
               if Set.Ptr.Indicator (Code) then
                  return False;
               end if;
            end loop;
         end loop;
         return True;
      end;
   end Is_Empty;

   function Is_In (Element : Character; Set : Unicode_Set)
      return Boolean is
   begin
      return Is_In (Character'Pos (Element), Set);
   end Is_In;

   function Is_In (Element : Wide_Character; Set : Unicode_Set)
      return Boolean is
   begin
      return Is_In (Wide_Character'Pos (Element), Set);
   end Is_In;

   function Is_In
            (  Element : UTF8_Code_Point;
               Set     : Unicode_Set
            )  return Boolean is
   begin
      if Set.Ptr = null or else Set.Ptr.Length = 0 then
         return False;
      else
         return
         (  Find (Set.Ptr.all, Element) > 0
         and then
            (  Set.Ptr.Indicator = null
            or else
               Set.Ptr.Indicator (Element)
         )  );
      end if;
   end Is_In;

   function Is_Prefix
            (  Prefix, Source : String;
               Map            : Unicode_Mapping
            )  return Boolean is
   begin
      if Prefix'Length = 0 then
         return True;
      end if;
      declare
         I : Integer := Prefix'First;
         J : Integer := Source'First;
         L : UTF8_Code_Point;
         R : UTF8_Code_Point;
      begin
         loop
            Get (Prefix, I, L);
            if J > Source'Last then
               return False;
            end if;
            Get (Prefix, J, R);
            if Value (Map, L) /= Value (Map, R) then
               return False;
            end if;
            if I > Prefix'Last then
               return True;
            end if;
         end loop;
      exception
         when Layout_Error =>
            return False;
      end;
   end Is_Prefix;

   function Is_Prefix
            (  Prefix, Source : String;
               Pointer        : Integer;
               Map            : Unicode_Mapping
            )  return Boolean is
   begin
      if Prefix'Length = 0 then
         return True;
      end if;
      declare
         I : Integer := Prefix'First;
         J : Integer := Pointer;
         L : UTF8_Code_Point;
         R : UTF8_Code_Point;
      begin
         loop
            Get (Prefix, I, L);
            if J > Source'Last then
               return False;
            end if;
            Get (Prefix, J, R);
            if Value (Map, L) /= Value (Map, R) then
               return False;
            end if;
            if I > Prefix'Last then
               return True;
            end if;
         end loop;
      exception
         when Layout_Error =>
            return False;
      end;
   end Is_Prefix;

   function Is_Range (Set : Unicode_Set) return Boolean is
   begin
      if Set.Ptr = null or else Set.Ptr.Length /= 1 then
         return False;
      elsif Set.Ptr.Indicator = null then
         return True;
      end if;
      declare
         Inside : Boolean := False;
         Count  : Natural := 0;
         Ranges : Code_Points_Ranges renames Set.Ptr.Ranges;
      begin
         for Index in 1..Set.Ptr.Length loop
            for Code in Ranges (Index).Low..Ranges (Index).High loop
               if Set.Ptr.Indicator (Code) then
                  if not Inside then
                     Inside := True;
                     Count  := Count + 1;
                     if Count > 1 then
                        return False;
                     end if;
                  end if;
               else
                  Inside := False;
               end if;
            end loop;
         end loop;
         return Count = 1;
      end;
   end Is_Range;

   function Is_Subset
            (  Elements : Code_Points_Ranges;
               Set      : Unicode_Set
            )  return Boolean is
   begin
      if Elements'Length = 0 then
         return True;
      elsif Set.Ptr = null then
         return False;
      else
         for Index in Elements'Range loop
            if not Is_Subset (Elements (Index), Set) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Subset;

   function Is_Subset
            (  Elements : Wide_String;
               Set      : Unicode_Set
            )  return Boolean is
   begin
      if Elements'Length = 0 then
         return True;
      elsif Set.Ptr = null then
         return False;
      else
         for Index in Elements'Range loop
            if not Is_In (Elements (Index), Set) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Subset;

   function Is_Singleton (Set : Unicode_Set) return Boolean is
   begin
      if Set.Ptr = null or else Set.Ptr.Length /= 1 then
         return False;
      elsif Set.Ptr.Indicator = null then
         return Set.Ptr.Ranges (1).Low = Set.Ptr.Ranges (1).High;
      end if;
      declare
         Ranges : Code_Points_Ranges renames Set.Ptr.Ranges;
         Count : Natural := 0;
      begin
         for Index in 1..Set.Ptr.Length loop
            for Code in Ranges (Index).Low..Ranges (Index).High loop
               if Set.Ptr.Indicator (Code) then
                  Count := Count + 1;
                  if Count > 1 then
                     return False;
                  end if;
               end if;
            end loop;
         end loop;
         return Count = 1;
      end;
   end Is_Singleton;

   function Is_Universal (Set : Unicode_Set) return Boolean is
   begin
      if (  Set.Ptr = null
         or else
            Set.Ptr.Length /= 1
         or else
            Set.Ptr.Ranges (1) /= Full_Range
         )
      then
         return False;
      end if;
      if Set.Ptr.Indicator = null then
         return True;
      end if;
      for Code in UTF8_Code_Point'Range loop
         if not Set.Ptr.Indicator (Code) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Universal;

   procedure Release (Set : in out Code_Points_List_Ptr) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Code_Points_List,
                Code_Points_List_Ptr
             );
   begin
      if Set /= null then
         case Set.Use_Count is
            when 0 =>
               raise Program_Error;
            when 1 =>
               Set.Use_Count := 0;
               Free (Set);
            when others =>
               Set.Use_Count := Set.Use_Count - 1;
         end case;
      end if;
   end Release;

   procedure Release (Map : in out Map_Implementation_Ptr) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Map_Implementation'Class,
                Map_Implementation_Ptr
             );
   begin
      if Map /= null then
         case Map.Use_Count is
            when 0 =>
               raise Program_Error;
            when 1 =>
               Map.Use_Count := 0;
               Free (Map);
            when others =>
               Map.Use_Count := Map.Use_Count - 1;
         end case;
      end if;
   end Release;

   procedure Remove
             (  List  : in out Code_Points_List;
                First : Positive;
                Last  : Integer
             )  is
      pragma Inline (Remove);
   begin
      if First <= Last then
         if Last < List.Length then
            List.Ranges (First..List.Length - Last) :=
               List.Ranges (Last + 1..List.Length);
         end if;
         List.Length := List.Length - Last + First - 1;
      end if;
   end Remove;

   function To_Domain (Map : Unicode_Mapping) return String is
   begin
      if Map.Ptr = null then
         return "";
      else
         return To_Domain (Map.Ptr.all);
      end if;
   end To_Domain;

   function To_Domain (Map : Code_Points_Map) return String is
      Length : Natural := 0;
      List   : Mapping_Array renames Map.Map;
   begin
      for Index in List'Range loop
         Length := Length + Image (List (Index).From)'Length;
      end loop;
      if Length = Natural'Last then
         raise Constraint_Error;
      end if;
      declare
         Result  : String (1..Length);
         Pointer : Positive := 1;
      begin
         for Index in List'Range loop
            Put (Result, Pointer, List (Index).From);
         end loop;
         return Result;
      end;
   end To_Domain;

   function To_Domain (Map : Code_Points_Function) return String is
      Length : Natural := 0;
      From   : UTF8_Code_Point;
      To     : UTF8_Code_Point;
   begin
      for Code in UTF8_Code_Point'Range loop
         if Map.Map (Code) /= Code then
            if Length = 0 then
               From := Code;
            end if;
            To := Code;
            Length := Length + Image (Code)'Length;
         end if;
      end loop;
      if Length = 0 then
         return "";
      elsif Length = Natural'Last then
         raise Constraint_Error;
      end if;
      declare
         Result  : String (1..Length);
         Pointer : Positive := 1;
      begin
         for Code in From..To loop
            if Map.Map (Code) /= Code then
               Put (Result, Pointer, Code);
            end if;
         end loop;
         return Result;
      end;
   end To_Domain;

   function To_Mapping (From, To : String) return Unicode_Mapping is
   begin
      if From'Length = 0 then
         if To'Length = 0 then
            return Identity;
         else
            raise Translation_Error;
         end if;
      end if;
      declare
         Result : constant Unicode_Mapping :=
                  (  Ada.Finalization.Controlled
                  with
                     new Code_Points_Map (Length (From))
                  );
         Map       : Mapping_Array renames
                        Code_Points_Map (Result.Ptr.all).Map;
         Pointer_1 : Positive := From'First;
         Pointer_2 : Positive := To'First;
         Size      : Natural  := 0;
         Index     : Integer;
         Item      : Mapping;
      begin
         loop
            Get (From, Pointer_1, Item.From);
            Get (To,   Pointer_2, Item.To);
            Index := Find (Map (1..Size), Item.From);
            if Index > 0 then
               raise Translation_Error;
            end if;
            Index := -Index;
            Map (Index + 1..Size + 1) := Map (Index..Size);
            Size := Size + 1;
            Map (Index) := Item;
            exit when Pointer_1 > From'Last;
         end loop;
         if Pointer_2 <= To'Last then
            raise Translation_Error;
         end if;
         return Result;
      exception
         when End_Error =>
            raise Translation_Error;
      end;
   end To_Mapping;

   function To_Mapping (Map : Unicode_Mapping_Function)
      return Unicode_Mapping is
   begin
      if Map = null then
         return Identity;
      else
         return
         (  Ada.Finalization.Controlled
         with
            new Code_Points_Function'
                (  Ada.Finalization.Controlled
                with
                   Use_Count => 1,
                   Map       => Map
         )      );
      end if;
   end To_Mapping;

   function To_Range (Map : Unicode_Mapping) return String is
   begin
      if Map.Ptr = null then
         return "";
      else
         return To_Range (Map.Ptr.all);
      end if;
   end To_Range;

   function To_Range  (Map : Code_Points_Map) return String is
      List   : Mapping_Array renames Map.Map;
      Length : Natural := 0;
   begin
      for Index in List'Range loop
         Length := Length + Image (List (Index).To)'Length;
      end loop;
      if Length = Natural'Last then
         raise Constraint_Error;
      end if;
      declare
         Result  : String (1..Length);
         Pointer : Natural := 0;
      begin
         for Index in List'Range loop
            Put (Result, Pointer, List (Index).To);
         end loop;
         return Result;
      end;
   end To_Range;

   function To_Range  (Map : Code_Points_Function) return String is
      Length : Natural := 0;
      From   : UTF8_Code_Point;
      To     : UTF8_Code_Point;
   begin
      for Code in UTF8_Code_Point'Range loop
         if Map.Map (Code) /= Code then
            if Length = 0 then
               From := Code;
            end if;
            To := Code;
            Length := Length + Image (Map.Map (Code))'Length;
         end if;
      end loop;
      if Length = 0 then
         return "";
      elsif Length = Natural'Last then
         raise Constraint_Error;
      end if;
      declare
         Result  : String (1..Length);
         Pointer : Positive := 1;
      begin
         for Code in From..To loop
            if Map.Map (Code) /= Code then
               Put (Result, Pointer, Map.Map (Code));
            end if;
         end loop;
         return Result;
      end;
   end To_Range;

   function To_Ranges (Set : Unicode_Set) return Code_Points_Ranges is
   begin
      if Set.Ptr = null then
         return Code_Points_Ranges'(1..0 => (0, 0));
      elsif Set.Ptr.Indicator = null then
         return Set.Ptr.Ranges (1..Set.Ptr.Length);
      else
         return To_Ranges (Flatten (Set));
      end if;
   end To_Ranges;

   function To_Set (Indicator : Unicode_Indicator_Function)
      return Unicode_Set is
   begin
      return
      (  Ada.Finalization.Controlled
      with
         new Code_Points_List'
             (  Ada.Finalization.Controlled
             with
                Use_Count => 1,
                Size      => 1,
                Length    => 1,
                Indicator => Indicator,
                Ranges    => (1 => Full_Range)
      )      );
   end To_Set;

   function To_Set (Span : Code_Points_Range) return Unicode_Set is
   begin
      if Span.Low > Span.High then
         return Null_Set;
      else
         return
         (  Ada.Finalization.Controlled
         with
            new Code_Points_List'
                (  Ada.Finalization.Controlled
                with
                   Use_Count => 1,
                   Size      => 1,
                   Length    => 1,
                   Indicator => null,
                   Ranges    => (1 => Span)
         )      );
      end if;
   end To_Set;

   function To_Set (Ranges : Code_Points_Ranges) return Unicode_Set is
      Increment : constant Positive :=
         Positive'Min (Ranges'Length + 1, Default_Increment);
      Result    : Code_Points_List_Ptr;
   begin
      for Index in Ranges'Range loop
         Insert (Result, Ranges (Index), 1, Increment);
      end loop;
      return (Ada.Finalization.Controlled with Result);
   end To_Set;

   function To_Set (Singleton : Character) return Unicode_Set is
   begin
      return
      (  Ada.Finalization.Controlled
      with
         new Code_Points_List'
             (  Ada.Finalization.Controlled
             with
                Use_Count => 1,
                Size      => 1,
                Length    => 1,
                Indicator => null,
                Ranges    =>
                   (  1 => (  Character'Pos (Singleton),
                              Character'Pos (Singleton)
      )      )     )       );
   end To_Set;

   function To_Set (Singleton : Wide_Character) return Unicode_Set is
   begin
      return
      (  Ada.Finalization.Controlled
      with
         new Code_Points_List'
             (  Ada.Finalization.Controlled
             with
                Use_Count => 1,
                Size      => 1,
                Length    => 1,
                Indicator => null,
                Ranges    =>
                   (  1 => (  Wide_Character'Pos (Singleton),
                              Wide_Character'Pos (Singleton)
      )      )     )       );
   end To_Set;

   function To_Set (Singleton : UTF8_Code_Point) return Unicode_Set is
   begin
      return
      (  Ada.Finalization.Controlled
      with
         new Code_Points_List'
             (  Ada.Finalization.Controlled
             with
                Use_Count => 1,
                Size      => 1,
                Length    => 1,
                Indicator => null,
                Ranges    => (1 => (Singleton, Singleton))
      )      );
   end To_Set;

   function To_Set (Sequence : String) return Unicode_Set is
   begin
      if Sequence'Length = 0 then
         return Null_Set;
      end if;
      declare
         Increment : constant Positive :=
            Positive'Min (Sequence'Length + 1, Default_Increment);
         Result    : Code_Points_List_Ptr;
         Pointer   : Positive := Sequence'First;
         Span      : Code_Points_Range;
         This      : UTF8_Code_Point;
      begin
         Get (Sequence, Pointer, Span.Low);
         Span.High := Span.Low;
         while Pointer <= Sequence'Last loop
            Get (Sequence, Pointer, This);
            if This in Span.Low..Span.High + 1 then
               -- Continue the range Span
               Span.High := UTF8_Code_Point'Max (Span.High, This);
            else
               -- Insert Span and start a new range
               Insert (Result, Span, 1, Increment);
               Span.Low  := This;
               Span.High := This;
            end if;
         end loop;
         Insert (Result, Span, 1, Increment);
         return (Ada.Finalization.Controlled with Result);
      end;
   end To_Set;

   function To_Set (Low, High : UTF8_Code_Point)
      return Unicode_Set is
   begin
      if Low > High then
         return Null_Set;
      else
         return
         (  Ada.Finalization.Controlled
         with
            new Code_Points_List'
                (  Ada.Finalization.Controlled
                with
                   Use_Count => 1,
                   Size      => 1,
                   Length    => 1,
                   Indicator => null,
                   Ranges    => (1 => (Low, High))
         )      );
      end if;
   end To_Set;

   function To_Sequence (Set : Unicode_Set) return String is
   begin
      if Set.Ptr = null or else Set.Ptr.Length = 0 then
         return "";
      end if;
      declare
         Ranges : Code_Points_Ranges renames Set.Ptr.Ranges;
         Length : Natural := 0;
      begin
         for Index in 1..Set.Ptr.Length loop
            for Code in Ranges (Index).Low..Ranges (Index).High loop
               if (  Set.Ptr.Indicator = null
                  or else
                     Set.Ptr.Indicator (Code)
                  )
               then
                  Length := Length + Image (Code)'Length;
               end if;
            end loop;
         end loop;
         if Length = 0 then
            return "";
         elsif Length = Natural'Last then
            raise Constraint_Error;
         end if;
         declare
            Result  : String (1..Length);
            Pointer : Integer := Result'First;
         begin
            for Index in 1..Set.Ptr.Length loop
               for Code in Ranges (Index).Low
                        .. Ranges (Index).High
               loop
                  if (  Set.Ptr.Indicator = null
                     or else
                        Set.Ptr.Indicator (Code)
                     )
                  then
                     Put (Result, Pointer, Code);
                  end if;
               end loop;
            end loop;
            return Result;
         end;
      end;
   end To_Sequence;

   function Trim
            (  Source : in String;
               Blanks : in Unicode_Set
            )  return String is
      First : Integer := Source'First;
      Next  : Integer := Source'Last + 1;
      Index : Integer;
      Code  : UTF8_Code_Point;
   begin
      Index := First;
      while First < Next loop
         Get (Source, Index, Code);
         exit when not Is_In (Code, Blanks);
         First := Index;
      end loop;
      Index := Next;
      while First < Next loop
         Get_Backwards (Source, Index, Code);
         exit when not Is_In (Code, Blanks);
         Next := Index;
      end loop;
      return Source (First..Next - 1);
   end Trim;

   procedure Unchecked_Insert
             (  List      : in out Code_Points_List_Ptr;
                Index     : Positive;
                Item      : Code_Points_Range;
                Use_Count : Positive;
                Increment : Positive
             )  is
   begin
      if List = null then
         if Index /= 1 then
            raise Constraint_Error;
         end if;
         Clone (List, 0, Increment);
         List.Length := 1;
      else
         if Index > List.Length + 1 then
            raise Constraint_Error;
         end if;
         if List.Length = List.Size then
            Clone (List, Use_Count, Increment);
         end if;
         declare
            Ranges : Code_Points_Ranges renames List.Ranges;
            Length : Natural renames List.Length;
         begin
            Ranges (Index + 1..Length + 1) := Ranges (Index..Length);
            Length := Length + 1;
         end;
      end if;
      List.Ranges (Index) := Item;
   end Unchecked_Insert;

   function Unite (Left, Right : Code_Points_Ranges)
      return Unicode_Set is
      Increment : constant Positive :=
                     Positive'Min
                        (Left'Length + Right'Length, Default_Increment);
      Result  : Unicode_Set;
      Index_1 : Positive := Left'First;
      Index_2 : Positive := Right'First;
      Index_3 : Positive := 1;
      Span_1  : Code_Points_Range := Left  (Index_1);
      Span_2  : Code_Points_Range := Right (Index_2);
   begin
      loop
         if Code_Point (Span_1.High) + 1 < Span_2.Low then
            -- First precedes the second
            Unchecked_Insert
            (  Result.Ptr,
               Index_3,
               Span_1,
               1,
               Increment
            );
            Index_3 := Index_3 + 1;
            if Index_1 >= Left'Last then
               -- Copy remainging ranges from Right and complete
               for Index in Index_2..Right'Last loop
                  Unchecked_Insert
                  (  Result.Ptr,
                     Index_3,
                     Right (Index),
                     1,
                     Right'Last - Index + 1
                  );
                  Index_3 := Index_3 + 1;
               end loop;
               return Result;
            end if;
            Index_1 := Index_1 + 1;
            Span_1  := Left (Index_1);
         elsif Span_1.Low > Code_Point (Span_2.High) + 1 then
            -- Second precedes the first
            Unchecked_Insert
            (  Result.Ptr,
               Index_3,
               Span_2,
               1,
               Increment
            );
            Index_3 := Index_3 + 1;
            if Index_2 >= Right'Last then
               -- Copy remainging ranges from Left and complete
               for Index in Index_1..Left'Last loop
                  Unchecked_Insert
                  (  Result.Ptr,
                     Index_3,
                     Left (Index),
                     1,
                     Left'Last - Index + 1
                  );
                  Index_3 := Index_3 + 1;
               end loop;
               return Result;
            end if;
            Index_2 := Index_2 + 1;
            Span_2  := Right (Index_2);
         else
            -- First and second overlap or else adjacent
            declare
               Span_3 : constant Code_Points_Range :=
                  (  UTF8_Code_Point'Min (Span_1.Low,  Span_2.Low),
                     UTF8_Code_Point'Max (Span_1.High, Span_2.High)
                  );
            begin
               if Index_2 < Right'Last then
                  -- Replace Span_1 and advance Index_2
                  Span_1  := Span_3;
                  Index_2 := Index_2 + 1;
                  Span_2  := Right (Index_2);
               elsif Index_1 < Left'Last then
                  -- Replace Span_2 and advance Index_1
                  Span_2  := Span_3;
                  Index_1 := Index_1 + 1;
                  Span_1  := Left (Index_1);
               else
                  -- No more ranges to deal with
                  Unchecked_Insert (Result.Ptr, Index_3, Span_3, 1, 1);
                  return Result;
               end if;
            end;
         end if;
      end loop;
   end Unite;

   function Value
            (  Map     : Code_Points_Map;
               Element : UTF8_Code_Point
            )  return UTF8_Code_Point is
      Index : constant Integer := Find (Map.Map, Element);
   begin
      if Index > 0 then
         return Map.Map (Index).To;
      else
         return Element;
      end if;
   end Value;

   function Value
            (  Map     : Code_Points_Function;
               Element : UTF8_Code_Point
            )  return UTF8_Code_Point is
   begin
      return Map.Map (Element);
   end Value;

   function Value
            (  Map     : Unicode_Mapping;
               Element : Character
            )  return UTF8_Code_Point is
   begin
      return Value (Map, Character'Pos (Element));
   end Value;

   function Value
            (  Map     : Unicode_Mapping;
               Element : Wide_Character
            )  return UTF8_Code_Point is
   begin
      return Value (Map, Wide_Character'Pos (Element));
   end Value;

   function Value
            (  Map     : Unicode_Mapping;
               Element : UTF8_Code_Point
            )  return UTF8_Code_Point is
   begin
      if Map.Ptr = null then
         return Element;
      else
         return Value (Map.Ptr.all, Element);
      end if;
   end Value;

   function "and" (Left, Right : Unicode_Set)
      return Unicode_Set is
   begin
      if Left.Ptr = null or else Right.Ptr = null then
         return Null_Set;
      elsif Left.Ptr = Right.Ptr then
         return Left;
      elsif Left.Ptr.Length = 0 or else Right.Ptr.Length = 0 then
         return Null_Set;
      elsif Left.Ptr.Indicator = null then
         return
            Intersect
            (  Left.Ptr.Ranges  (1..Left.Ptr.Length),
               Right.Ptr.Ranges (1..Right.Ptr.Length),
               Right.Ptr.Indicator
            );
      elsif Right.Ptr.Indicator = null then
         return
            Intersect
            (  Left.Ptr.Ranges  (1..Left.Ptr.Length),
               Right.Ptr.Ranges (1..Right.Ptr.Length),
               Left.Ptr.Indicator
            );
      else
         return Flatten (Left) and Right;
      end if;
   end "and";

   function "and" (Left : Unicode_Set; Right : Code_Points_Range)
      return Unicode_Set is
   begin
      if Left.Ptr = null or else Right.Low > Right.High then
         return Null_Set;
      elsif Left.Ptr.Length = 0 then
         return Null_Set;
      elsif Right = Full_Range then
         return Left;
      else
         return
            Intersect
            (  Left.Ptr.Ranges (1..Left.Ptr.Length),
               Code_Points_Ranges'(1 => Right),
               Left.Ptr.Indicator
            );
      end if;
   end "and";

   function "and" (Left : Code_Points_Range; Right : Unicode_Set)
      return Unicode_Set is
   begin
      return Right and Left;
   end "and";

   function "and" (Left : Unicode_Set; Right : String)
      return Unicode_Set is
   begin
      return Left and To_Set (Right);
   end "and";

   function "and" (Left : String; Right : Unicode_Set)
      return Unicode_Set is
   begin
      return To_Set (Left) and Right;
   end "and";

   function "not" (Right : Unicode_Set) return Unicode_Set is
   begin
      if Right.Ptr = null then
         return Universal_Set;
      elsif Right.Ptr.Indicator = null then
         return Complement (Right.Ptr.Ranges (1..Right.Ptr.Length));
      else
         return not Flatten (Right);
      end if;
   end "not";

   function "not" (Right : Code_Points_Range) return Unicode_Set is
   begin
      if Right.Low > Right.High then
         return Universal_Set;
      elsif Right.Low = UTF8_Code_Point'First then
         if Right.High = UTF8_Code_Point'Last then
            return Null_Set;
         else
            return
               To_Set
               (  Code_Points_Range'
                  (  Right.High + 1,
                     UTF8_Code_Point'Last
               )  );
         end if;
      else
         if Right.High = UTF8_Code_Point'Last then
            return
               To_Set
               (  Code_Points_Range'
                  (  UTF8_Code_Point'First,
                     Right.Low - 1
               )  );
         else
            return
            (  Ada.Finalization.Controlled
            with
               new Code_Points_List'
                   (  Ada.Finalization.Controlled
                   with
                      Use_Count => 1,
                      Size      => 2,
                      Length    => 2,
                      Indicator => null,
                      Ranges    =>
                         (  (UTF8_Code_Point'First, Right.Low - 1),
                            (Right.High + 1, UTF8_Code_Point'Last)
            )      )     );
         end if;
      end if;
   end "not";

   function "not" (Right : String) return Unicode_Set is
   begin
      return not To_Set (Right);
   end "not";

   function "or" (Left, Right : Unicode_Set)
      return Unicode_Set is
   begin
      if Left.Ptr = null or else Left.Ptr.Length = 0 then
         return Right;
      elsif Right.Ptr = null or else Right.Ptr.Length = 0 then
         return Left;
      elsif Left.Ptr = Right.Ptr then
         return Left;
      elsif Left.Ptr.Indicator /= null then
         return Flatten (Left) or Right;
      elsif Right.Ptr.Indicator /= null then
         return Left or Flatten (Right);
      else
         return
            Unite
            (  Left.Ptr.Ranges  (1..Left.Ptr.Length),
               Right.Ptr.Ranges (1..Right.Ptr.Length)
            );
      end if;
   end "or";

   function "or" (Left : Unicode_Set; Right : Code_Points_Range)
      return Unicode_Set is
      Result : Unicode_Set := Left;
   begin
      if Right.Low <= Right.High then
         Insert (Result.Ptr, Right, 2, 1);
      end if;
      return Result;
   end "or";

   function "or" (Left : Code_Points_Range; Right : Unicode_Set)
      return Unicode_Set is
   begin
      return Right or Left;
   end "or";

   function "or" (Left : Unicode_Set; Right : String)
      return Unicode_Set is
   begin
      return Left or To_Set (Right);
   end "or";

   function "or" (Left : String; Right : Unicode_Set)
      return Unicode_Set is
   begin
      return To_Set (Left) or Right;
   end "or";

   function "xor" (Left, Right : Unicode_Set)
      return Unicode_Set is
   begin
      return (Left and not Right) or (not Left and Right);
   end "xor";

   function "xor" (Left : Unicode_Set; Right : Code_Points_Range)
      return Unicode_Set is
   begin
      return (Left and not Right) or (not Left and Right);
   end "xor";

   function "xor" (Left : Code_Points_Range; Right : Unicode_Set)
      return Unicode_Set is
   begin
      return (Left and not Right) or (not Left and Right);
   end "xor";

   function "xor" (Left : Unicode_Set; Right : String)
      return Unicode_Set is
   begin
      return Left xor To_Set (Right);
   end "xor";

   function "xor" (Left : String; Right : Unicode_Set)
      return Unicode_Set is
   begin
      return To_Set (Left) xor Right;
   end "xor";

   function "=" (Left, Right : Unicode_Set) return Boolean is
   begin
      if Left.Ptr = Right.Ptr then
         return True;
      elsif Left.Ptr = null or else Left.Ptr.Length = 0 then
         return Is_Empty (Right);
      elsif Right.Ptr = null or else Right.Ptr.Length = 0 then
         return Is_Empty (Left);
      else
         return
            Equivalent
            (  Left.Ptr.Ranges  (1..Left.Ptr.Length),
               Left.Ptr.Indicator,
               Right.Ptr.Ranges (1..Right.Ptr.Length),
               Right.Ptr.Indicator
            );
      end if;
   end "=";

   function "=" (Left : Unicode_Set; Right : Code_Points_Range)
      return Boolean is
   begin
      if Left.Ptr = null or else Left.Ptr.Length = 0 then
         return Right.Low > Right.High;
      else
         return
            Equivalent
            (  Left.Ptr.Ranges (1..Left.Ptr.Length),
               Left.Ptr.Indicator,
               (1 => Right),
               null
            );
      end if;
   end "=";

   function "=" (Left : Code_Points_Range; Right : Unicode_Set)
      return Boolean is
   begin
      return Right = Left;
   end "=";

   function "=" (Left : Unicode_Set; Right : String)
      return Boolean is
   begin
      return Left = To_Set (Right);
   end "=";

   function "=" (Left : String; Right : Unicode_Set)
      return Boolean is
   begin
      return Right = To_Set (Left);
   end "=";

   function "<" (Left, Right : Unicode_Set) return Boolean is
   begin
      if (  Left.Ptr = Right.Ptr
         or else
            Right.Ptr = null
         or else
            Right.Ptr.Length = 0
         )
      then
         return False;
      elsif Left.Ptr.Indicator /= null then
         return Flatten (Left) < Right;
      elsif Right.Ptr.Indicator /= null then
         return Left < Flatten (Right);
      elsif Left.Ptr = null or else Left.Ptr.Length = 0 then
         return True;
      end if;
      declare
         Ranges_1 : Code_Points_Ranges renames Left.Ptr.Ranges;
         Ranges_2 : Code_Points_Ranges renames Right.Ptr.Ranges;
         Index_1  : Positive := 1;
         Index_2  : Positive := 1;
         Span_1   : Code_Points_Range := Ranges_1 (1);
         Span_2   : Code_Points_Range := Ranges_2 (1);
         Same     : Boolean := True;
      begin
         loop
            if Span_1.Low < Span_2.Low then
               --
               --   [XXXX///////   Left
               --        [\\\\\\   Right
               --
               return False;
            elsif Span_1.Low <= Span_2.High then
               --
               --     [//////   Left
               --  \\\\\]       Right
               --
               if Span_1.High > Span_2.High then
                  return False;
               end if;
               -- Left range is contained by the right one
               Same := Same and then Span_1 = Span_2;
               if Index_1 = Left.Ptr.Length then
                  return not Same;
               end if;
               Index_1 := Index_1 + 1;
               Span_1  := Ranges_1 (Index_1);
            else
               --
               --          [//////   Left
               --  \\\\\]            Right
               --
               -- We  have  an  uncompared  range  from the Left set. If
               -- there is no more ranges from the Right set, then it is
               -- a failure.
               --
               if Index_2 = Right.Ptr.Length then
                  return False;
               end if;
               Index_2 := Index_2 + 1;
               Span_2  := Ranges_2 (Index_2);
            end if;
         end loop;
      end;
   end "<";

   function "<" (Left : Unicode_Set; Right : Code_Points_Range)
      return Boolean is
   begin
      if Right.Low > Right.High then
         return False;
      elsif Left.Ptr = null or else Left.Ptr.Length = 0 then
         return True;
      elsif Left.Ptr.Indicator /= null then
         return Flatten (Left) < Right;
      elsif Left.Ptr.Ranges (1).Low < Right.Low then
         return False;
      elsif Left.Ptr.Ranges (1).Low = Right.Low then
         return Left.Ptr.Ranges (Left.Ptr.Length).High < Right.High;
      else
         return Left.Ptr.Ranges (Left.Ptr.Length).High <= Right.High;
      end if;
   end "<";

   function "<" (Left : Code_Points_Range; Right : Unicode_Set)
      return Boolean is
   begin
      if Right.Ptr = null or else Right.Ptr.Length = 0 then
         return False;
      elsif Right.Ptr.Indicator /= null then
         return Left < Flatten (Right);
      elsif Left.Low > Left.High then
         return True;
      else
         declare
            Low : constant Integer := Find (Right.Ptr.all, Left.Low);
         begin
            return
            (  Low > 0
            and then
               Low = Find (Right.Ptr.all, Left.High)
            and then
               Right.Ptr.Ranges (Low) /= Left
            );
         end;
      end if;
   end "<";

   function "<" (Left : Unicode_Set; Right : String)
      return Boolean is
   begin
      return Left < To_Set (Right);
   end "<";

   function "<" (Left : String; Right : Unicode_Set)
      return Boolean is
   begin
      return To_Set (Left) < Right;
   end "<";

   function "<=" (Left, Right : Unicode_Set) return Boolean is
   begin
      if (  Left.Ptr = Right.Ptr
         or else
            Left.Ptr = null
         or else
            Left.Ptr.Length = 0
         )
      then
         return True;
      elsif Left.Ptr.Indicator /= null then
         return Flatten (Left) <= Right;
      elsif Right.Ptr.Indicator /= null then
         return Left <= Flatten (Right);
      elsif Right.Ptr = null or else Right.Ptr.Length = 0 then
         return False;
      end if;
      declare
         Ranges_1 : Code_Points_Ranges renames Left.Ptr.Ranges;
         Ranges_2 : Code_Points_Ranges renames Right.Ptr.Ranges;
         Index_1  : Positive := 1;
         Index_2  : Positive := 1;
         Span_1   : Code_Points_Range := Ranges_1 (1);
         Span_2   : Code_Points_Range := Ranges_2 (1);
      begin
         loop
            if Span_1.Low < Span_2.Low then
               --
               --   [XXXX///////   Left
               --        [\\\\\\   Right
               --
               return False;
            elsif Span_1.Low <= Span_2.High then
               --
               --     [//////   Left
               --  \\\\\]       Right
               --
               if Span_1.High > Span_2.High then
                  return False;
               end if;
               -- Left range is contained by the right one
               if Index_1 = Left.Ptr.Length then
                  return True;
               end if;
               Index_1 := Index_1 + 1;
               Span_1  := Ranges_1 (Index_1);
            else
               --
               --          [//////   Left
               --  \\\\\]            Right
               --
               -- We  have  an  uncompared  range  from the Left set. If
               -- there is no more ranges from the Right set, then it is
               -- a failure.
               --
               if Index_2 = Right.Ptr.Length then
                  return False;
               end if;
               Index_2 := Index_2 + 1;
               Span_2  := Ranges_2 (Index_2);
            end if;
         end loop;
      end;
   end "<=";

   function "<=" (Left : Unicode_Set; Right : Code_Points_Range)
      return Boolean is
   begin
      if Left.Ptr = null or else Left.Ptr.Length = 0 then
         return True;
      elsif Left.Ptr.Indicator /= null then
         for Code in Right.Low..Right.High loop
            if not Is_In (Code, Left) then
               return False;
            end if;
         end loop;
         return True;
      elsif Right.Low > Right.High then
         return False;
      else
         return
         (  Left.Ptr.Ranges (1).Low >= Right.Low
         and then
            Left.Ptr.Ranges (Left.Ptr.Length).High <= Right.High
         );
      end if;
   end "<=";

   function "<=" (Left : Code_Points_Range; Right : Unicode_Set)
      return Boolean is
   begin
      if Left.Low > Left.High then
         return False;
      elsif Right.Ptr = null or else Right.Ptr.Length = 0 then
         return True;
      elsif Left.Low = Left.High then
         return Is_In (Left.Low, Right);
      elsif Right.Ptr.Indicator /= null then
         return Left <= Flatten (Right);
      else
         declare
            Low : constant Integer := Find (Right.Ptr.all, Left.Low);
         begin
            return
            (  Low > 0
            and then
               Low = Find (Right.Ptr.all, Left.High)
            );
         end;
      end if;
   end "<=";

   function "<=" (Left : Unicode_Set; Right : String)
      return Boolean is
   begin
      return Left <= To_Set (Right);
   end "<=";

   function "<=" (Left : String; Right : Unicode_Set)
      return Boolean is
   begin
      return To_Set (Left) <= Right;
   end "<=";

   function ">" (Left, Right : Unicode_Set) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">" (Left : Unicode_Set; Right : Code_Points_Range)
      return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">" (Left : Code_Points_Range; Right : Unicode_Set)
      return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">" (Left : Unicode_Set; Right : String)
      return Boolean is
   begin
      return  To_Set (Right) < Left;
   end ">";

   function ">" (Left : String; Right : Unicode_Set)
      return Boolean is
   begin
      return Right < To_Set (Left);
   end ">";

   function ">=" (Left, Right : Unicode_Set) return Boolean is
   begin
      return Right <= Left;
   end ">=";

   function ">=" (Left : Unicode_Set; Right : Code_Points_Range)
      return Boolean is
   begin
      return Right <= Left;
   end ">=";

   function ">=" (Left : Code_Points_Range; Right : Unicode_Set)
      return Boolean is
   begin
      return Right <= Left;
   end ">=";

   function ">=" (Left : Unicode_Set; Right : String)
      return Boolean is
   begin
      return To_Set (Right) <= Left;
   end ">=";

   function ">=" (Left : String; Right : Unicode_Set)
      return Boolean is
   begin
      return Right <= To_Set (Left);
   end ">=";

   function "-" (Left, Right : Unicode_Set) return Unicode_Set is
   begin
     if Is_Empty (Left) or else Is_Universal (Right) then
        return Null_Set;
     elsif Is_Empty (Right) then
        return Left;
     elsif Is_Universal (Left) then
        return not Right;
     else
        return Left and not Right;
     end if;
   end "-";

   function "-" (Left : Unicode_Set; Right : Code_Points_Range)
      return Unicode_Set is
   begin
     if Is_Empty (Left) or else Right = Full_Range then
        return Null_Set;
     elsif Right.Low > Right.High then
        return Left;
     elsif Is_Universal (Left) then
        return not Right;
     else
        return Left and not Right;
     end if;
   end "-";

   function "-" (Left : Code_Points_Range; Right : Unicode_Set)
      return Unicode_Set is
   begin
     if Left.Low > Left.High or else Is_Universal (Right) then
        return Null_Set;
     elsif Is_Empty (Right) then
        return To_Set (Left);
     elsif Left = Full_Range then
        return not Right;
     else
        return Left and not Right;
     end if;
   end "-";

   function "-" (Left : Unicode_Set; Right : String)
      return Unicode_Set is
   begin
      return Left - To_Set (Right);
   end "-";

   function "-" (Left : String; Right : Unicode_Set)
      return Unicode_Set is
   begin
      return To_Set (Left) - Right;
   end "-";

end Strings_Edit.UTF8.Maps;
