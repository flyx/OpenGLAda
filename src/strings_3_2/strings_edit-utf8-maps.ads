--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Maps                      Luebeck            --
--  Interface                                      Spring, 2008       --
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
--
--  This package is an equivalent to Ada.Strings.Maps augmented to UTF-8
--  support. It provides sets and mappings of Unicode code points.
--
with Ada.Finalization;

package Strings_Edit.UTF8.Maps is
   pragma Elaborate_Body (Strings_Edit.UTF8.Maps);
--
-- Unicode_Set -- A set of all Unicode code points
--
   type Unicode_Set is private;
--
-- Unicode_Indicator_Function -- Set indicator function
--
--    Value - The parameter
--
-- Set  indicator  function  can  be used in order to construct a set by
-- selection code points from another set.
--
-- Returns :
--
--    True if Value is selected
--
   type Unicode_Indicator_Function is
      access function (Value : UTF8_Code_Point) return Boolean;
--
-- Set-theoreric operations
--
-- The operation - is defined as A - B = A and not B
--
   function "not" (Right : Unicode_Set) return Unicode_Set;
   function "not" (Right : Code_Points_Range) return Unicode_Set;
   function "and" (Left, Right : Unicode_Set) return Unicode_Set;
   function "and" (Left : Unicode_Set; Right : Code_Points_Range)
      return Unicode_Set;
   function "and" (Left : Code_Points_Range; Right : Unicode_Set)
      return Unicode_Set;
   function "and" (Left : Unicode_Set; Right : String)
      return Unicode_Set;
   function "and" (Left : String; Right : Unicode_Set)
      return Unicode_Set;
   function "or"  (Left, Right : Unicode_Set) return Unicode_Set;
   function "or"  (Left : Unicode_Set; Right : Code_Points_Range)
      return Unicode_Set;
   function "or"  (Left : Code_Points_Range; Right : Unicode_Set)
      return Unicode_Set;
   function "or" (Left : Unicode_Set; Right : String)
      return Unicode_Set;
   function "or" (Left : String; Right : Unicode_Set)
      return Unicode_Set;
   function "xor" (Left, Right : Unicode_Set) return Unicode_Set;
   function "xor" (Left : Unicode_Set; Right : Code_Points_Range)
      return Unicode_Set;
   function "xor" (Left : Code_Points_Range; Right : Unicode_Set)
      return Unicode_Set;
   function "xor" (Left : Unicode_Set; Right : String)
      return Unicode_Set;
   function "xor" (Left : String; Right : Unicode_Set)
      return Unicode_Set;
   function "-" (Left, Right : Unicode_Set) return Unicode_Set;
   function "-" (Left : Unicode_Set; Right : Code_Points_Range)
      return Unicode_Set;
   function "-" (Left : Code_Points_Range; Right : Unicode_Set)
      return Unicode_Set;
   function "-" (Left : Unicode_Set; Right : String) return Unicode_Set;
   function "-" (Left : String; Right : Unicode_Set) return Unicode_Set;
--
-- =, >, <, <=, >= -- Comparisons
--
   function "=" (Left, Right : Unicode_Set) return Boolean;
   function "=" (Left : Unicode_Set; Right : Code_Points_Range)
      return Boolean;
   function "=" (Left : Code_Points_Range; Right : Unicode_Set)
      return Boolean;
   function "=" (Left : Unicode_Set; Right : String) return Boolean;
   function "=" (Left : String; Right : Unicode_Set) return Boolean;
   function "<"  (Left, Right : Unicode_Set) return Boolean;
   function "<" (Left : Unicode_Set; Right : Code_Points_Range)
      return Boolean;
   function "<" (Left : Code_Points_Range; Right : Unicode_Set)
      return Boolean;
   function "<" (Left : Unicode_Set; Right : String) return Boolean;
   function "<" (Left : String; Right : Unicode_Set) return Boolean;
   function "<=" (Left, Right : Unicode_Set) return Boolean;
   function "<=" (Left : Unicode_Set; Right : Code_Points_Range)
      return Boolean;
   function "<=" (Left : Code_Points_Range; Right : Unicode_Set)
      return Boolean;
   function "<=" (Left : Unicode_Set; Right : String) return Boolean;
   function "<=" (Left : String; Right : Unicode_Set) return Boolean;
   function ">"  (Left, Right : Unicode_Set) return Boolean;
   function ">" (Left : Unicode_Set; Right : Code_Points_Range)
      return Boolean;
   function ">" (Left : Code_Points_Range; Right : Unicode_Set)
      return Boolean;
   function ">" (Left : Unicode_Set; Right : String) return Boolean;
   function ">" (Left : String; Right : Unicode_Set) return Boolean;
   function ">=" (Left, Right : Unicode_Set) return Boolean;
   function ">=" (Left : Unicode_Set; Right : Code_Points_Range)
      return Boolean;
   function ">=" (Left : Code_Points_Range; Right : Unicode_Set)
      return Boolean;
   function ">=" (Left : Unicode_Set; Right : String) return Boolean;
   function ">=" (Left : String; Right : Unicode_Set) return Boolean;
--
-- Cardinality -- The number of code points in the set
--
--    Set - The set
--
-- Returns :
--
--    The number of code points in
--
   function Cardinality (Set : Unicode_Set) return Natural;
--
-- Choose
--
--    Set       - The set to select elements from
--    Indicator - The indicator function
--
-- Returns :
--
--    The set of elements from Set selected by Indicator
--
   function Choose
            (  Set       : Unicode_Set;
               Indicator : Unicode_Indicator_Function
            )  return Unicode_Set;
--
-- Get -- Skip characters from a set
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Blanks  - The set code points to skip
--
-- This procedure skips all the characters of the  set  Blanks  starting
-- from Source (Pointer). Pointer is advanced  to  the  first  non-blank
-- character or to Source'Last + 1. When Source  is  improperly  encoded
-- the procedure stops at  the  position  following  the  last  properly
-- encoded code point from Blanks.
--
-- Exceptions :
--
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Blanks  : Unicode_Set
             );
--
-- Is_* -- Classification of a set
--
--    Set - To classify
--
-- Returns :
--
--    True if the set falls under the cathegory
--
   function Is_Empty (Set : Unicode_Set) return Boolean;
   function Is_Range (Set : Unicode_Set) return Boolean;
   function Is_Singleton (Set : Unicode_Set) return Boolean;
   function Is_Universal (Set : Unicode_Set) return Boolean;
--
-- Is_In -- Membership tests
--
--    Element - To test
--    Set     - The set
--
-- Returns :
--
--    True if Element is in Set
--
   function Is_In (Element : Character; Set : Unicode_Set)
      return Boolean;
   function Is_In (Element : Wide_Character; Set : Unicode_Set)
      return Boolean;
   function Is_In (Element : UTF8_Code_Point; Set : Unicode_Set)
      return Boolean;
--
-- Is_Subset -- Inclusion tests
--
--    Elements - To test
--    Set      - The set
--
-- When  Elements  is a string all characters of are tested. When it has
-- the type String it is considered Latin-1 encoded.
--
-- Returns :
--
--    True if Element is in Set
--
   function Is_Subset
            (  Elements : Code_Points_Range;
               Set      : Unicode_Set
            )  return Boolean renames "<=";
   function Is_Subset
            (  Elements : Code_Points_Ranges;
               Set      : Unicode_Set
            )  return Boolean;
   function Is_Subset
            (  Elements : String;
               Set      : Unicode_Set
            )  return Boolean renames "<=";
   function Is_Subset
            (  Elements : Unicode_Set;
               Set      : Unicode_Set
            )  return Boolean renames "<=";
--
-- To_Ranges -- Conversion
--
--    Set - To convert
--
-- Returns :
--
--    An array of ascending disjoint ranges of set elements
--
   function To_Ranges (Set : Unicode_Set) return Code_Points_Ranges;
--
-- To_Sequence -- Conversion to a sequence of UTF-8 characters
--
--    Set - To convert
--
-- Returns :
--
--    An UTF-8 string of the set elements in ascending order
--
   function To_Sequence (Set : Unicode_Set) return String;
--
-- To_Set -- Conversion
--
--    Singleton, Ranges, Span, String, Low..High - To convert
--
-- When  the  argument  is  Character,  it  is assumed Latin-1. When the
-- argument is an UTF-8 encoded string. When the argument is a range  or
-- a  set  of  ranges, then any range of the argument with the low bound
-- greater than the high bound is considered empty.
--
-- Result :
--
--    A set containing the argument
--
-- Exceptions :
--
--    Data_Error - Sequence is not a valid UTF-8 string
--
   function To_Set (Singleton : UTF8_Code_Point)    return Unicode_Set;
   function To_Set (Singleton : Character)          return Unicode_Set;
   function To_Set (Singleton : Wide_Character)     return Unicode_Set;
   function To_Set (Ranges    : Code_Points_Ranges) return Unicode_Set;
   function To_Set (Span      : Code_Points_Range)  return Unicode_Set;
   function To_Set (Low, High : UTF8_Code_Point)    return Unicode_Set;
   function To_Set (Sequence  : String)             return Unicode_Set;
   function To_Set (Indicator : Unicode_Indicator_Function)
      return Unicode_Set;
--
-- Trim -- Delete blank characters form string ends
--
--    Source - The string to be processed
--    Blanks - The set of blank characters
--
-- This function removes any characters of the Blanks set from both ends
-- of the string and returns the result.
--
-- Returns :
--
--    The result string
--
-- Exceptions :
--
--    Data_Error - Source is an invalid UTF-8 string
--
   function Trim
            (  Source : String;
               Blanks : Unicode_Set
            )  return String;
--
-- Set constants
--
   Null_Set      : constant Unicode_Set;
   Universal_Set : constant Unicode_Set;
--
-- Generic_Choose
--
--    Set - The set to select elements from
--
-- Returns :
--
--    The set of elements from Set selected by Indicator
--
   generic
      with function Indicator (Value : UTF8_Code_Point) return Boolean;
   function Generic_Choose (Set : Unicode_Set) return Unicode_Set;

   type Unicode_Mapping_Function is
      access function (Value : UTF8_Code_Point) return UTF8_Code_Point;
--
-- Unicode_Mapping -- Of the set of UTF-8 code points to itself
--
   type Unicode_Mapping is private;
--
-- Is_Prefix -- Test if Prefix is a prefix of Text
--
--    Prefix    - To check
--    Source    - The string
--  [ Pointer ] - To start at
--    Map       - Used to convert characters before comparison
--
-- Returns :
--
--    True if Prefix is a prefix of Source
--
-- Exceptions :
--
--    Data_Error - Illegal UTF-8 strings
--
   function Is_Prefix
            (  Prefix : String;
               Source : String;
               Map    : Unicode_Mapping
            )  return Boolean;
   function Is_Prefix
            (  Prefix  : String;
               Source  : String;
               Pointer : Integer;
               Map     : Unicode_Mapping
            )  return Boolean;
--
-- Value -- Mapping result
--
--    Map     - The mapping
--    Element - To map
--
-- The  second  argument  can  be a Latin-1 character, wide character or
-- else a code point.
--
-- Result :
--
--    The corresponding code point
--
   function Value
            (  Map     : Unicode_Mapping;
               Element : Character
            )  return UTF8_Code_Point;
   function Value
            (  Map     : Unicode_Mapping;
               Element : Wide_Character
            )  return UTF8_Code_Point;
   function Value
            (  Map     : Unicode_Mapping;
               Element : UTF8_Code_Point
            )  return UTF8_Code_Point;
--
-- To_Domain -- The domain set of non-idempotent part of the mapping
--
--    Map - The mapping
--
-- The result is an UTF-8 string of ascending code points  X  such  that
-- Map (X) /= X.
--
-- Returns :
--
--    The set
--
   function To_Domain (Map : Unicode_Mapping) return String;
--
-- To_Mapping -- Conversion of two string to a mapping
--
--    From - The domain set of the result mapping
--    To   - The range set of the domain
--
-- The  parameters are UTF-8 encoded strings. For nth code point of From
-- the resulting mapping yields the nth code point of To. For all  other
-- code points the mapping  acts  as  an  identity  mapping.  When  From
-- contains  repeating code points or else the numbers of code points in
-- From and To differ Translation_Error is propagated.
--
-- Returns :
--
--    A mapping From -> To
--
-- Exceptions :
--
--    Data_Error        - UTF-8 syntax error
--    Translation_Error - Different lengths or repeating points in From
--
   function To_Mapping (From, To : String) return Unicode_Mapping;
--
-- To_Mapping -- Construction from a function
--
--    Map - The mapping
--
-- The result is identity mapping if Map is null.
--
-- Returns :
--
--    The corresponding object
--
   function To_Mapping (Map : Unicode_Mapping_Function)
      return Unicode_Mapping;
--
-- To_Range -- The range set of non-idempotent part of the mapping
--
--    Map - The mapping
--
-- The  result is an UTF-8 string of code points X such that original of
-- X is not X. I.e. such that Map (Y) = X, where Y /= X. The  points  in
-- the result are ordered by Y. I.e. X1 precedes X2 if Y1 < Y2  and  Map
-- (Y1) = X1, Map (Y2) = X2.
--
-- Returns :
--
--    The set
--
   function To_Range (Map : Unicode_Mapping) return String;

   Identity : constant Unicode_Mapping;

private
   pragma Inline (Is_Empty, Is_In, Is_Universal, ">", ">=");
--
-- Generic_From_Ranges -- Select a set from ranges
--
--    Ranges - An array of ranges
--
-- Returns :
--
--    The set of elements from Ranges selected by Indicator
--
   generic
      with function Indicator (Code : UTF8_Code_Point) return Boolean;
   function Generic_From_Ranges (Ranges : Code_Points_Ranges)
      return Unicode_Set;
--
-- Code_Points_List -- Set implementation using sorted ranges list
--
-- The  implementation  is  a  list of ranges with an indicator function
-- selecting points from the ranges.
--
   type Code_Points_List (Size : Positive) is
      new Ada.Finalization.Controlled with
   record
      Use_Count : Natural := 1;
      Length    : Natural := 0;
      Indicator : Unicode_Indicator_Function;
      Ranges    : Code_Points_Ranges (1..Size);
   end record;
   type Code_Points_List_Ptr is access Code_Points_List;
   procedure Finalize (List : in out Code_Points_List);
--
-- Find -- Lookup
--
--    List - The list to search through
--    Code - To search for
--
-- Returns :
--
--    [+]  The index in the list of the range containing Code
--    [-]  Negated index of the first range following Code
--
   function Find
            (  List : Code_Points_List;
               Code : UTF8_Code_Point
            )  return Integer;
--
-- Flatten -- Produce an equivalent set with null indicator function
--
--    Set - The list to search through
--
-- Returns :
--
--    Ranges-only set
--
   function Flatten (Set : Unicode_Set) return Unicode_Set;
--
-- Remove -- Ranges from the list
--
--    List  - The list
--    First - The index of the first range to remove
--    Last  - The index of the last range
--
-- Nothing happens if First > Last.
--
   procedure Remove
             (  List  : in out Code_Points_List;
                First : Positive;
                Last  : Integer
             );
--
-- Clone -- In order to modify shared implementation
--
--    List      - To insert range into
--    Use_Count - Of the caller (inflicted by it on Set)
--    Increment - If no room
--
   procedure Clone
             (  List      : in out Code_Points_List_Ptr;
                Use_Count : Natural;
                Increment : Natural
             );
--
-- Insert -- A new range
--
--    List      - To insert range into
--    Span      - The range
--    Use_Count - Of the caller (inflicted by it on Set)
--    Increment - If no room
--
-- The range is merged to existing ranges as necessary.
--
   procedure Insert
             (  List      : in out Code_Points_List_Ptr;
                Span      : Code_Points_Range;
                Use_Count : Positive;
                Increment : Positive
             );
--
-- Release -- Decrease use count
--
--    List - The list to search through
--
   procedure Release (Set : in out Code_Points_List_Ptr);
   pragma Inline (Release);
--
-- Unchecked_Insert -- A new range
--
--    List      - To insert range into
--    Index     - Where to insert
--    Span      - The range
--    Use_Count - Of the caller (inflicted by it on Set)
--    Increment - If no room
--
-- Note that Index must be in the range 1..Set.Ptr.Length. The operation
-- neither sort nor merge ranges. It merely inserts  the  range  at  the
-- position specified.
--
   procedure Unchecked_Insert
             (  List      : in out Code_Points_List_Ptr;
                Index     : Positive;
                Item      : Code_Points_Range;
                Use_Count : Positive;
                Increment : Positive
             );

   type Unicode_Set is new Ada.Finalization.Controlled with record
      Ptr : Code_Points_List_Ptr;
   end record;
   procedure Adjust (Set : in out Unicode_Set);
   procedure Finalize (Set : in out Unicode_Set);

   Null_Set : constant Unicode_Set :=
                (Ada.Finalization.Controlled with null);

   Universal_Set : constant Unicode_Set :=
                   (  Ada.Finalization.Controlled
                   with
                      new Code_Points_List'
                          (  Ada.Finalization.Controlled
                          with
                             Size      => 1,
                             Use_Count => 1,
                             Length    => 1,
                             Indicator => null,
                             Ranges    => (1 => Full_Range)
                   )      );
--
-- Map_Implementation -- Abstract base with reference counting
--
   type Map_Implementation is
      abstract new Ada.Finalization.Controlled with
   record
      Use_Count : Natural := 1;
   end record;
   function To_Domain (Map : Map_Implementation)
      return String is abstract;
   function To_Range (Map : Map_Implementation)
      return String is abstract;
   function Value
            (  Map     : Map_Implementation;
               Element : UTF8_Code_Point
            )  return UTF8_Code_Point is abstract;

   type Map_Implementation_Ptr is access Map_Implementation'Class;
   procedure Finalize (Map : in out Map_Implementation);
   procedure Release  (Map : in out Map_Implementation_Ptr);
   pragma Inline (Release);
--
-- Code_Points_Map -- Mapping implemented by an array
--
   type Mapping is record
      From : UTF8_Code_Point;
      To   : UTF8_Code_Point;
   end record;
   type Mapping_Array is array (Positive range <>) of Mapping;
   type Code_Points_Map (Size : Positive) is
      new Map_Implementation with
   record
      Map : Mapping_Array (1..Size);
   end record;
--
-- Find -- Lookup
--
--    List - The list to search through
--    Code - To search for
--
-- Returns :
--
--    [+]  The index in the list of the range containing Code
--    [-]  Negated index of the first range following Code
--
   function Find
            (  List : Mapping_Array;
               Code : UTF8_Code_Point
            )  return Integer;
   function To_Domain (Map : Code_Points_Map) return String;
   function To_Range  (Map : Code_Points_Map) return String;
   function Value
            (  Map     : Code_Points_Map;
               Element : UTF8_Code_Point
            )  return UTF8_Code_Point;
--
-- Code_Points_Function -- Mapping implemented by function
--
   type Code_Points_Function is new Map_Implementation with
   record
      Map : Unicode_Mapping_Function;
   end record;
   function To_Domain (Map : Code_Points_Function) return String;
   function To_Range  (Map : Code_Points_Function) return String;
   function Value
            (  Map     : Code_Points_Function;
               Element : UTF8_Code_Point
            )  return UTF8_Code_Point;

   type Unicode_Mapping is
      new Ada.Finalization.Controlled with
   record
      Ptr : Map_Implementation_Ptr;
   end record;
   procedure Adjust (Map : in out Unicode_Mapping);
   procedure Finalize (Map : in out Unicode_Mapping);

   Identity : constant Unicode_Mapping :=
                (Ada.Finalization.Controlled with null);

end Strings_Edit.UTF8.Maps;
