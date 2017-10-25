--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Streams                        Luebeck            --
--  Interface                                      Spring, 2009       --
--                                                                    --
--                                Last revision :  14:23 11 Feb 2012  --
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
--  The  package  provides  stream  interface to string. A string can be
--  read and written using stream I/O attributes.
--
with Ada.Streams;  use Ada.Streams;

with System.Storage_Elements;

package Strings_Edit.Streams is
--
-- String_Stream -- A string stream, when read,  Data (Position..Length)
--                  is amount of data available to read/write. Note that
-- before reading from the stream is  must  be  initialized  using  Set.
-- Otherwise  the  result of reading will be the unitialized contents of
-- the Data field.
--
   type String_Stream (Length : Natural) is
      new Root_Stream_Type with
   record
      Position : Positive := 1;
      Data     : String (1..Length);
   end record;
--
-- Get -- Written contents of the stream
--
--    Stream - The stream object
--
-- Get is an operation inverse to T'Write.
--
-- Returns :
--
--    String written
--
   function Get (Stream : String_Stream) return String;
--
-- Get_Size -- Number of stream elements available to write or to read
--
--    Stream - The stream object
--
   function Get_Size (Stream : String_Stream)
      return Stream_Element_Count;
--
-- Read -- Overrides Ada.Streams...
--
   procedure Read
             (  Stream : in out String_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
--
-- Rewind -- The stream
--
--    Stream - The stream object
--
-- This procedure moves Stream.Position to the  beginning.  This  undoes
-- all reading/writing actions.
--
   procedure Rewind (Stream : in out String_Stream);
--
-- Set -- Contents
--
--    Stream  - The stream object
--    Content - String to read
--
-- The  stream  is  changed  to contain Content. The next read operation
-- will yield the first  character  of  Content.  Set  is  an  operation
-- inverse to T'Read.
--
-- Exceptions :
--
--    Contraint_Error - no room in Stream
--
   procedure Set (Stream : in out String_Stream; Content : String);
--
-- Write -- Overrides Ada.Streams...
--
-- Exceptions :
--
--    End_Error - No room in the string
--
   procedure Write
             (  Stream : in out String_Stream;
                Item   : Stream_Element_Array
             );

private
   use System.Storage_Elements;

   pragma Inline (Get_Size);
--
-- Char_Count -- Number of characters per string elements
--
   Char_Count : constant := Stream_Element'Size / Character'Size;
--
-- Stream_Element'Size must be a  multiple  of  Character'Size  and  the
-- later be a multiple of Storage_Element'Size.
--
   subtype Confirmed is Boolean range True..True;
   Assert : constant Confirmed :=
            (  Char_Count * Character'Size = Stream_Element'Size
            and then
               Character'Size mod Storage_Element'Size = 0
            );
end Strings_Edit.Streams;
