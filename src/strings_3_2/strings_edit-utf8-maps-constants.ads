--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Maps.Constants            Luebeck            --
--  Interface                                      Spring, 2008       --
--                                                                    --
--                                Last revision :  21:03 21 Apr 2009  --
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
with Strings_Edit.UTF8.Mapping;  use Strings_Edit.UTF8.Mapping;

with Strings_Edit.UTF8.Categorization;
use  Strings_Edit.UTF8.Categorization;

package Strings_Edit.UTF8.Maps.Constants is

   Alphanumeric_Set      : constant Unicode_Set;
   Control_Set           : constant Unicode_Set;
   Digit_Set             : constant Unicode_Set;
   Identifier_Extend_Set : constant Unicode_Set;
   Identifier_Start_Set  : constant Unicode_Set;
   ISO_646_Set           : constant Unicode_Set;
   Letter_Set            : constant Unicode_Set;
   Lower_Set             : constant Unicode_Set;
   Other_Format_Set      : constant Unicode_Set;
   Space_Set             : constant Unicode_Set;
   Subscript_Digit_Set   : constant Unicode_Set;
   Superscript_Digit_Set : constant Unicode_Set;
   Title_Set             : constant Unicode_Set;
   Upper_Set             : constant Unicode_Set;

   Lower_Case_Map : constant Unicode_Mapping;
   Upper_Case_Map : constant Unicode_Mapping;

private
   Control_Set : constant Unicode_Set := To_Set (Is_Control'Access);
   Digit_Set   : constant Unicode_Set := To_Set (Is_Digit'Access);
   Letter_Set  : constant Unicode_Set := To_Set (Is_Letter'Access);
   Lower_Set   : constant Unicode_Set := To_Set (Is_Lower'Access);
   Upper_Set   : constant Unicode_Set := To_Set (Is_Upper'Access);
   Space_Set   : constant Unicode_Set := To_Set (Is_Space'Access);
   Title_Set   : constant Unicode_Set := To_Set (Is_Title'Access);
   ISO_646_Set : constant Unicode_Set := To_Set (Is_ISO_646'Access);

   Alphanumeric_Set      : constant Unicode_Set :=
                              To_Set (Is_Alphanumeric'Access);
   Identifier_Extend_Set : constant Unicode_Set :=
                              To_Set (Is_Identifier_Extend'Access);
   Identifier_Start_Set  : constant Unicode_Set :=
                              To_Set (Is_Identifier_Start'Access);
   Other_Format_Set      : constant Unicode_Set :=
                              To_Set (Is_Other_Format'Access);
   Subscript_Digit_Set   : constant Unicode_Set :=
                              To_Set (Is_Subscript_Digit'Access);
   Superscript_Digit_Set : constant Unicode_Set :=
                              To_Set (Is_Superscript_Digit'Access);

   Lower_Case_Map : constant Unicode_Mapping :=
                       To_Mapping (To_Lowercase'Access);
   Upper_Case_Map : constant Unicode_Mapping :=
                       To_Mapping (To_Uppercase'Access);

end Strings_Edit.UTF8.Maps.Constants;
