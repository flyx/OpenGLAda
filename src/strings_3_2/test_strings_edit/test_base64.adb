--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Base64                                 Luebeck            --
--  Test                                           Autumn, 2014       --
--                                                                    --
--                                Last revision :  10:03 22 Nov 2014  --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;
with Strings_Edit.Base64;  use Strings_Edit.Base64;

procedure Test_Base64 is
   procedure Check_1 (Plain, Encoded : String) is
   begin
      if To_Base64 (Plain) /= Encoded then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Encoding '"
            &  Plain
            &  "' error, got '"
            &  To_Base64 (Plain)
            &  "', expected '"
            &  Encoded
            &  '''
         )  );
      end if;
      if From_Base64 (Encoded) /= Plain then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Decoding '"
            &  Encoded
            &  "' error, got '"
            &  From_Base64 (Encoded)
            &  "', expected '"
            &  Plain
            &  '''
         )  );
      end if;
   end Check_1;

   procedure Check_2 (Encoded, Plain : String) is
   begin
      if From_Base64 (Encoded) /= Plain then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Decoding '"
            &  Encoded
            &  "' error, got '"
            &  From_Base64 (Encoded)
            &  "', expected '"
            &  Plain
            &  '''
         )  );
      end if;
   end Check_2;
begin
   Check_1 ("Man",                  "TWFu");
   Check_1 ("sure.",                "c3VyZS4=");
   Check_1 ("asure.",               "YXN1cmUu");
   Check_1 ("any carnal pleasure.", "YW55IGNhcm5hbCBwbGVhc3VyZS4=");
   Check_1 ("any carnal pleasure",  "YW55IGNhcm5hbCBwbGVhc3VyZQ==");
   Check_1 ("any carnal pleasur",   "YW55IGNhcm5hbCBwbGVhc3Vy");
   Check_1 ("any carnal pleasu",    "YW55IGNhcm5hbCBwbGVhc3U=");
   Check_1 ("any carnal pleas",     "YW55IGNhcm5hbCBwbGVhcw==");
   Check_1 ("pleasure.",            "cGxlYXN1cmUu");
   Check_1 ("leasure.",             "bGVhc3VyZS4=");
   Check_1 ("easure.",              "ZWFzdXJlLg==");

   Check_2 ("YW55IGNhcm5hbCBwbGVhcw",   "any carnal pleas");
   Check_2 ("YW55IGNhcm5hbCBwbGVhc3U",  "any carnal pleasu");
   Check_2 ("YW55IGNhcm5hbCBwbGVhc3Vy", "any carnal pleasur");

end Test_Base64;
