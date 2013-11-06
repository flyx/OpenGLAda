--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Interfaces.C.Strings;

with GL.API;
with GL.Enums.Getter;
with GL.Errors;

package body GL.Context is

   function Major_Version return Int is
      Result : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Major_Version, Result'Access);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Major_Version;

   function Minor_Version return Int is
      Result : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Minor_Version, Result'Access);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Minor_Version;

   function Version_String return String is
   begin
      return C.Strings.Value (API.Get_String (Enums.Getter.Version));
   end Version_String;

   function Vendor return String is
   begin
      return C.Strings.Value (API.Get_String (Enums.Getter.Vendor));
   end Vendor;

   function Renderer return String is
   begin
      return C.Strings.Value (API.Get_String (Enums.Getter.Renderer));
   end Renderer;

   function Extensions return String_List is
      use Ada.Strings.Unbounded;
      use type Errors.Error_Code;
      Count : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Num_Extensions, Count'Access);
      if API.Get_Error = Errors.No_Error then
         -- we are on OpenGL 3
         return List : String_List (1 .. Positive (Count)) do
            for I in List'Range loop
               List (I) := To_Unbounded_String
                 (C.Strings.Value (API.Get_String_I
                                   (Enums.Getter.Extensions, UInt (I - 1))));
            end loop;
         end return;
      else
         -- OpenGL 2 fallback
         declare
            Raw : constant String := C.Strings.Value
              (API.Get_String (Enums.Getter.Extensions));
            Cur_Pos : Positive := Raw'First;
            Next_Space : Natural;
         begin
            if Raw'Length = 0 then
               return Null_String_List;
            end if;

            return List : String_List
              (1 .. Ada.Strings.Fixed.Count (Raw, " ") + 1) do
               for I in List'Range loop
                  Next_Space := Ada.Strings.Fixed.Index (Raw, " ", Cur_Pos);
                  if Next_Space = 0 then
                     -- this is the last token, there is no space behind it.
                     Next_Space := Raw'Last + 1;
                  end if;
                  List (I) := To_Unbounded_String
                    (Raw (Cur_Pos .. Next_Space - 1));
                  Cur_Pos := Next_Space + 1;
               end loop;
            end return;
         end;
      end if;
   end Extensions;


   function Has_Extension (Name : String) return Boolean is
      use type Errors.Error_Code;
      Count : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Num_Extensions, Count'Access);
      if API.Get_Error = Errors.No_Error then
         -- we are on OpenGL 3
         for i in 1 .. Count loop
            declare
               Extension : constant String := C.Strings.Value
                 (API.Get_String_I (Enums.Getter.Extensions, UInt (I - 1)));
            begin
               if Extension = Name then
                  return True;
               end if;
            end;
         end loop;
      else
         -- OpenGL 2 fallback
         declare
            Raw : constant String := C.Strings.Value
              (API.Get_String (Enums.Getter.Extensions));
            Cur_Pos : Positive := Raw'First;
            Next_Space : Natural;
         begin
            if Raw'Length = 0 then
               return False;
            end if;
            for I in 1 .. Ada.Strings.Fixed.Count (Raw, " ") + 1 loop
               Next_Space := Ada.Strings.Fixed.Index (Raw, " ", Cur_Pos);
               if Next_Space = 0 then
                  Next_Space := Raw'Last + 1;
               end if;
               if Raw (Cur_Pos .. Next_Space - 1) = Name then
                  return True;
               end if;
               Cur_Pos := Next_Space + 1;
            end loop;
         end;
      end if;
      return False;
   end Has_Extension;

   function Primary_Shading_Language_Version return String is
      Result : constant String := C.Strings.Value
        (API.Get_String (Enums.Getter.Shading_Language_Version));
   begin
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Primary_Shading_Language_Version;

   function Supported_Shading_Language_Versions return String_List is
      use Ada.Strings.Unbounded;
      use type Errors.Error_Code;
      Count : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Num_Shading_Language_Versions, Count'Access);
      if API.Get_Error = Errors.Invalid_Enum then
         raise Feature_Not_Supported_Exception;
      end if;
      return List : String_List (1 .. Positive (Count)) do
         for I in List'Range loop
            List (I) := To_Unbounded_String
              (C.Strings.Value (API.Get_String_I (
               Enums.Getter.Shading_Language_Version, UInt (I))));
         end loop;
      end return;
   end Supported_Shading_Language_Versions;

   function Supports_Shading_Language_Version (Name : String) return Boolean is
      Count : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Num_Shading_Language_Versions, Count'Access);
      Raise_Exception_On_OpenGL_Error;
      for I in 1 .. Count loop
         if C.Strings.Value
           (API.Get_String_I (Enums.Getter.Shading_Language_Version,
                              UInt (I))) = Name then
            return True;
         end if;
      end loop;
      return False;
   end Supports_Shading_Language_Version;

end GL.Context;
