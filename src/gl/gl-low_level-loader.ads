--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with System;

package GL.Low_Level.Loader is
   -- this package loads raw OpenGL functions at runtime
   -- (meaning it requests the function pointer of the requested function
   --  at runtime). when a function is not available, it raises a
   -- Feature_Not_Supported_Exception
   
   Feature_Not_Supported_Exception : exception;
   
   function Available (GL_Function_Name : String) return Boolean;
   
   generic
      GL_Function_Name : String;
      type Param1_Type is private;
      type Return_Type is private;
   function Function_With_1_Param (Param1 : Param1_Type) return Return_Type;
   
   generic
      GL_Function_Name : String;
      type Param1_Type is private;
      type Param2_Type is private;
      type Return_Type is private;
   function Function_With_2_Params (Param1 : Param1_Type;
                                    Param2 : Param2_Type)
                                   return Return_Type;
   
   generic
      GL_Procedure_Name : String;
      type Param1_Type is private;
   procedure Procedure_With_1_Param (Param1 : Param1_Type);
   
   generic
      GL_Procedure_Name : String;
      type Param1_Type is private;
      type Param2_Type is private;
   procedure Procedure_With_2_Params (Param1 : Param1_Type;
                                      Param2 : Param2_Type);

private
   generic
      type Function_Reference is private;
   function Load (GL_Function_Name : String) return Function_Reference;
   pragma Inline (Load);
   
   package Function_Maps is new Ada.Containers.Indefinite_Hashed_Maps (
     Key_Type        => String,
     Element_Type    => System.Address,
     Hash            => Ada.Strings.Hash,
     Equivalent_Keys => Standard."=",
     "="             => System."="
   );
   
   Loaded : Function_Maps.Map;
end GL.Low_Level.Loader;